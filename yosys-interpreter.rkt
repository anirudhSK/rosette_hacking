#lang rosette

;; ============================================================
;; Yosys RTLIL Module Interpreter
;;
;; Evaluates a complete RTLIL module using topological ordering
;; of combinational cells. Sequential elements (flip-flops)
;; separate combinational logic into clock cycles.
;;
;; Usage:
;;   (eval-module mod inputs)         -> outputs hash
;;   (step-module mod state inputs)   -> (values outputs new-state)
;;   (eval-module* mod inputs cycles) -> final outputs
;;
;; All values are Rosette bitvectors, enabling symbolic execution
;; for verification, synthesis, and equivalence checking.
;; ============================================================

(require "yosys-rtlil.rkt"
         "yosys-cells.rkt")

(provide eval-module
         step-module
         eval-module*
         initial-state
         module-input-names
         module-output-names
         module-state-wires)

;; ------------------------------------------------------------
;; Topological sort of combinational cells
;; ------------------------------------------------------------
;; We need to evaluate cells in dependency order. A cell can be
;; evaluated once all its input wires have values in the env.
;;
;; DFF cells are "state sources" — their Q outputs are taken from
;; the state hash and their D inputs are only needed to compute
;; the *next* state. We treat them specially.

(define (sequential-cell? c)
  (member (cell-type c)
          (list '$dff '$dffe '$adff '$adffe '$sdff '$sdffe
                '$dlatch '$_DFF_P_ '$_DFF_N_)))

(define (combinational-cell? c)
  (not (sequential-cell? c)))

;; Return the set of output wire names driven by cell c.
(define (cell-output-wires c)
  (for/list ([(_ sig) (in-hash (cell-outputs c))]
             #:when (symbol? sig))
    sig))

;; Return the set of input wire names read by cell c.
(define (cell-input-wires c)
  (for/list ([(_ sig) (in-hash (cell-inputs c))]
             #:when (symbol? sig))
    sig))

;; Topological sort: given combinational cells, return an ordered list.
;; Uses iterative "ready" selection (all inputs resolved in env/state).
(define (topo-sort cells driven-by-state)
  ;; driven-by-state: set of wire names available before combinational eval
  (let loop ([remaining cells]
             [available (list->set driven-by-state)]
             [sorted    '()])
    (if (null? remaining)
        (reverse sorted)
        ;; Find any cell whose inputs are all available
        (let ([ready (filter (lambda (c)
                               (for/and ([w (cell-input-wires c)])
                                 (set-member? available w)))
                             remaining)])
          (when (null? ready)
            ;; No progress — likely a combinational loop (not synthesizable)
            (error 'topo-sort
                   "combinational loop detected among cells: ~a"
                   (map cell-name remaining)))
          ;; Take first ready cell (could take all, order doesn't matter)
          (let ([c (car ready)])
            (loop (remove c remaining)
                  (set-union available (list->set (cell-output-wires c)))
                  (cons c sorted)))))))

;; ------------------------------------------------------------
;; Environment construction
;; ------------------------------------------------------------

;; All input port names of a module.
(define (module-input-names mod)
  (for/list ([p (rtlil-module-ports mod)]
             #:when (eq? (port-direction p) 'input))
    (wire-name (port-wire p))))

;; All output port names of a module.
(define (module-output-names mod)
  (for/list ([p (rtlil-module-ports mod)]
             #:when (eq? (port-direction p) 'output))
    (wire-name (port-wire p))))

;; Wire names driven by flip-flop Q outputs (state wires).
(define (module-state-wires mod)
  (for*/list ([c (rtlil-module-cells mod)]
              #:when (sequential-cell? c)
              [w (cell-output-wires c)])
    w))

;; ------------------------------------------------------------
;; Initial state: all flip-flop outputs start at 0.
;; ------------------------------------------------------------
(define (initial-state mod)
  (for/hash ([c (rtlil-module-cells mod)]
             #:when (sequential-cell? c))
    ;; Each sequential cell drives one Q output
    (let* ([q-name (hash-ref (cell-outputs c) 'Q)]
           [w      (hash-ref (rtlil-module-wires mod) q-name)]
           [width  (wire-width w)])
      (values q-name (bv 0 width)))))

;; ------------------------------------------------------------
;; Combinational evaluation
;; ------------------------------------------------------------
;; eval-combinational : module hash[sym->bv] -> hash[sym->bv]
;;
;; Evaluate all combinational cells in topological order.
;; Returns an updated environment with all wire values resolved.

(define (eval-combinational mod env)
  (let* ([comb-cells  (filter combinational-cell? (rtlil-module-cells mod))]
         [state-wires (module-state-wires mod)]
         [input-wires (module-input-names mod)]
         [driven      (append state-wires input-wires)]
         [ordered     (topo-sort comb-cells driven)])
    (foldl
     (lambda (c current-env)
       (let ([out-bvs (eval-cell c current-env)])
         (for/fold ([e current-env])
                   ([(port-name bv-val) (in-hash out-bvs)])
           (let ([wire-name (hash-ref (cell-outputs c) port-name #f)])
             (if (and wire-name (symbol? wire-name))
                 (hash-set e wire-name bv-val)
                 e)))))
     env
     ordered)))

;; ------------------------------------------------------------
;; Sequential evaluation (compute next state from D inputs)
;; ------------------------------------------------------------

;; Evaluate sequential cells to compute the next state.
;; Returns hash[wire-name -> bv] for all Q outputs' next values.
(define (eval-sequential mod env)
  (for/hash ([c (rtlil-module-cells mod)]
             #:when (sequential-cell? c))
    (let* ([q-name  (hash-ref (cell-outputs c) 'Q)]
           [type    (cell-type c)])
      (case type

        [($dff $_DFF_P_ $_DFF_N_)
         ;; Basic DFF: Q+ = D (clock edge is abstracted away)
         (let ([d (resolve-signal (hash-ref (cell-inputs c) 'D) env)])
           (values q-name d))]

        [($dffe)
         ;; DFF with clock enable: Q+ = EN ? D : Q
         (let* ([d  (resolve-signal (hash-ref (cell-inputs c) 'D) env)]
                [en (resolve-signal (hash-ref (cell-inputs c) 'EN) env)]
                [q  (hash-ref env q-name)])
           (values q-name (if (bveq en bv1) d q)))]

        [($adff)
         ;; Async reset DFF: ARST overrides everything
         (let* ([d      (resolve-signal (hash-ref (cell-inputs c) 'D) env)]
                [arst   (resolve-signal (hash-ref (cell-inputs c) 'ARST) env)]
                [arst-v (cell-param c 'ARST_VALUE 0)]
                [width  (bitvector-size (type-of d))]
                [rst-bv (bv arst-v width)]
                [arst-pol (cell-param c 'ARST_POLARITY 1)])
           ;; Reset is active when arst == arst-pol
           (let ([active (bveq arst (bv arst-pol 1))])
             (values q-name (if active rst-bv d))))]

        [($adffe)
         ;; Async reset + enable
         (let* ([d      (resolve-signal (hash-ref (cell-inputs c) 'D) env)]
                [en     (resolve-signal (hash-ref (cell-inputs c) 'EN) env)]
                [arst   (resolve-signal (hash-ref (cell-inputs c) 'ARST) env)]
                [arst-v (cell-param c 'ARST_VALUE 0)]
                [width  (bitvector-size (type-of d))]
                [rst-bv (bv arst-v width)]
                [arst-pol (cell-param c 'ARST_POLARITY 1)]
                [q      (hash-ref env q-name)]
                [arst-active (bveq arst (bv arst-pol 1))]
                [en-active   (bveq en bv1)])
           (values q-name
                   (cond
                     [arst-active rst-bv]
                     [en-active   d]
                     [else        q])))]

        [($sdff)
         ;; Sync reset: reset takes priority over D on clock edge
         (let* ([d      (resolve-signal (hash-ref (cell-inputs c) 'D) env)]
                [srst   (resolve-signal (hash-ref (cell-inputs c) 'SRST) env)]
                [srst-v (cell-param c 'SRST_VALUE 0)]
                [width  (bitvector-size (type-of d))]
                [rst-bv (bv srst-v width)]
                [srst-pol (cell-param c 'SRST_POLARITY 1)]
                [active (bveq srst (bv srst-pol 1))])
           (values q-name (if active rst-bv d)))]

        [($sdffe)
         ;; Sync reset + enable
         (let* ([d      (resolve-signal (hash-ref (cell-inputs c) 'D) env)]
                [en     (resolve-signal (hash-ref (cell-inputs c) 'EN) env)]
                [srst   (resolve-signal (hash-ref (cell-inputs c) 'SRST) env)]
                [srst-v (cell-param c 'SRST_VALUE 0)]
                [width  (bitvector-size (type-of d))]
                [rst-bv (bv srst-v width)]
                [srst-pol (cell-param c 'SRST_POLARITY 1)]
                [q      (hash-ref env q-name)]
                [rst-active (bveq srst (bv srst-pol 1))]
                [en-active  (bveq en bv1)])
           (values q-name
                   (cond
                     [rst-active rst-bv]
                     [en-active  d]
                     [else       q])))]

        [($dlatch)
         ;; Level-sensitive latch: Q = EN ? D : Q
         (let* ([d  (resolve-signal (hash-ref (cell-inputs c) 'D) env)]
                [en (resolve-signal (hash-ref (cell-inputs c) 'EN) env)]
                [q  (hash-ref env q-name)]
                [en-pol (cell-param c 'EN_POLARITY 1)]
                [active (bveq en (bv en-pol 1))])
           (values q-name (if active d q)))]

        [else
         (error 'eval-sequential "unhandled sequential cell type: ~a" type)]))))

;; ------------------------------------------------------------
;; Public API
;; ------------------------------------------------------------

;; eval-module : rtlil-module hash[sym->bv] -> hash[sym->bv]
;;
;; Evaluate a purely combinational module (no flip-flops).
;; Returns a hash of all wire values including outputs.
(define (eval-module mod inputs)
  (let* ([env (eval-combinational mod inputs)])
    ;; Extract output ports
    (for/hash ([name (module-output-names mod)])
      (values name (hash-ref env name
                              (lambda ()
                                (error 'eval-module
                                       "output ~a has no value" name)))))))

;; step-module : rtlil-module hash[sym->bv] hash[sym->bv]
;;              -> (values hash[sym->bv] hash[sym->bv])
;;
;; Execute one clock cycle:
;;   1. Merge state (Q outputs) into env
;;   2. Evaluate combinational logic (pre-clock)
;;   3. Sample D inputs to get next state (DFFs update on clock edge)
;;   4. Re-evaluate combinational logic with next state (post-clock)
;;   5. Return (post-clock outputs, next-state)
;;
;; Outputs reflect the post-edge state, matching RTL simulation convention:
;; after posedge clk, DFFs capture new values and combinational outputs settle.
(define (step-module mod state inputs)
  (let* ([pre-env    (eval-combinational mod (hash-union inputs state))]
         [next-state (eval-sequential mod pre-env)]
         [post-env   (eval-combinational mod (hash-union inputs next-state))]
         [outs       (for/hash ([name (module-output-names mod)])
                       (values name (hash-ref post-env name)))])
    (values outs next-state)))

;; eval-module* : rtlil-module hash[sym->bv] (listof hash[sym->bv])
;;              -> hash[sym->bv]
;;
;; Run multiple clock cycles given a list of input vectors.
;; Returns the final output values.
(define (eval-module* mod init-state input-seq)
  (let loop ([state  init-state]
             [inputs (if (list? input-seq) input-seq (list input-seq))]
             [last-out (hash)])
    (if (null? inputs)
        last-out
        (let-values ([(out next-state) (step-module mod state (car inputs))])
          (loop next-state (cdr inputs) out)))))

;; ------------------------------------------------------------
;; Helper: hash-union (prefer left on conflict)
;; ------------------------------------------------------------
(define (hash-union h1 h2)
  (for/fold ([acc h1])
            ([(k v) (in-hash h2)])
    (if (hash-has-key? acc k) acc (hash-set acc k v))))
