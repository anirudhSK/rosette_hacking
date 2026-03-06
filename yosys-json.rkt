#lang rosette

;; ============================================================
;; yosys-json.rkt — Bit-level evaluator for Yosys JSON output
;;
;; Yosys represents netlists at the bit level: each wire bit has a
;; unique integer ID, and cell connections are lists of bit IDs (or
;; the strings "0"/"1" for constants). This representation is simpler
;; than word-level RTLIL when cells like $shiftx scatter their output
;; bits across multiple named wires.
;;
;; The evaluator works directly with bit IDs:
;;
;;   bit-env : hash[bit-id → (bitvector 1)]
;;
;; Input/output ports and cell parameters are assembled from/into
;; bitvectors using the port's bit-ID lists.
;;
;; Usage:
;;   (define json-mod (load-json-module "tiny_cpu.json"))
;;   (define state0   (json-initial-state json-mod))
;;   (define out      (json-cpu-run json-mod state0 inputs n))
;; ============================================================

(require json
         "yosys-cells.rkt")   ; for extend-to, bv0, bv1

(provide load-json-module
         json-initial-state
         make-json-state
         json-step-module
         json-cpu-run)

;; ── Helpers ───────────────────────────────────────────────────────────────

(define (bool->bv b) (if b bv1 bv0))

;; Parse a Yosys binary parameter string (MSB-first) to a Racket integer.
(define (parse-param-str s)
  (for/fold ([acc 0])
            ([c (in-list (reverse (string->list s)))]
             [i (in-naturals)])
    (+ acc (if (char=? c #\1) (expt 2 i) 0))))

(define (cell-param-int cell key [default 0])
  (define params (hash-ref cell 'parameters))
  (if (hash-has-key? params key)
      (parse-param-str (hash-ref params key))
      default))

(define (cell-param-bool cell key [default #f])
  (not (zero? (cell-param-int cell key (if default 1 0)))))

;; ── Bit-level read / write ────────────────────────────────────────────────
;;
;; A Yosys connection is a list where each element is either:
;;   - a Racket integer  → a wire bit ID, looked up in bit-env
;;   - the string "0"    → constant 0 bit
;;   - the string "1"    → constant 1 bit
;;
;; Bits are ordered LSB-first in all Yosys connection lists.

(define (read-bits env bits)
  (define bvals
    (for/list ([b bits])
      (cond [(equal? b "0") bv0]
            [(equal? b "1") bv1]
            [else (hash-ref env b bv0)])))   ; default 0 if not yet driven
  (if (null? bvals)
      bv0
      ;; Fold: concat makes first arg the new MSB, so LSB-first list → correct bv
      (for/fold ([acc (car bvals)])
                ([b   (cdr bvals)])
        (concat b acc))))

(define (write-bits env bits bv-val)
  (for/fold ([acc env])
            ([b bits] [i (in-naturals)]
             #:when (integer? b))            ; skip constant-bit positions
    (hash-set acc b (extract i i bv-val))))

;; ── Sequential / combinational classification ─────────────────────────────

(define sequential-types
  '("$dff" "$dffe" "$adff" "$adffe" "$sdff" "$sdffe"
    "$dlatch" "$_DFF_P_" "$_DFF_N_"))

(define (sequential-cell? cell)
  (member (hash-ref cell 'type) sequential-types))

;; ── Bit-ID dependency sets (for topological sort) ─────────────────────────

(define (port-bit-ids cell direction)
  (define dirs  (hash-ref cell 'port_directions))
  (define conns (hash-ref cell 'connections))
  (for/fold ([result '()])
            ([(port dir) (in-hash dirs)]
             #:when (string=? dir direction))
    (append result (filter integer? (hash-ref conns port '())))))

(define (cell-input-bits  c) (port-bit-ids c "input"))
(define (cell-output-bits c) (port-bit-ids c "output"))

;; ── Topological sort ──────────────────────────────────────────────────────

(define (topo-sort-cells cells initial-bits)
  (let loop ([remaining cells]
             [available (list->set initial-bits)]
             [sorted    '()])
    (if (null? remaining)
        (reverse sorted)
        (let ([ready (filter (lambda (c)
                               (for/and ([b (cell-input-bits c)])
                                 (set-member? available b)))
                             remaining)])
          (when (null? ready)
            (error 'topo-sort-cells "combinational loop detected"))
          (let ([c (car ready)])
            (loop (remove c remaining)
                  (set-union available (list->set (cell-output-bits c)))
                  (cons c sorted)))))))

;; ── Combinational cell evaluator ──────────────────────────────────────────

(define (eval-comb-cell cell env)
  (define type  (hash-ref cell 'type))
  (define conns (hash-ref cell 'connections))

  (define (get port) (read-bits env (hash-ref conns port '())))
  (define (yw)  (cell-param-int  cell 'Y_WIDTH 1))
  (define (mw)  (max (cell-param-int cell 'A_WIDTH 1)
                     (cell-param-int cell 'B_WIDTH 1)))
  (define (s)   (and (cell-param-bool cell 'A_SIGNED)
                     (cell-param-bool cell 'B_SIGNED)))

  (define results
    (case type
      [("$add")
       (hash 'Y (bvadd (extend-to (get 'A) (yw) (s))
                       (extend-to (get 'B) (yw) (s))))]
      [("$sub")
       (hash 'Y (bvsub (extend-to (get 'A) (yw) (s))
                       (extend-to (get 'B) (yw) (s))))]
      [("$and")
       (hash 'Y (bvand (extend-to (get 'A) (yw) (s))
                       (extend-to (get 'B) (yw) (s))))]
      [("$or")
       (hash 'Y (bvor  (extend-to (get 'A) (yw) (s))
                       (extend-to (get 'B) (yw) (s))))]
      [("$mux")
       (hash 'Y (if (bveq (get 'S) bv1) (get 'B) (get 'A)))]
      [("$eq")
       (let* ([a (extend-to (get 'A) (mw) (s))]
              [b (extend-to (get 'B) (mw) (s))])
         (hash 'Y (zero-extend (bool->bv (bveq a b)) (bitvector (yw)))))]
      [("$reduce_and")
       (let* ([a (get 'A)]
              [w (bitvector-size (type-of a))]
              [r (bool->bv (bveq a (bvnot (bv 0 w))))])
         (hash 'Y (zero-extend r (bitvector (yw)))))]
      [("$logic_not")
       ;; Y = (A == 0) ? 1 : 0, zero-extended to Y_WIDTH
       (let* ([a (get 'A)]
              [w (bitvector-size (type-of a))]
              [r (bool->bv (bveq a (bv 0 w)))])
         (hash 'Y (zero-extend r (bitvector (yw)))))]
      [("$logic_or")
       ;; Y = (A != 0 || B != 0), zero-extended to Y_WIDTH
       (let* ([a (get 'A)]
              [b (get 'B)]
              [wa (bitvector-size (type-of a))]
              [wb (bitvector-size (type-of b))]
              [r (bool->bv (or (not (bveq a (bv 0 wa)))
                               (not (bveq b (bv 0 wb)))))])
         (hash 'Y (zero-extend r (bitvector (yw)))))]
      [("$shiftx")
       ;; Variable bit-select: Y = A[B +: Y_WIDTH]
       ;; Used for imem[16*pc +: 16]. Concrete-only via Racket integers.
       (let* ([yw-n (yw)]
              [a-nat (bitvector->natural (get 'A))]
              [b-nat (bitvector->natural (get 'B))]
              [result-nat (bitwise-and (arithmetic-shift a-nat (- b-nat))
                                       (sub1 (expt 2 yw-n)))])
         (hash 'Y (bv result-nat yw-n)))]
      [else
       (error 'eval-comb-cell "unsupported cell type: ~a" type)]))

  ;; Write result bits back to the environment
  (for/fold ([acc env])
            ([(port bv-val) (in-hash results)])
    (write-bits acc (hash-ref conns port '()) bv-val)))

;; ── Combinational evaluation pass ────────────────────────────────────────

(define (eval-combinational json-mod env)
  (define all-cells  (hash-values (hash-ref json-mod 'cells)))
  (define comb-cells (filter (lambda (c) (not (sequential-cell? c))) all-cells))
  (define ordered    (topo-sort-cells comb-cells (hash-keys env)))
  (foldl eval-comb-cell env ordered))

;; ── Sequential evaluation pass ────────────────────────────────────────────

(define (eval-sequential json-mod env)
  (define all-cells (hash-values (hash-ref json-mod 'cells)))
  (for/fold ([next (hash)])
            ([cell all-cells]
             #:when (sequential-cell? cell))
    (define conns (hash-ref cell 'connections))
    (define type  (hash-ref cell 'type))
    (define q-bits (hash-ref conns 'Q))
    (define width  (length (filter integer? q-bits)))

    (define next-q
      (case type
        [("$dff" "$_DFF_P_" "$_DFF_N_")
         (read-bits env (hash-ref conns 'D))]
        [("$dffe")
         (let ([d (read-bits env (hash-ref conns 'D))]
               [e (read-bits env (hash-ref conns 'EN))]
               [q (read-bits env q-bits)])
           (if (bveq e bv1) d q))]
        [("$sdff")
         (let* ([d    (read-bits env (hash-ref conns 'D))]
                [srst (read-bits env (hash-ref conns 'SRST))]
                [rv   (bv (cell-param-int cell 'SRST_VALUE 0) width)]
                [pol  (cell-param-bool cell 'SRST_POLARITY #t)])
           (if (bveq srst (if pol bv1 bv0)) rv d))]
        [("$sdffe")
         (let* ([d    (read-bits env (hash-ref conns 'D))]
                [en   (read-bits env (hash-ref conns 'EN))]
                [srst (read-bits env (hash-ref conns 'SRST))]
                [rv   (bv (cell-param-int cell 'SRST_VALUE 0) width)]
                [spol (cell-param-bool cell 'SRST_POLARITY #t)]
                [epol (cell-param-bool cell 'EN_POLARITY   #t)]
                [q    (read-bits env q-bits)])
           (cond [(bveq srst (if spol bv1 bv0)) rv]
                 [(bveq en   (if epol bv1 bv0)) d]
                 [else                           q]))]
        [else
         (error 'eval-sequential "unsupported: ~a" type)]))

    (write-bits next q-bits next-q)))

;; ── Initial and user-specified state ─────────────────────────────────────

;; All sequential Q bits = 0.
(define (json-initial-state json-mod)
  (for/fold ([env (hash)])
            ([cell (hash-values (hash-ref json-mod 'cells))]
             #:when (sequential-cell? cell))
    (define q-bits (hash-ref (hash-ref cell 'connections) 'Q))
    (define width  (length (filter integer? q-bits)))
    (write-bits env q-bits (bv 0 width))))

;; Make initial state with dmem pre-loaded from a list of integers.
;; pc and registers start at 0; only dmem entries are set.
(define (make-json-state json-mod #:dmem [dmem (make-list 20 0)])
  (define env      (json-initial-state json-mod))
  (define netnames (hash-ref json-mod 'netnames))
  (define dm-names '#(dm0  dm1  dm2  dm3  dm4
                      dm5  dm6  dm7  dm8  dm9
                      dm10 dm11 dm12 dm13 dm14
                      dm15 dm16 dm17 dm18 dm19))
  (for/fold ([acc env])
            ([name (in-vector dm-names)] [val dmem])
    (define info (hash-ref netnames name #f))
    (if info
        (write-bits acc (hash-ref info 'bits) (bv val 8))
        acc)))

;; ── Seed env with input port values ──────────────────────────────────────

(define (apply-inputs json-mod env inputs-hash)
  (define ports (hash-ref json-mod 'ports))
  (for/fold ([acc env])
            ([(pname pinfo) (in-hash ports)]
             #:when (string=? (hash-ref pinfo 'direction) "input"))
    (define w   (length (hash-ref pinfo 'bits)))
    (define val (hash-ref inputs-hash pname (lambda () (bv 0 w))))
    (write-bits acc (hash-ref pinfo 'bits) val)))

;; ── Read output port values ───────────────────────────────────────────────

(define (read-outputs json-mod env)
  (define ports (hash-ref json-mod 'ports))
  (for/hash ([(pname pinfo) (in-hash ports)]
             #:when (string=? (hash-ref pinfo 'direction) "output"))
    (values pname (read-bits env (hash-ref pinfo 'bits)))))

;; ── Public API ────────────────────────────────────────────────────────────

;; Load the first module from a Yosys JSON file.
(define (load-json-module path)
  (define json (call-with-input-file path read-json))
  (first (hash-values (hash-ref json 'modules))))

;; One clock cycle.
;; state: bit-env for sequential Q bits
;; inputs-hash: symbol → bitvector  (e.g., 'imem → bv1024, 'rst → bv1)
;; Returns (values output-hash next-state).
(define (json-step-module json-mod state inputs-hash)
  ;; Pre-clock: state + inputs → combinational evaluation
  (define pre-env
    (eval-combinational json-mod (apply-inputs json-mod state inputs-hash)))

  ;; Clock edge: sample D inputs to get next state
  (define next-state (eval-sequential json-mod pre-env))

  ;; Post-clock: next-state + inputs → combinational re-settlement
  (define post-env
    (eval-combinational json-mod (apply-inputs json-mod next-state inputs-hash)))

  (values (read-outputs json-mod post-env) next-state))

;; Run n clock cycles. Returns final output hash.
(define (json-cpu-run json-mod init-state inputs-hash n)
  (let loop ([state init-state] [i 0] [last-out (hash)])
    (if (= i n)
        last-out
        (let-values ([(out next) (json-step-module json-mod state inputs-hash)])
          (loop next (+ i 1) out)))))
