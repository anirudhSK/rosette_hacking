#lang rosette

;; ============================================================
;; Yosys Cell Semantics
;;
;; This module provides an interpreter for individual Yosys
;; RTLIL cells using Rosette bitvectors. Each cell type maps
;; to well-defined bitvector operations, making the interpreter
;; fully symbolic (SMT-amenable).
;;
;; Key invariant: every cell input/output is a Rosette bitvector
;; of exactly the width declared in the cell's parameters.
;;
;; Reference: Yosys manual, Appendix A (Cell Reference)
;; ============================================================

(require "yosys-rtlil.rkt")
(provide eval-cell
         eval-simple-gate
         extend-to
         reduce-bits
         resolve-signal
         bv1 bv0)

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------

;; Single-bit constants
(define bv1 (bv 1 1))
(define bv0 (bv 0 1))

;; bool->bv: convert a Rosette boolean to a 1-bit bitvector
(define (bool->bv b) (if b bv1 bv0))

;; Extend bitvector v to width w, using sign-extension if signed?.
(define (extend-to v w signed?)
  (let ([cur (bitvector-size (type-of v))])
    (cond
      [(= cur w) v]
      [(< cur w)
       (if signed?
           (sign-extend v (bitvector w))
           (zero-extend v (bitvector w)))]
      [else
       ;; Truncate — take the low w bits
       (extract (- w 1) 0 v)])))

;; Reduce all bits of a bitvector with a binary op (fold over bit positions).
(define (reduce-bits v binop identity)
  (let ([w (bitvector-size (type-of v))])
    (let loop ([i 1] [acc (extract 0 0 v)])
      (if (>= i w)
          acc
          (loop (+ i 1) (binop acc (extract i i v)))))))

;; ------------------------------------------------------------
;; Reduction cells
;; ------------------------------------------------------------

(define (eval-reduce-and a)
  ;; AND of all bits: 1 iff every bit is 1 (i.e., a == all-ones)
  (let ([w (bitvector-size (type-of a))])
    (bool->bv (bveq a (bvnot (bv 0 w))))))

(define (eval-reduce-or a)
  ;; OR of all bits: 1 iff any bit is 1
  (bool->bv (not (bveq a (bv 0 (bitvector-size (type-of a)))))))

(define (eval-reduce-xor a)
  ;; XOR of all bits: parity
  (reduce-bits a bvxor bv0))

(define (eval-reduce-xnor a)
  ;; XNOR = NOT XOR
  (bvnot (eval-reduce-xor a)))

(define (eval-reduce-bool a)
  ;; Same as reduce_or: 1 iff non-zero
  (eval-reduce-or a))

;; ------------------------------------------------------------
;; Arithmetic helpers
;; ------------------------------------------------------------

;; Yosys arithmetic: extend both operands to Y_WIDTH, then operate.
;; For division by zero, Yosys returns 0 — we model the same.
(define (safe-div a b signed?)
  (let ([zero (bv 0 (bitvector-size (type-of b)))])
    (if (bveq b zero)
        (bv 0 (bitvector-size (type-of a)))
        (if signed? (bvsdiv a b) (bvudiv a b)))))

(define (safe-mod a b signed?)
  (let ([zero (bv 0 (bitvector-size (type-of b)))])
    (if (bveq b zero)
        (bv 0 (bitvector-size (type-of a)))
        (if signed? (bvsrem a b) (bvurem a b)))))

;; ------------------------------------------------------------
;; Main cell evaluator
;; ------------------------------------------------------------
;; eval-cell : cell hash[symbol->bv] -> hash[symbol->bv]
;;
;; Given a cell and a map from wire names to current bitvector
;; values, compute the cell's output ports as a hash.
;;
;; The caller is responsible for reading inputs from the environment
;; and writing outputs back; this function handles pure combinational
;; semantics. Sequential cells ($dff etc.) are handled separately
;; in the interpreter.

(define (eval-cell c env)
  (define (get-input port-name)
    ;; Look up what signal drives this cell input
    (let ([sig (hash-ref (cell-inputs c) port-name
                         (lambda () (error 'eval-cell
                                           "cell ~a missing input ~a"
                                           (cell-name c) port-name)))])
      (resolve-signal sig env)))

  (define (param key default) (cell-param c key default))
  (define (param! key) (cell-param! c key))

  (case (cell-type c)

    ;; ----------------------------------------------------------
    ;; Logic: $not, $and, $or, $xor, $xnor
    ;; Inputs sign/zero-extended to Y_WIDTH; result is Y_WIDTH.
    ;; ----------------------------------------------------------
    [($not)
     (let* ([yw (param! 'Y_WIDTH)]
            [as (param 'A_SIGNED #f)]
            [a  (extend-to (get-input 'A) yw as)])
       (hash 'Y (bvnot a)))]

    [($and)
     (let* ([yw (param! 'Y_WIDTH)]
            [as (param 'A_SIGNED #f)]
            [bs (param 'B_SIGNED #f)]
            ;; Yosys uses the *more* permissive signedness for extends
            [s  (and as bs)]
            [a  (extend-to (get-input 'A) yw s)]
            [b  (extend-to (get-input 'B) yw s)])
       (hash 'Y (bvand a b)))]

    [($or)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (and (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [a  (extend-to (get-input 'A) yw s)]
            [b  (extend-to (get-input 'B) yw s)])
       (hash 'Y (bvor a b)))]

    [($xor)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (and (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [a  (extend-to (get-input 'A) yw s)]
            [b  (extend-to (get-input 'B) yw s)])
       (hash 'Y (bvxor a b)))]

    [($xnor)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (and (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [a  (extend-to (get-input 'A) yw s)]
            [b  (extend-to (get-input 'B) yw s)])
       (hash 'Y (bvnot (bvxor a b))))]

    ;; ----------------------------------------------------------
    ;; Reduction
    ;; ----------------------------------------------------------
    [($reduce_and)
     (let* ([yw (param! 'Y_WIDTH)]
            [a  (get-input 'A)]
            [r  (eval-reduce-and a)])
       (hash 'Y (zero-extend r (bitvector yw))))]

    [($reduce_or)
     (let* ([yw (param! 'Y_WIDTH)]
            [a  (get-input 'A)]
            [r  (eval-reduce-or a)])
       (hash 'Y (zero-extend r (bitvector yw))))]

    [($reduce_xor)
     (let* ([yw (param! 'Y_WIDTH)]
            [a  (get-input 'A)]
            [r  (eval-reduce-xor a)])
       (hash 'Y (zero-extend r (bitvector yw))))]

    [($reduce_xnor)
     (let* ([yw (param! 'Y_WIDTH)]
            [a  (get-input 'A)]
            [r  (eval-reduce-xnor a)])
       (hash 'Y (zero-extend r (bitvector yw))))]

    [($reduce_bool)
     (let* ([yw (param! 'Y_WIDTH)]
            [a  (get-input 'A)]
            [r  (eval-reduce-bool a)])
       (hash 'Y (zero-extend r (bitvector yw))))]

    ;; ----------------------------------------------------------
    ;; Arithmetic: $pos, $neg, $add, $sub, $mul, $div, $mod
    ;; ----------------------------------------------------------
    [($pos)
     (let* ([yw (param! 'Y_WIDTH)]
            [as (param 'A_SIGNED #f)]
            [a  (extend-to (get-input 'A) yw as)])
       (hash 'Y a))]

    [($neg)
     (let* ([yw (param! 'Y_WIDTH)]
            [as (param 'A_SIGNED #f)]
            [a  (extend-to (get-input 'A) yw as)])
       (hash 'Y (bvneg a)))]

    [($add)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (or (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [a  (extend-to (get-input 'A) yw s)]
            [b  (extend-to (get-input 'B) yw s)])
       (hash 'Y (bvadd a b)))]

    [($sub)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (or (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [a  (extend-to (get-input 'A) yw s)]
            [b  (extend-to (get-input 'B) yw s)])
       (hash 'Y (bvsub a b)))]

    [($mul)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (or (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [a  (extend-to (get-input 'A) yw s)]
            [b  (extend-to (get-input 'B) yw s)])
       (hash 'Y (bvmul a b)))]

    [($div)
     (let* ([yw (param! 'Y_WIDTH)]
            [as (param 'A_SIGNED #f)]
            [bs (param 'B_SIGNED #f)]
            [s  (and as bs)]
            [a  (extend-to (get-input 'A) yw s)]
            [b  (extend-to (get-input 'B) yw s)])
       (hash 'Y (safe-div a b s)))]

    [($mod)
     (let* ([yw (param! 'Y_WIDTH)]
            [as (param 'A_SIGNED #f)]
            [bs (param 'B_SIGNED #f)]
            [s  (and as bs)]
            [a  (extend-to (get-input 'A) yw s)]
            [b  (extend-to (get-input 'B) yw s)])
       (hash 'Y (safe-mod a b s)))]

    ;; ----------------------------------------------------------
    ;; Comparison: result is 1 bit, zero-extended to Y_WIDTH
    ;; ----------------------------------------------------------
    [($eq $eqx)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (and (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [mw (max (param 'A_WIDTH 1) (param 'B_WIDTH 1))]
            [a  (extend-to (get-input 'A) mw s)]
            [b  (extend-to (get-input 'B) mw s)]
            [r  (bool->bv (bveq a b))])
       (hash 'Y (zero-extend r (bitvector yw))))]

    [($ne $nex)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (and (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [mw (max (param 'A_WIDTH 1) (param 'B_WIDTH 1))]
            [a  (extend-to (get-input 'A) mw s)]
            [b  (extend-to (get-input 'B) mw s)]
            [r  (bool->bv (not (bveq a b)))])
       (hash 'Y (zero-extend r (bitvector yw))))]

    [($lt)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (and (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [mw (max (param 'A_WIDTH 1) (param 'B_WIDTH 1))]
            [a  (extend-to (get-input 'A) mw s)]
            [b  (extend-to (get-input 'B) mw s)]
            [r  (bool->bv (if s (bvslt a b) (bvult a b)))])
       (hash 'Y (zero-extend r (bitvector yw))))]

    [($le)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (and (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [mw (max (param 'A_WIDTH 1) (param 'B_WIDTH 1))]
            [a  (extend-to (get-input 'A) mw s)]
            [b  (extend-to (get-input 'B) mw s)]
            [r  (bool->bv (if s (bvsle a b) (bvule a b)))])
       (hash 'Y (zero-extend r (bitvector yw))))]

    [($gt)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (and (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [mw (max (param 'A_WIDTH 1) (param 'B_WIDTH 1))]
            [a  (extend-to (get-input 'A) mw s)]
            [b  (extend-to (get-input 'B) mw s)]
            [r  (bool->bv (if s (bvsgt a b) (bvugt a b)))])
       (hash 'Y (zero-extend r (bitvector yw))))]

    [($ge)
     (let* ([yw (param! 'Y_WIDTH)]
            [s  (and (param 'A_SIGNED #f) (param 'B_SIGNED #f))]
            [mw (max (param 'A_WIDTH 1) (param 'B_WIDTH 1))]
            [a  (extend-to (get-input 'A) mw s)]
            [b  (extend-to (get-input 'B) mw s)]
            [r  (bool->bv (if s (bvsge a b) (bvuge a b)))])
       (hash 'Y (zero-extend r (bitvector yw))))]

    ;; ----------------------------------------------------------
    ;; Shift: $shl, $shr, $sshl, $sshr, $shift
    ;;
    ;; $shl/$sshl  : logical left shift (same)
    ;; $shr        : logical right shift
    ;; $sshr       : arithmetic right shift (if A_SIGNED)
    ;; $shift      : shift left if B positive, right if negative
    ;; ----------------------------------------------------------
    [($shl $sshl)
     (let* ([yw   (param! 'Y_WIDTH)]
            [as   (param 'A_SIGNED #f)]
            [a    (extend-to (get-input 'A) yw as)]
            [b    (get-input 'B)]
            ;; Shift amount: zero-extend to yw bits for bvshl
            [bext (zero-extend b (bitvector yw))])
       (hash 'Y (bvshl a bext)))]

    [($shr)
     (let* ([yw   (param! 'Y_WIDTH)]
            [as   (param 'A_SIGNED #f)]
            [a    (extend-to (get-input 'A) yw as)]
            [b    (get-input 'B)]
            [bext (zero-extend b (bitvector yw))])
       (hash 'Y (bvlshr a bext)))]

    [($sshr)
     (let* ([yw   (param! 'Y_WIDTH)]
            [as   (param 'A_SIGNED #f)]
            [a    (extend-to (get-input 'A) yw as)]
            [b    (get-input 'B)]
            [bext (zero-extend b (bitvector yw))])
       (hash 'Y (if as (bvashr a bext) (bvlshr a bext))))]

    [($shift)
     ;; Signed B: positive = left shift, negative = right shift.
     ;; A is sign/zero extended to Y_WIDTH.
     (let* ([yw  (param! 'Y_WIDTH)]
            [as  (param 'A_SIGNED #f)]
            [bs  (param 'B_SIGNED #f)]
            [a   (extend-to (get-input 'A) yw as)]
            [b   (extend-to (get-input 'B) yw bs)])
       (hash 'Y (if (and bs (bvslt b (bv 0 yw)))
                    (bvlshr a (bvneg b))
                    (bvshl  a b))))]

    ;; ----------------------------------------------------------
    ;; Mux: $mux, $pmux
    ;; ----------------------------------------------------------
    [($mux)
     (let* ([w  (param! 'WIDTH)]
            [a  (get-input 'A)]   ; selected when S=0
            [b  (get-input 'B)]   ; selected when S=1
            [s  (get-input 'S)])  ; 1-bit select
       ;; Rosette if-then-else on bitvectors via bvite (if S != 0)
       (hash 'Y (if (bveq s bv1) b a)))]

    [($pmux)
     ;; Priority mux: S is N bits wide, B is N*WIDTH bits.
     ;; Lowest set bit in S selects corresponding WIDTH-bit slice of B.
     ;; If no bit set, A is selected.
     (let* ([w  (param! 'WIDTH)]
            [n  (param! 'S_WIDTH)]
            [a  (get-input 'A)]
            [b  (get-input 'B)]
            [s  (get-input 'S)])
       ;; Build result using fold: later (higher-priority) bits win.
       ;; In Yosys $pmux, bit 0 is highest priority (first matching).
       (define result
         (let loop ([i 0] [out a])
           (if (>= i n)
               out
               ;; Extract bit i of S
               (let* ([si  (extract i i s)]
                      ;; Extract bits [i*w+w-1 : i*w] from B
                      [bi  (extract (+ (* i w) w -1) (* i w) b)])
                 ;; Only take bi if si=1 AND no lower bit was set
                 ;; (priority: bit 0 first). We use Rosette's symbolic if.
                 (loop (+ i 1) (if (bveq si bv1) bi out))))))
       (hash 'Y result))]

    ;; ----------------------------------------------------------
    ;; Concatenation / Slicing
    ;; ----------------------------------------------------------
    [($concat)
     ;; Y = {B, A}  (B is the more significant part)
     (let* ([a (get-input 'A)]
            [b (get-input 'B)])
       (hash 'Y (concat b a)))]

    [($slice)
     (let* ([offset (param! 'OFFSET)]
            [yw     (param! 'Y_WIDTH)]
            [a      (get-input 'A)])
       (hash 'Y (extract (+ offset yw -1) offset a)))]

    ;; ----------------------------------------------------------
    ;; Technology-mapped simple gates (single-bit)
    ;; These appear after technology mapping (e.g., synth -top).
    ;; ----------------------------------------------------------
    [else
     (eval-simple-gate (cell-type c) c get-input)]))

;; ------------------------------------------------------------
;; Simple gate evaluator (technology primitives)
;; ------------------------------------------------------------
(define (eval-simple-gate type c get-input)
  (case type
    [($_BUF_)  (hash 'Y (get-input 'A))]
    [($_NOT_)  (hash 'Y (bvnot (get-input 'A)))]
    [($_AND_)  (hash 'Y (bvand (get-input 'A) (get-input 'B)))]
    [($_NAND_) (hash 'Y (bvnot (bvand (get-input 'A) (get-input 'B))))]
    [($_OR_)   (hash 'Y (bvor  (get-input 'A) (get-input 'B)))]
    [($_NOR_)  (hash 'Y (bvnot (bvor  (get-input 'A) (get-input 'B))))]
    [($_XOR_)  (hash 'Y (bvxor (get-input 'A) (get-input 'B)))]
    [($_XNOR_) (hash 'Y (bvnot (bvxor (get-input 'A) (get-input 'B))))]
    [($_MUX_)
     (let ([s (get-input 'S)])
       (hash 'Y (if (bveq s bv1) (get-input 'B) (get-input 'A))))]
    [($_NMUX_)
     (let ([s (get-input 'S)])
       (hash 'Y (if (bveq s bv1) (get-input 'A) (get-input 'B))))]
    ;; 3-input AOI: Y = !((A & B) | C)
    [($_AOI3_)
     (hash 'Y (bvnot (bvor (bvand (get-input 'A) (get-input 'B))
                            (get-input 'C))))]
    ;; 3-input OAI: Y = !((A | B) & C)
    [($_OAI3_)
     (hash 'Y (bvnot (bvand (bvor (get-input 'A) (get-input 'B))
                             (get-input 'C))))]
    ;; 4-input AOI: Y = !((A & B) | (C & D))
    [($_AOI4_)
     (hash 'Y (bvnot (bvor (bvand (get-input 'A) (get-input 'B))
                            (bvand (get-input 'C) (get-input 'D)))))]
    ;; 4-input OAI: Y = !((A | B) & (C | D))
    [($_OAI4_)
     (hash 'Y (bvnot (bvand (bvor (get-input 'A) (get-input 'B))
                             (bvor (get-input 'C) (get-input 'D)))))]
    [else
     (error 'eval-cell "unknown cell type: ~a (cell ~a)"
            type (cell-name c))]))

;; ------------------------------------------------------------
;; Signal resolution
;; ------------------------------------------------------------
;; resolve-signal : signal-ref hash[symbol->bv] -> bv
;;
;; A signal-ref is either:
;;   - a symbol (wire name): look it up in env
;;   - a const struct: convert to bv
;;   - a bv directly (already resolved)

(define (resolve-signal sig env)
  (cond
    [(symbol? sig)  (hash-ref env sig
                               (lambda ()
                                 (error 'resolve-signal
                                        "wire ~a not found in environment" sig)))]
    [(const? sig)   (const->bv sig)]
    [(bitvector? (type-of sig)) sig]  ; already a bv
    [else (error 'resolve-signal "unknown signal type: ~a" sig)]))
