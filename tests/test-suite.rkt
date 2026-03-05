#lang rosette

;; ============================================================
;; Yosys RTLIL Rosette Interpreter — Test Suite
;; Run with: raco test tests/test-suite.rkt
;; ============================================================

(require rackunit
         rackunit/text-ui
         "../yosys-rtlil.rkt"
         "../yosys-cells.rkt"
         "../yosys-interpreter.rkt")

;; ------------------------------------------------------------
;; Shared circuit definitions
;; ------------------------------------------------------------

;; 8-bit AND
(define and8-mod
  (rtlil-module
   'and8
   (list (make-input 'A 8) (make-input 'B 8) (make-output 'Y 8))
   (make-immutable-hash
    (list (cons 'A (wire 'A 8)) (cons 'B (wire 'B 8)) (cons 'Y (wire 'Y 8))))
   (list (make-cell 'cell0 '$and
                    (list (cons 'A_WIDTH 8) (cons 'B_WIDTH 8) (cons 'Y_WIDTH 8)
                          (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
                    (list (cons 'A 'A) (cons 'B 'B))
                    (list (cons 'Y 'Y))))
   '()))

;; 4-bit adder (A + B, no carry-in for simplicity)
(define add4-mod
  (rtlil-module
   'add4
   (list (make-input 'A 4) (make-input 'B 4) (make-output 'SUM 4) (make-output 'COUT 1))
   (make-immutable-hash
    (list (cons 'A    (wire 'A 4))
          (cons 'B    (wire 'B 4))
          (cons 'SUM  (wire 'SUM 4))
          (cons 'COUT (wire 'COUT 1))
          (cons 'a5   (wire 'a5 5))
          (cons 'b5   (wire 'b5 5))
          (cons 'sum5 (wire 'sum5 5))))
   (list
    (make-cell 'a_ext '$pos
               (list (cons 'A_WIDTH 4) (cons 'Y_WIDTH 5) (cons 'A_SIGNED #f))
               (list (cons 'A 'A)) (list (cons 'Y 'a5)))
    (make-cell 'b_ext '$pos
               (list (cons 'A_WIDTH 4) (cons 'Y_WIDTH 5) (cons 'A_SIGNED #f))
               (list (cons 'A 'B)) (list (cons 'Y 'b5)))
    (make-cell 'add0 '$add
               (list (cons 'A_WIDTH 5) (cons 'B_WIDTH 5) (cons 'Y_WIDTH 5)
                     (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
               (list (cons 'A 'a5) (cons 'B 'b5)) (list (cons 'Y 'sum5)))
    (make-cell 'sum_slice '$slice
               (list (cons 'OFFSET 0) (cons 'Y_WIDTH 4) (cons 'A_WIDTH 5))
               (list (cons 'A 'sum5)) (list (cons 'Y 'SUM)))
    (make-cell 'cout_slice '$slice
               (list (cons 'OFFSET 4) (cons 'Y_WIDTH 1) (cons 'A_WIDTH 5))
               (list (cons 'A 'sum5)) (list (cons 'Y 'COUT))))
   '()))

;; 8-bit shift register
(define shift8-mod
  (rtlil-module
   'shift8
   (list (make-input 'D 1) (make-output 'Q 1))
   (make-immutable-hash
    (list (cons 'D      (wire 'D 1))
          (cons 'Q      (wire 'Q 1))
          (cons 'sr     (wire 'sr 8))
          (cons 'sr_d   (wire 'sr_d 8))
          (cons 'sr_low (wire 'sr_low 7))))
   (list
    (make-cell 'slice0 '$slice
               (list (cons 'OFFSET 0) (cons 'Y_WIDTH 7) (cons 'A_WIDTH 8))
               (list (cons 'A 'sr)) (list (cons 'Y 'sr_low)))
    (make-cell 'concat0 '$concat
               (list (cons 'A_WIDTH 1) (cons 'B_WIDTH 7))
               (list (cons 'A 'D) (cons 'B 'sr_low)) (list (cons 'Y 'sr_d)))
    (make-cell 'dff0 '$dff
               (list (cons 'WIDTH 8) (cons 'CLK_POLARITY 1))
               (list (cons 'CLK (const 0 1)) (cons 'D 'sr_d)) (list (cons 'Q 'sr)))
    (make-cell 'slice1 '$slice
               (list (cons 'OFFSET 7) (cons 'Y_WIDTH 1) (cons 'A_WIDTH 8))
               (list (cons 'A 'sr)) (list (cons 'Y 'Q))))
   '()))

;; 4-bit counter with synchronous reset
(define counter-mod
  (rtlil-module
   'counter
   (list (make-input 'RST 1) (make-output 'CNT 4))
   (make-immutable-hash
    (list (cons 'RST    (wire 'RST 1))
          (cons 'CNT    (wire 'CNT 4))
          (cons 'r      (wire 'r 4))
          (cons 'r_inc  (wire 'r_inc 4))
          (cons 'r_next (wire 'r_next 4))))
   (list
    (make-cell 'inc '$add
               (list (cons 'A_WIDTH 4) (cons 'B_WIDTH 4) (cons 'Y_WIDTH 4)
                     (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
               (list (cons 'A 'r) (cons 'B (const 1 4))) (list (cons 'Y 'r_inc)))
    (make-cell 'mux0 '$mux
               (list (cons 'WIDTH 4))
               (list (cons 'A 'r_inc) (cons 'S 'RST) (cons 'B (const 0 4)))
               (list (cons 'Y 'r_next)))
    (make-cell 'dff0 '$dff
               (list (cons 'WIDTH 4) (cons 'CLK_POLARITY 1))
               (list (cons 'CLK (const 0 1)) (cons 'D 'r_next)) (list (cons 'Q 'r)))
    (make-cell 'buf0 '$pos
               (list (cons 'A_WIDTH 4) (cons 'Y_WIDTH 4) (cons 'A_SIGNED #f))
               (list (cons 'A 'r)) (list (cons 'Y 'CNT))))
   '()))

;; Helper: get a single output as a natural number
(define (out mod inputs port)
  (bitvector->natural (hash-ref (eval-module mod inputs) port)))

;; ------------------------------------------------------------
;; Test Suite 1: Concrete Cell Evaluation
;; ------------------------------------------------------------

(define concrete-tests
  (test-suite
   "Concrete cell evaluation"

   (test-case "$and: bitwise AND"
     (check-equal? (out and8-mod (hash 'A (bv #xAB 8) 'B (bv #x0F 8)) 'Y) #x0B)
     (check-equal? (out and8-mod (hash 'A (bv #xFF 8) 'B (bv #xFF 8)) 'Y) #xFF)
     (check-equal? (out and8-mod (hash 'A (bv #x00 8) 'B (bv #xFF 8)) 'Y) #x00))

   (test-case "$add: 4-bit addition no overflow"
     (check-equal? (out add4-mod (hash 'A (bv 7 4) 'B (bv 8 4)) 'SUM)  15)
     (check-equal? (out add4-mod (hash 'A (bv 7 4) 'B (bv 8 4)) 'COUT)  0))

   (test-case "$add: 4-bit addition with carry-out"
     (check-equal? (out add4-mod (hash 'A (bv 15 4) 'B (bv 1 4)) 'SUM)   0)
     (check-equal? (out add4-mod (hash 'A (bv 15 4) 'B (bv 1 4)) 'COUT)  1))

   (test-case "$add: 4-bit addition identity"
     (check-equal? (out add4-mod (hash 'A (bv 0 4) 'B (bv 0 4)) 'SUM)  0)
     (check-equal? (out add4-mod (hash 'A (bv 0 4) 'B (bv 0 4)) 'COUT) 0))

   (test-case "eval-cell: $not"
     (let* ([c   (make-cell 'n '$not
                            (list (cons 'A_WIDTH 8) (cons 'Y_WIDTH 8))
                            (list (cons 'A 'A))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv #xAA 8))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) #x55)))

   (test-case "eval-cell: $or"
     (let* ([c   (make-cell 'o '$or
                            (list (cons 'A_WIDTH 4) (cons 'B_WIDTH 4) (cons 'Y_WIDTH 4)
                                  (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
                            (list (cons 'A 'A) (cons 'B 'B))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv #b1010 4) 'B (bv #b0101 4))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) #b1111)))

   (test-case "eval-cell: $xor"
     (let* ([c   (make-cell 'x '$xor
                            (list (cons 'A_WIDTH 4) (cons 'B_WIDTH 4) (cons 'Y_WIDTH 4)
                                  (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
                            (list (cons 'A 'A) (cons 'B 'B))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv #b1111 4) 'B (bv #b0101 4))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) #b1010)))

   (test-case "eval-cell: $mux selects A when S=0"
     (let* ([c   (make-cell 'm '$mux
                            (list (cons 'WIDTH 4))
                            (list (cons 'A 'A) (cons 'B 'B) (cons 'S 'S))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv 3 4) 'B (bv 7 4) 'S (bv 0 1))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) 3)))

   (test-case "eval-cell: $mux selects B when S=1"
     (let* ([c   (make-cell 'm '$mux
                            (list (cons 'WIDTH 4))
                            (list (cons 'A 'A) (cons 'B 'B) (cons 'S 'S))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv 3 4) 'B (bv 7 4) 'S (bv 1 1))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) 7)))

   (test-case "eval-cell: $concat"
     (let* ([c   (make-cell 'cat '$concat
                            (list (cons 'A_WIDTH 4) (cons 'B_WIDTH 4))
                            (list (cons 'A 'A) (cons 'B 'B))
                            (list (cons 'Y 'Y)))]
            ;; Y = {B, A}: B is MSB, A is LSB
            ;; B=0xA=1010, A=0xF=1111 => Y=1010_1111=0xAF
            [env (hash 'A (bv #xF 4) 'B (bv #xA 4))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) #xAF)))

   (test-case "eval-cell: $slice"
     (let* ([c   (make-cell 'sl '$slice
                            (list (cons 'OFFSET 4) (cons 'Y_WIDTH 4) (cons 'A_WIDTH 8))
                            (list (cons 'A 'A))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv #xAB 8))]
            [out (hash-ref (eval-cell c env) 'Y)])
       ;; bits [7:4] of 0xAB = 0xA
       (check-equal? (bitvector->natural out) #xA)))

   (test-case "eval-cell: $reduce_or"
     (let* ([c   (make-cell 'ro '$reduce_or
                            (list (cons 'A_WIDTH 4) (cons 'Y_WIDTH 1))
                            (list (cons 'A 'A))
                            (list (cons 'Y 'Y)))]
            [env0 (hash 'A (bv 0 4))]
            [env1 (hash 'A (bv 5 4))])
       (check-equal? (bitvector->natural (hash-ref (eval-cell c env0) 'Y)) 0)
       (check-equal? (bitvector->natural (hash-ref (eval-cell c env1) 'Y)) 1)))

   (test-case "eval-cell: $reduce_and"
     (let* ([c   (make-cell 'ra '$reduce_and
                            (list (cons 'A_WIDTH 4) (cons 'Y_WIDTH 1))
                            (list (cons 'A 'A))
                            (list (cons 'Y 'Y)))]
            [env-all  (hash 'A (bv #xF 4))]
            [env-some (hash 'A (bv #x7 4))])
       (check-equal? (bitvector->natural (hash-ref (eval-cell c env-all)  'Y)) 1)
       (check-equal? (bitvector->natural (hash-ref (eval-cell c env-some) 'Y)) 0)))

   (test-case "eval-cell: $lt unsigned"
     (let* ([c   (make-cell 'lt '$lt
                            (list (cons 'A_WIDTH 4) (cons 'B_WIDTH 4) (cons 'Y_WIDTH 1)
                                  (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
                            (list (cons 'A 'A) (cons 'B 'B))
                            (list (cons 'Y 'Y)))]
            [env-t (hash 'A (bv 3 4) 'B (bv 7 4))]
            [env-f (hash 'A (bv 7 4) 'B (bv 3 4))])
       (check-equal? (bitvector->natural (hash-ref (eval-cell c env-t) 'Y)) 1)
       (check-equal? (bitvector->natural (hash-ref (eval-cell c env-f) 'Y)) 0)))

   (test-case "eval-cell: $shl"
     (let* ([c   (make-cell 'sh '$shl
                            (list (cons 'A_WIDTH 8) (cons 'B_WIDTH 4) (cons 'Y_WIDTH 8)
                                  (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
                            (list (cons 'A 'A) (cons 'B 'B))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv 1 8) 'B (bv 3 4))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) 8)))

   (test-case "eval-cell: $shr"
     (let* ([c   (make-cell 'sh '$shr
                            (list (cons 'A_WIDTH 8) (cons 'B_WIDTH 4) (cons 'Y_WIDTH 8)
                                  (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
                            (list (cons 'A 'A) (cons 'B 'B))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv 16 8) 'B (bv 2 4))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) 4)))

   (test-case "eval-cell: $_AND_ (tech-mapped)"
     (let* ([c   (make-cell 'g '$_AND_ '()
                            (list (cons 'A 'A) (cons 'B 'B))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv 1 1) 'B (bv 0 1))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) 0)))

   (test-case "eval-cell: $_NOT_ (tech-mapped)"
     (let* ([c   (make-cell 'g '$_NOT_ '()
                            (list (cons 'A 'A))
                            (list (cons 'Y 'Y)))]
            [env (hash 'A (bv 0 1))]
            [out (hash-ref (eval-cell c env) 'Y)])
       (check-equal? (bitvector->natural out) 1)))))

;; ------------------------------------------------------------
;; Test Suite 2: Sequential Circuits
;; ------------------------------------------------------------

(define sequential-tests
  (test-suite
   "Sequential circuit evaluation"

   (test-case "shift register: initial state is zero"
     (let ([s (initial-state shift8-mod)])
       (check-equal? (bitvector->natural (hash-ref s 'sr)) 0)))

   (test-case "shift register: Q=0 before 8 ones clocked in"
     (let-values ([(out _) (step-module shift8-mod
                                        (initial-state shift8-mod)
                                        (hash 'D (bv 1 1)))])
       (check-equal? (bitvector->natural (hash-ref out 'Q)) 0)))

   (test-case "shift register: Q=1 after 8 ones clocked in"
     (define out
       (eval-module* shift8-mod
                     (initial-state shift8-mod)
                     (make-list 8 (hash 'D (bv 1 1)))))
     (check-equal? (bitvector->natural (hash-ref out 'Q)) 1))

   (test-case "shift register: Q=0 after 8 zeros following 8 ones"
     (define state-after-ones
       (let loop ([s (initial-state shift8-mod)] [i 0])
         (if (= i 8) s
             (let-values ([(_ ns) (step-module shift8-mod s (hash 'D (bv 1 1)))])
               (loop ns (+ i 1))))))
     (define out
       (eval-module* shift8-mod state-after-ones
                     (make-list 8 (hash 'D (bv 0 1)))))
     (check-equal? (bitvector->natural (hash-ref out 'Q)) 0))

   (test-case "counter: starts at 0"
     (check-equal? (bitvector->natural (hash-ref (initial-state counter-mod) 'r)) 0))

   (test-case "counter: increments each cycle"
     (define (run-counter n)
       (let loop ([s (initial-state counter-mod)] [i 0] [acc '()])
         (if (= i n) (reverse acc)
             (let-values ([(out ns) (step-module counter-mod s (hash 'RST (bv 0 1)))])
               (loop ns (+ i 1) (cons (bitvector->natural (hash-ref out 'CNT)) acc))))))
     (check-equal? (run-counter 5) '(1 2 3 4 5)))

   (test-case "counter: reset returns to 0"
     (define state-at-5
       (let loop ([s (initial-state counter-mod)] [i 0])
         (if (= i 5) s
             (let-values ([(_ ns) (step-module counter-mod s (hash 'RST (bv 0 1)))])
               (loop ns (+ i 1))))))
     (let-values ([(out _) (step-module counter-mod state-at-5 (hash 'RST (bv 1 1)))])
       (check-equal? (bitvector->natural (hash-ref out 'CNT)) 0)))))

;; ------------------------------------------------------------
;; Test Suite 3: Symbolic Queries
;; ------------------------------------------------------------

(define symbolic-tests
  (test-suite
   "Symbolic execution and SMT queries"

   (test-case "solve: find A,B where AND8 = 0xFF"
     (define-symbolic A8sym B8sym (bitvector 8))
     (define result (eval-module and8-mod (hash 'A A8sym 'B B8sym)))
     (define sol (solve (assert (bveq (hash-ref result 'Y) (bv #xFF 8)))))
     (check-true (sat? sol))
     ;; Witness must satisfy the constraint
     (check-equal?
      (bitvector->natural
       (bvand (evaluate A8sym sol) (evaluate B8sym sol)))
      #xFF))

   (test-case "solve: A AND 0 = 0 always (no solution for Y=1 with B=0)"
     (define-symbolic A8z (bitvector 8))
     (define result (eval-module and8-mod (hash 'A A8z 'B (bv 0 8))))
     (define sol (solve (assert (bveq (hash-ref result 'Y) (bv 1 8)))))
     (check-true (unsat? sol)))

   (test-case "verify: 4-bit adder computes A+B correctly"
     (define-symbolic Av Bv (bitvector 4))
     (define out (eval-module add4-mod (hash 'A Av 'B Bv)))
     (define expected (bvadd (zero-extend Av (bitvector 5))
                             (zero-extend Bv (bitvector 5))))
     (define cex
       (verify
        (assert
         (and (bveq (hash-ref out 'SUM)  (extract 3 0 expected))
              (bveq (hash-ref out 'COUT) (extract 4 4 expected))))))
     (check-true (unsat? cex)))

   (test-case "verify: addition is commutative"
     (define add8-mod
       (rtlil-module
        'add8
        (list (make-input 'A 8) (make-input 'B 8) (make-output 'Y 8))
        (make-immutable-hash
         (list (cons 'A (wire 'A 8)) (cons 'B (wire 'B 8)) (cons 'Y (wire 'Y 8))))
        (list (make-cell 'add0 '$add
                         (list (cons 'A_WIDTH 8) (cons 'B_WIDTH 8) (cons 'Y_WIDTH 8)
                               (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
                         (list (cons 'A 'A) (cons 'B 'B))
                         (list (cons 'Y 'Y))))
        '()))
     (define-symbolic Ac Bc (bitvector 8))
     (define out-ab (eval-module add8-mod (hash 'A Ac 'B Bc)))
     (define out-ba (eval-module add8-mod (hash 'A Bc 'B Ac)))
     (define cex
       (verify (assert (bveq (hash-ref out-ab 'Y) (hash-ref out-ba 'Y)))))
     (check-true (unsat? cex)))

   (test-case "verify: counter == N after N steps from reset"
     (define-symbolic cnt-init (bitvector 4))
     (define (run-from-sym-init n)
       (let loop ([s (hash 'r cnt-init)] [i 0] [last (hash 'CNT (bv 0 4))])
         (if (= i n) last
             (let-values ([(out ns)
                           (step-module counter-mod s
                                        (hash 'RST (if (= i 0) (bv 1 1) (bv 0 1))))])
               (loop ns (+ i 1) out)))))
     ;; After reset at step 0, then 5 free-run steps => CNT = 5
     (define final (run-from-sym-init 6))
     (define cex
       (verify (assert (bveq (hash-ref final 'CNT) (bv 5 4)))))
     (check-true (unsat? cex)))

   (test-case "solve: find A,B where A-B = 42"
     (define sub8-mod
       (rtlil-module
        'sub8
        (list (make-input 'A 8) (make-input 'B 8) (make-output 'Y 8))
        (make-immutable-hash
         (list (cons 'A (wire 'A 8)) (cons 'B (wire 'B 8)) (cons 'Y (wire 'Y 8))))
        (list (make-cell 'sub0 '$sub
                         (list (cons 'A_WIDTH 8) (cons 'B_WIDTH 8) (cons 'Y_WIDTH 8)
                               (cons 'A_SIGNED #f) (cons 'B_SIGNED #f))
                         (list (cons 'A 'A) (cons 'B 'B))
                         (list (cons 'Y 'Y))))
        '()))
     (define-symbolic As Bs (bitvector 8))
     (define sub-out (eval-module sub8-mod (hash 'A As 'B Bs)))
     (define sol (solve (assert (bveq (hash-ref sub-out 'Y) (bv 42 8)))))
     (check-true (sat? sol))
     (let ([av (bitvector->natural (evaluate As sol))]
           [bv-val (bitvector->natural (evaluate Bs sol))])
       (check-equal? (modulo (- av bv-val) 256) 42)))))

;; ------------------------------------------------------------
;; Run all suites
;; ------------------------------------------------------------

(define (run-all-tests)
  (run-tests concrete-tests)
  (run-tests sequential-tests)
  (run-tests symbolic-tests))

(run-all-tests)
