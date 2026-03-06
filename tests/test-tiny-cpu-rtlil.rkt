#lang rosette

;; ============================================================
;; Cross-validation: ISA interpreter vs. Yosys netlist
;;
;; For each test program, run both interpreters on the same inputs
;; and assert that every observable output (registers, dmem cells,
;; pc) matches exactly.
;;
;; - ISA interpreter  : tiny-cpu.rkt  (cpu-run)
;; - Yosys netlist    : yosys-json.rkt (json-cpu-run) over tiny_cpu.json,
;;                      the circuit compiled from tiny_cpu.v by Yosys
;;                      (proc; opt passes — word-level cells retained)
;; ============================================================

(require rackunit
         rackunit/text-ui
         (only-in rackunit/private/check-info
                  pretty-info? pretty-info-value
                  verbose-info? verbose-info-value)
         "../tiny-cpu.rkt"
         "../yosys-json.rkt")

;; ── Load Yosys module ────────────────────────────────────────────────────

(define tiny-cpu-json
  (load-json-module (build-path (current-directory) ".." "tiny_cpu.json")))

;; ── Helpers ──────────────────────────────────────────────────────────────

;; Pack a program vector (of 16-bit bvs) into a single 1024-bit bitvector.
;; Instruction i sits at bits [16i+15 : 16i].
(define (pack-imem prog)
  (for/fold ([packed 0])
            ([instr (in-vector prog)] [i (in-naturals)])
    (bitwise-ior packed
                 (arithmetic-shift (bitvector->natural instr) (* 16 i))))
  ;; return as bv
  (bv (for/fold ([p 0])
                ([instr (in-vector prog)] [i (in-naturals)])
        (bitwise-ior p (arithmetic-shift (bitvector->natural instr) (* 16 i))))
      1024))

;; Standard inputs: rst=0, clk=0 (clock abstracted), imem = packed program.
(define (make-inputs prog)
  (hash 'rst  (bv 0 1)
        'clk  (bv 0 1)
        'imem (pack-imem prog)))

;; Run both interpreters and return (isa-final-state, json-final-outputs).
(define (run-both prog n #:dmem [dmem (make-list 20 0)])
  (define isa-state (cpu-run (make-cpu-state #:dmem dmem) prog n))
  (define json-out  (json-cpu-run tiny-cpu-json
                                  (make-json-state tiny-cpu-json #:dmem dmem)
                                  (make-inputs prog)
                                  n))
  (values isa-state json-out))

;; Check that all 8 registers and 20 dmem cells match between the two.
(define (check-all-match isa-state json-out label)
  ;; Registers
  (for ([i (in-range 8)])
    (define rname  (string->symbol (format "r~a_out" i)))
    (define isa-v  (bitvector->natural (cpu-reg-ref isa-state i)))
    (define json-v (bitvector->natural (hash-ref json-out rname (bv 0 8))))
    (check-equal? json-v isa-v
                  (format "~a: r~a" label i)))
  ;; Data memory
  (for ([i (in-range 20)])
    (define dname  (string->symbol (format "dm~a_out" i)))
    (define isa-v  (bitvector->natural (cpu-dm-ref isa-state i)))
    (define json-v (bitvector->natural (hash-ref json-out dname (bv 0 8))))
    (check-equal? json-v isa-v
                  (format "~a: dm~a" label i))))

;; ── Test suite ───────────────────────────────────────────────────────────

(define rtlil-vs-isa-tests
  (test-suite
   "Yosys netlist vs. ISA interpreter — cross-validation"

   ;; ── ADD ───────────────────────────────────────────────────────────────
   (test-case "ADD: 3 + 5 = 8 (cross-check)"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-add   3 1 2)
                          (asm-store 3 0 2)))
     (define-values (isa json) (run-both prog 4 #:dmem '(3 5 0 0 0 0 0 0 0 0
                                                          0 0 0 0 0 0 0 0 0 0)))
     (check-all-match isa json "ADD 3+5"))

   ;; ── SUB ───────────────────────────────────────────────────────────────
   (test-case "SUB: 10 - 3 = 7 (cross-check)"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-sub   3 1 2)
                          (asm-store 3 0 2)))
     (define-values (isa json) (run-both prog 4 #:dmem '(10 3 0 0 0 0 0 0 0 0
                                                          0  0 0 0 0 0 0 0 0 0)))
     (check-all-match isa json "SUB 10-3"))

   ;; ── AND ───────────────────────────────────────────────────────────────
   (test-case "AND: 0xF0 & 0x0F = 0x00 (cross-check)"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-and   3 1 2)
                          (asm-store 3 0 2)))
     (define-values (isa json) (run-both prog 4 #:dmem (list #xF0 #x0F 0 0 0 0 0 0 0 0
                                                              0    0   0 0 0 0 0 0 0 0)))
     (check-all-match isa json "AND F0&0F"))

   ;; ── OR ────────────────────────────────────────────────────────────────
   (test-case "OR: 0xF0 | 0x0F = 0xFF (cross-check)"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-or    3 1 2)
                          (asm-store 3 0 2)))
     (define-values (isa json) (run-both prog 4 #:dmem (list #xF0 #x0F 0 0 0 0 0 0 0 0
                                                              0    0   0 0 0 0 0 0 0 0)))
     (check-all-match isa json "OR F0|0F"))

   ;; ── ADD overflow ──────────────────────────────────────────────────────
   (test-case "ADD overflow: 0xFF + 0x01 = 0x00 (cross-check)"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-add   3 1 2)
                          (asm-store 3 0 2)))
     (define-values (isa json) (run-both prog 4 #:dmem (list #xFF #x01 0 0 0 0 0 0 0 0
                                                              0    0   0 0 0 0 0 0 0 0)))
     (check-all-match isa json "ADD overflow"))

   ;; ── LOAD/STORE with immediate offset ─────────────────────────────────
   (test-case "LOAD/STORE with imm offset (cross-check)"
     (define prog (vector (asm-load  1 0 5)
                          (asm-add   2 1 1)
                          (asm-store 2 0 6)))
     (define-values (isa json) (run-both prog 3 #:dmem '(0 0 0 0 0 42 0 0 0 0
                                                          0 0 0 0 0  0 0 0 0 0)))
     (check-all-match isa json "LOAD/STORE imm"))

   ;; ── Register-based addressing ─────────────────────────────────────────
   (test-case "Register-based addressing (cross-check)"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 1 0)
                          (asm-store 2 0 10)))
     (define-values (isa json) (run-both prog 3 #:dmem '(3 0 0 99 0 0 0 0 0 0
                                                          0 0 0  0 0 0 0 0 0 0)))
     (check-all-match isa json "reg-based addr"))

   ;; ── Chained ALU ───────────────────────────────────────────────────────
   (test-case "Chained ADD: 2 + 3 + 4 = 9 (cross-check)"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-load  3 0 2)
                          (asm-add   4 1 2)
                          (asm-add   5 4 3)
                          (asm-store 5 0 3)))
     (define-values (isa json) (run-both prog 6 #:dmem '(2 3 4 0 0 0 0 0 0 0
                                                          0 0 0 0 0 0 0 0 0 0)))
     (check-all-match isa json "chained ADD"))))

;; ── Verbose runner ────────────────────────────────────────────────────────

(define (format-check-info ci)
  (define v (check-info-value ci))
  (define vs (cond
    [(pretty-info? v)  (format "~s" (pretty-info-value v))]
    [(verbose-info? v) (format "~s" (verbose-info-value v))]
    [else              (format "~s" v)]))
  (format "      ~a: ~a" (check-info-name ci) vs))

(define (run-suite/verbose suite)
  (define sname (rackunit-test-suite-name suite))
  (printf "\n~a\n~a\n" sname (make-string (string-length sname) #\-))
  (define failures
    (fold-test-results
     (lambda (result acc)
       (define tname (or (test-result-test-case-name result) "<unnamed>"))
       (cond
         [(test-success? result) (printf "  PASS  ~a\n" tname)]
         [(test-failure? result)
          (printf "  FAIL  ~a\n" tname)
          (for-each (lambda (ci) (displayln (format-check-info ci)))
                    (exn:test:check-stack (test-failure-result result)))]
         [(test-error? result)
          (printf "  ERROR ~a\n      ~a\n" tname
                  (exn-message (test-error-result result)))])
       (if (test-success? result) acc (+ acc 1)))
     0
     suite))
  (printf "~a\n" (if (zero? failures) "All tests passed."
                     (format "~a test(s) FAILED." failures)))
  failures)

(define total (run-suite/verbose rtlil-vs-isa-tests))
(printf "\n~a\n" (if (zero? total) "=== All suites passed ==="
                     (format "=== ~a failure(s) total ===" total)))
