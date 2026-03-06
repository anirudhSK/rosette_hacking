#lang rosette

;; ============================================================
;; tiny_cpu ISA — Test Suite
;; Run with: raco test tests/test-tiny-cpu.rkt
;; ============================================================

(require rackunit
         rackunit/text-ui
         (only-in rackunit/private/check-info
                  pretty-info? pretty-info-value
                  verbose-info? verbose-info-value)
         "../tiny-cpu.rkt")

;; ── Test helpers ──────────────────────────────────────────────────────────

;; Read a data memory cell as a plain integer.
(define (dm-nat state i) (bitvector->natural (cpu-dm-ref state i)))

;; Read a register as a plain integer.
(define (reg-nat state i) (bitvector->natural (cpu-reg-ref state i)))

;; Make initial state with specific dmem values; all registers and pc = 0.
(define (init-dmem . vals)
  (make-cpu-state
   #:dmem (append vals (make-list (- 20 (length vals)) 0))))

;; ── Programs ──────────────────────────────────────────────────────────────
;;
;; Register conventions used throughout:
;;   r0 = 0 at start, used as zero base for loads/stores
;;   r1, r2 = operands (loaded from dmem)
;;   r3, r4 = results (stored to dmem)

;; ── Test suite: concrete simulation ──────────────────────────────────────

(define isa-tests
  (test-suite
   "tiny_cpu ISA — concrete simulation"

   ;; ── ADD ───────────────────────────────────────────────────────────────
   ;;
   ;;   dm[0]=3, dm[1]=5
   ;;   LOAD r1, r0, 0    ; r1 = dm[0] = 3
   ;;   LOAD r2, r0, 1    ; r2 = dm[1] = 5
   ;;   ADD  r3, r1, r2   ; r3 = 8
   ;;   STORE r3, r0, 2   ; dm[2] = 8

   (test-case "ADD: 3 + 5 = 8"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-add   3 1 2)
                          (asm-store 3 0 2)))
     (define state (cpu-run (init-dmem 3 5) prog 4))
     (check-equal? (dm-nat state 2) 8))

   ;; ── SUB ───────────────────────────────────────────────────────────────
   ;;
   ;;   dm[0]=10, dm[1]=3
   ;;   LOAD r1, r0, 0    ; r1 = 10
   ;;   LOAD r2, r0, 1    ; r2 = 3
   ;;   SUB  r3, r1, r2   ; r3 = 7
   ;;   STORE r3, r0, 2   ; dm[2] = 7

   (test-case "SUB: 10 - 3 = 7"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-sub   3 1 2)
                          (asm-store 3 0 2)))
     (define state (cpu-run (init-dmem 10 3) prog 4))
     (check-equal? (dm-nat state 2) 7))

   ;; ── AND ───────────────────────────────────────────────────────────────
   ;;
   ;;   dm[0]=0xF0, dm[1]=0x0F
   ;;   AND result (0x00) → dm[2]

   (test-case "AND: 0xF0 & 0x0F = 0x00"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-and   3 1 2)
                          (asm-store 3 0 2)))
     (define state (cpu-run (init-dmem #xF0 #x0F) prog 4))
     (check-equal? (dm-nat state 2) #x00))

   ;; ── OR ────────────────────────────────────────────────────────────────
   ;;
   ;;   dm[0]=0xF0, dm[1]=0x0F
   ;;   OR result (0xFF) → dm[2]

   (test-case "OR: 0xF0 | 0x0F = 0xFF"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-or    3 1 2)
                          (asm-store 3 0 2)))
     (define state (cpu-run (init-dmem #xF0 #x0F) prog 4))
     (check-equal? (dm-nat state 2) #xFF))

   ;; ── AND + OR in same program ──────────────────────────────────────────
   ;;
   ;;   Both results written to different cells in one run.

   (test-case "AND+OR: mask and merge in one program"
     (define prog (vector (asm-load  1 0 0)    ; r1 = 0xF0
                          (asm-load  2 0 1)    ; r2 = 0x0F
                          (asm-and   3 1 2)    ; r3 = 0x00
                          (asm-or    4 1 2)    ; r4 = 0xFF
                          (asm-store 3 0 2)    ; dm[2] = 0x00
                          (asm-store 4 0 3)))  ; dm[3] = 0xFF
     (define state (cpu-run (init-dmem #xF0 #x0F) prog 6))
     (check-equal? (dm-nat state 2) #x00)
     (check-equal? (dm-nat state 3) #xFF))

   ;; ── 8-bit overflow ────────────────────────────────────────────────────
   ;;
   ;;   0xFF + 0x01 wraps to 0x00 in 8-bit arithmetic.

   (test-case "ADD overflow: 0xFF + 0x01 = 0x00 (mod 256)"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 0 1)
                          (asm-add   3 1 2)
                          (asm-store 3 0 2)))
     (define state (cpu-run (init-dmem #xFF #x01) prog 4))
     (check-equal? (dm-nat state 2) #x00))

   ;; ── Immediate offset addressing ───────────────────────────────────────
   ;;
   ;;   dm[5]=42; load it with a non-zero immediate, double, and store.
   ;;   LOAD r1, r0, 5    ; r1 = dm[0+5] = 42
   ;;   ADD  r2, r1, r1   ; r2 = 84
   ;;   STORE r2, r0, 6   ; dm[6] = 84

   (test-case "LOAD/STORE with immediate offset: dm[5]=42 → double → dm[6]=84"
     (define prog (vector (asm-load  1 0 5)
                          (asm-add   2 1 1)
                          (asm-store 2 0 6)))
     (define state (cpu-run (init-dmem 0 0 0 0 0 42) prog 3))
     (check-equal? (dm-nat state 6) 84))

   ;; ── rs1 as base address ───────────────────────────────────────────────
   ;;
   ;;   Use a register (not r0) as the base for LOAD and STORE.
   ;;   dm[0]=3 (pointer), dm[3]=99 (target value)
   ;;   LOAD r1, r0, 0    ; r1 = dm[0]   = 3  (pointer)
   ;;   LOAD r2, r1, 0    ; r2 = dm[r1]  = dm[3] = 99
   ;;   STORE r2, r0, 10  ; dm[10] = 99

   (test-case "Register-based addressing: use loaded value as base"
     (define prog (vector (asm-load  1 0 0)
                          (asm-load  2 1 0)
                          (asm-store 2 0 10)))
     (define state (cpu-run (init-dmem 3 0 0 99) prog 3))
     (check-equal? (dm-nat state 10) 99))

   ;; ── Chained ALU operations ────────────────────────────────────────────
   ;;
   ;;   dm[0]=2, dm[1]=3, dm[2]=4
   ;;   r1 = 2, r2 = 3, r3 = 4
   ;;   r4 = r1 + r2 = 5
   ;;   r5 = r4 + r3 = 9
   ;;   dm[3] = 9

   (test-case "Chained ADD: 2 + 3 + 4 = 9"
     (define prog (vector (asm-load  1 0 0)    ; r1 = 2
                          (asm-load  2 0 1)    ; r2 = 3
                          (asm-load  3 0 2)    ; r3 = 4
                          (asm-add   4 1 2)    ; r4 = 5
                          (asm-add   5 4 3)    ; r5 = 9
                          (asm-store 5 0 3)))  ; dm[3] = 9
     (define state (cpu-run (init-dmem 2 3 4) prog 6))
     (check-equal? (dm-nat state 3) 9))

   ;; ── NOP does not disturb state ────────────────────────────────────────
   ;;
   ;;   Running extra NOP cycles must not alter dm values.

   (test-case "NOP cycles do not disturb memory"
     (define prog (vector (asm-load  1 0 0)
                          (asm-store 1 0 1)
                          asm-nop
                          asm-nop
                          asm-nop))
     (define state (cpu-run (init-dmem 77) prog 5))
     (check-equal? (dm-nat state 1) 77))

   ;; ── Out-of-range STORE is silently ignored ────────────────────────────
   ;;
   ;;   Store to address 200 (beyond the 20-entry dmem) must not crash
   ;;   and must leave all valid dmem cells unchanged.

   (test-case "Out-of-range STORE is silently ignored"
     ;; r1 = 200 (load from dm[0]); store r2=42 to dm[200] → ignored
     (define prog (vector (asm-load  1 0 0)    ; r1 = 200 (base)
                          (asm-load  2 0 1)    ; r2 = 42  (value)
                          (asm-store 2 1 0)))  ; dm[r1+0] = dm[200] = r2 (ignored)
     (define state (cpu-run (init-dmem 200 42) prog 3))
     ;; dm[1] should still be 42, not overwritten
     (check-equal? (dm-nat state 1) 42))

   ;; ── PC advances correctly ─────────────────────────────────────────────

   (test-case "PC increments by 1 each cycle"
     (define prog (vector asm-nop asm-nop asm-nop))
     (define state (cpu-run (make-cpu-state) prog 3))
     (check-equal? (bitvector->natural (hash-ref state 'pc)) 3))))

;; ── Verbose runner (matches test-suite.rkt style) ─────────────────────────

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
         [(test-success? result)
          (printf "  PASS  ~a\n" tname)]
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
  (printf "~a\n" (if (zero? failures)
                     "All tests passed."
                     (format "~a test(s) FAILED." failures)))
  failures)

(define total-failures (run-suite/verbose isa-tests))
(printf "\n~a\n"
        (if (zero? total-failures)
            "=== All suites passed ==="
            (format "=== ~a failure(s) total ===" total-failures)))
