#lang rosette

;; ============================================================
;; tiny-cpu.rkt — ISA interpreter for tiny_cpu.v
;;
;; A high-level Rosette model of the tiny_cpu Verilog. Rather
;; than building the circuit from RTLIL cells, this directly
;; encodes the ISA semantics, making it fast to simulate and
;; easy to test. It is the "golden model" for the hardware.
;;
;; State is a hash:
;;   'pc        → (bitvector 6)
;;   'r0–'r7    → (bitvector 8)
;;   'dm0–'dm19 → (bitvector 8)
;;
;; The program is a vector of 16-bit bitvectors (instruction words).
;; ============================================================

(provide
 ;; Assembler
 asm-add asm-sub asm-and asm-or asm-load asm-store asm-nop
 ;; State
 make-cpu-state cpu-dm-ref cpu-reg-ref
 ;; Simulation
 cpu-step cpu-run
 ;; Name tables (useful in tests)
 reg-names dm-names)

;; ── Opcode bitvectors ─────────────────────────────────────────────────────
(define OP-ADD   (bv #x0 4))
(define OP-SUB   (bv #x1 4))
(define OP-AND   (bv #x2 4))
(define OP-OR    (bv #x3 4))
(define OP-LOAD  (bv #x4 4))
(define OP-STORE (bv #x5 4))

;; ── Assembler helpers ─────────────────────────────────────────────────────
;; Arguments are plain integers; result is a 16-bit bv.

(define (encode-r op rd rs1 rs2)
  (bv (bitwise-ior (arithmetic-shift op  12)
                   (arithmetic-shift rd   9)
                   (arithmetic-shift rs1  6)
                   (arithmetic-shift rs2  3))
      16))

(define (encode-i op rd rs1 imm6)
  (bv (bitwise-ior (arithmetic-shift op  12)
                   (arithmetic-shift rd   9)
                   (arithmetic-shift rs1  6)
                   (bitwise-and imm6 #x3F))
      16))

(define (asm-add   rd rs1 rs2) (encode-r #x0 rd rs1 rs2))
(define (asm-sub   rd rs1 rs2) (encode-r #x1 rd rs1 rs2))
(define (asm-and   rd rs1 rs2) (encode-r #x2 rd rs1 rs2))
(define (asm-or    rd rs1 rs2) (encode-r #x3 rd rs1 rs2))
(define (asm-load  rd rs1 imm) (encode-i #x4 rd rs1 imm))
(define (asm-store rd rs1 imm) (encode-i #x5 rd rs1 imm))
(define asm-nop                (asm-add 0 0 0))  ; r0 = r0 + r0, harmless

;; ── State name tables ─────────────────────────────────────────────────────
(define reg-names '#(r0 r1 r2 r3 r4 r5 r6 r7))
(define dm-names  '#(dm0  dm1  dm2  dm3  dm4
                     dm5  dm6  dm7  dm8  dm9
                     dm10 dm11 dm12 dm13 dm14
                     dm15 dm16 dm17 dm18 dm19))

;; ── State constructors / accessors ────────────────────────────────────────

;; make-cpu-state : #:pc int  #:regs (listof int)  #:dmem (listof int)
;;               → state-hash
(define (make-cpu-state #:pc   [pc-val 0]
                        #:regs [regs   (make-list 8  0)]
                        #:dmem [dmem   (make-list 20 0)])
  (define h (hash 'pc (bv pc-val 6)))
  (define h2
    (for/fold ([acc h])
              ([name (in-vector reg-names)] [v regs])
      (hash-set acc name (bv v 8))))
  (for/fold ([acc h2])
            ([name (in-vector dm-names)] [v dmem])
    (hash-set acc name (bv v 8))))

;; Read a register by index (0–7).
(define (cpu-reg-ref state i)
  (hash-ref state (vector-ref reg-names i)))

;; Read a data memory cell by index (0–19).
(define (cpu-dm-ref state i)
  (hash-ref state (vector-ref dm-names i)))

;; ── Internal: register file and memory access ─────────────────────────────

(define (reg-read state idx-bv)
  (hash-ref state (vector-ref reg-names (bitvector->natural idx-bv))))

(define (reg-write state idx-bv val)
  (hash-set state (vector-ref reg-names (bitvector->natural idx-bv)) val))

(define (dmem-read state ea-bv)
  (let ([i (bitvector->natural ea-bv)])
    (if (< i 20)
        (hash-ref state (vector-ref dm-names i))
        (bv 0 8))))          ; out-of-range reads return 0

(define (dmem-write state ea-bv val)
  (let ([i (bitvector->natural ea-bv)])
    (if (< i 20)
        (hash-set state (vector-ref dm-names i) val)
        state)))              ; out-of-range writes ignored

;; ── cpu-step ──────────────────────────────────────────────────────────────
;; Execute one clock cycle. program is a vector of 16-bit bvs.

(define (cpu-step state program)
  (define pc     (hash-ref state 'pc))
  (define pc-nat (bitvector->natural pc))

  ;; Fetch — out-of-bounds yields NOP
  (define instr
    (if (< pc-nat (vector-length program))
        (vector-ref program pc-nat)
        (bv 0 16)))

  ;; Decode
  (define op   (extract 15 12 instr))
  (define rd   (extract 11  9 instr))
  (define rs1  (extract  8  6 instr))
  (define rs2  (extract  5  3 instr))
  (define imm6 (extract  5  0 instr))

  ;; Register reads
  (define va (reg-read state rs1))   ; rs1 value
  (define vb (reg-read state rs2))   ; rs2 value
  (define vd (reg-read state rd))    ; rd value (source for STORE)

  ;; Effective address: zero-extend imm6 to 8 bits and add to rs1
  (define ea (bvadd va (zero-extend imm6 (bitvector 8))))

  ;; ALU (OR is the default, matching Verilog)
  (define alu-out
    (cond
      [(bveq op OP-ADD) (bvadd va vb)]
      [(bveq op OP-SUB) (bvsub va vb)]
      [(bveq op OP-AND) (bvand va vb)]
      [else             (bvor  va vb)]))

  ;; Register writeback
  (define rf-wdata (if (bveq op OP-LOAD) (dmem-read state ea) alu-out))
  (define rf-we    (or (bveq op OP-ADD) (bveq op OP-SUB)
                       (bveq op OP-AND) (bveq op OP-OR)
                       (bveq op OP-LOAD)))

  ;; Commit: PC first, then register file, then data memory
  (define s1 (hash-set state 'pc (bvadd pc (bv 1 6))))
  (define s2 (if rf-we (reg-write s1 rd rf-wdata) s1))
  (define s3 (if (bveq op OP-STORE) (dmem-write s2 ea vd) s2))
  s3)

;; ── cpu-run ───────────────────────────────────────────────────────────────
;; Run n clock cycles and return the final state.

(define (cpu-run init-state program n)
  (let loop ([state init-state] [i 0])
    (if (= i n)
        state
        (loop (cpu-step state program) (+ i 1)))))
