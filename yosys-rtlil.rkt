#lang rosette

;; ============================================================
;; Yosys RTLIL (Register Transfer Level Intermediate Language)
;; Data structures for representing Yosys netlists in Rosette.
;;
;; Reference: https://yosyshq.readthedocs.io/projects/yosys/en/latest/RTLIL_internals.html
;; ============================================================

(provide (all-defined-out))

;; ------------------------------------------------------------
;; Signal / Wire
;; ------------------------------------------------------------
;; A signal is a named wire with a specific bit-width.
;; In RTLIL, all values are bitvectors of a given width.

(struct wire
  (name    ; symbol: wire identifier
   width   ; positive integer: number of bits
   )
  #:transparent)

;; A port is a wire with a direction.
(struct port
  (direction  ; 'input | 'output | 'inout
   wire       ; wire struct
   )
  #:transparent)

;; ------------------------------------------------------------
;; Cell
;; ------------------------------------------------------------
;; A cell is a primitive operation in the netlist. It has:
;;   - type    : symbol naming the cell kind (e.g., '$and)
;;   - params  : hash of parameter name -> value (integers/booleans)
;;   - inputs  : hash of port name -> signal-ref
;;   - outputs : hash of port name -> signal-ref
;;
;; A signal-ref is either:
;;   - a wire name (symbol), or
;;   - a constant: (const bits width) where bits is an integer

(struct cell
  (name     ; symbol: instance name (for debugging)
   type     ; symbol: cell type, e.g., '$and, '$dff, '$mux
   params   ; hash: symbol -> any  (e.g., 'A_WIDTH -> 8)
   inputs   ; hash: symbol -> signal-ref
   outputs  ; hash: symbol -> signal-ref
   )
  #:transparent)

;; A literal constant appearing in a connection.
(struct const
  (value   ; integer (two's complement)
   width   ; positive integer
   )
  #:transparent)

;; Helper to make a constant bitvector from a const struct.
(define (const->bv c)
  (bv (const-value c) (const-width c)))

;; ------------------------------------------------------------
;; Process (always block abstraction)
;; ------------------------------------------------------------
;; Yosys can lower always blocks into cells, but some passes
;; leave RTLIL processes. We model a simplified form.

(struct rtlil-process
  (name     ; symbol
   cases    ; list of rtlil-case
   )
  #:transparent)

(struct rtlil-case
  (compare   ; list of (signal-ref . signal-ref) pairs to match, or '() for default
   actions   ; list of (wire-name . signal-ref) assignments
   )
  #:transparent)

;; ------------------------------------------------------------
;; Module
;; ------------------------------------------------------------
;; A module is a collection of wires, cells, and processes.

(struct rtlil-module
  (name      ; symbol
   ports     ; list of port structs (ordered)
   wires     ; hash: symbol -> wire  (all internal + port wires)
   cells     ; list of cell structs
   processes ; list of rtlil-process structs (usually empty after synthesis)
   )
  #:transparent)

;; ------------------------------------------------------------
;; Design
;; ------------------------------------------------------------
;; A design is a collection of modules.

(struct design
  (modules   ; hash: symbol -> rtlil-module
   top       ; symbol: name of top-level module
   )
  #:transparent)

;; ------------------------------------------------------------
;; Utility constructors
;; ------------------------------------------------------------

;; Make a simple input port
(define (make-input name width)
  (port 'input (wire name width)))

;; Make a simple output port
(define (make-output name width)
  (port 'output (wire name width)))

;; Make a cell with the given type and connections.
;; params, inputs, outputs are association lists for convenience.
(define (make-cell name type params inputs outputs)
  (cell name type
        (make-immutable-hash params)
        (make-immutable-hash inputs)
        (make-immutable-hash outputs)))

;; Retrieve a parameter with a default value.
(define (cell-param c key default)
  (hash-ref (cell-params c) key default))

;; Retrieve a required parameter (error if missing).
(define (cell-param! c key)
  (hash-ref (cell-params c) key
            (lambda ()
              (error 'cell-param! "cell ~a missing required param ~a" (cell-name c) key))))

;; ------------------------------------------------------------
;; Well-known cell type constants (as symbols)
;; ------------------------------------------------------------

;; Logic
(define $not   '$not)
(define $and   '$and)
(define $or    '$or)
(define $xor   '$xor)
(define $xnor  '$xnor)
(define $nand  '$nand)  ; available as $_NAND_ primitive
(define $nor   '$nor)   ; available as $_NOR_ primitive

;; Reduction (fold over bits of A)
(define $reduce_and  '$reduce_and)
(define $reduce_or   '$reduce_or)
(define $reduce_xor  '$reduce_xor)
(define $reduce_xnor '$reduce_xnor)
(define $reduce_bool '$reduce_bool)

;; Arithmetic
(define $pos  '$pos)   ; identity / zero-extend
(define $neg  '$neg)   ; negate (2's complement)
(define $add  '$add)
(define $sub  '$sub)
(define $mul  '$mul)
(define $div  '$div)
(define $mod  '$mod)
(define $pow  '$pow)

;; Comparison (result is 1-bit)
(define $lt   '$lt)
(define $le   '$le)
(define $gt   '$gt)
(define $ge   '$ge)
(define $eq   '$eq)
(define $ne   '$ne)
(define $eqx  '$eqx)   ; ===  (no X/Z in Rosette model)
(define $nex  '$nex)   ; !==

;; Shift
(define $shl   '$shl)
(define $shr   '$shr)
(define $sshl  '$sshl)
(define $sshr  '$sshr)
(define $shift '$shift)  ; signed shift amount

;; Mux
(define $mux   '$mux)
(define $pmux  '$pmux)  ; priority mux

;; Flip-flops
(define $dff   '$dff)
(define $dffe  '$dffe)  ; DFF with clock enable
(define $adff  '$adff)  ; DFF with async reset
(define $adffe '$adffe) ; DFF with async reset + enable
(define $sdff  '$sdff)  ; DFF with sync reset
(define $sdffe '$sdffe) ; DFF with sync reset + enable
(define $dlatch '$dlatch) ; level-sensitive latch

;; Concatenation / slicing
(define $concat '$concat)
(define $slice  '$slice)

;; Memory (high-level)
(define $mem    '$mem)
(define $mem_rd '$mem_rd)
(define $mem_wr '$mem_wr)

;; Simple gates (technology-mapped)
(define $_BUF_   '$_BUF_)
(define $_NOT_   '$_NOT_)
(define $_AND_   '$_AND_)
(define $_NAND_  '$_NAND_)
(define $_OR_    '$_OR_)
(define $_NOR_   '$_NOR_)
(define $_XOR_   '$_XOR_)
(define $_XNOR_  '$_XNOR_)
(define $_MUX_   '$_MUX_)
(define $_NMUX_  '$_NMUX_)
(define $_AOI3_  '$_AOI3_)
(define $_OAI3_  '$_OAI3_)
(define $_AOI4_  '$_AOI4_)
(define $_OAI4_  '$_OAI4_)
(define $_DFF_P_ '$_DFF_P_) ; positive-edge DFF
(define $_DFF_N_ '$_DFF_N_) ; negative-edge DFF
