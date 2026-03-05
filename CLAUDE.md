# Yosys RTLIL Semantics in Rosette

A reference interpreter for Yosys RTLIL (Register Transfer Level Intermediate Language) written in Rosette. Yosys compiles Verilog into a netlist of typed cells; this project gives those cells a formal semantics using Rosette bitvectors, enabling symbolic execution and SMT-based reasoning over circuits.

## File Structure

| File | Purpose |
|------|---------|
| `yosys-rtlil.rkt` | Core data structures: wires, ports, cells, modules |
| `yosys-cells.rkt` | Cell semantics: `eval-cell` and helpers |
| `yosys-interpreter.rkt` | Module evaluator: topological sort, step/run functions |
| `examples.rkt` | Six worked examples demonstrating concrete + symbolic use |
| `tests/test-suite.rkt` | `rackunit` test suite covering all examples |
| `.github/workflows/test.yml` | GitHub Actions CI: installs Racket + Rosette, runs tests |

## Running Tests

```bash
raco test tests/test-suite.rkt
```

The CI workflow runs on every push and pull request. It installs Racket 8.14 (CS, full distribution) and Rosette via `raco pkg install`, then runs the test suite. Racket packages are cached by version + file hash to speed up subsequent runs.

## Architecture

### Signal Representation
All signal values are Rosette **bitvectors** of a declared width. A signal reference (`signal-ref`) is one of:
- A `symbol` — wire name looked up in the environment `hash[symbol → bv]`
- A `const` struct — literal integer with explicit width, converted via `const->bv`
- A raw `bv` — Rosette-specific convenience; does not exist in real RTLIL. Allows a bitvector (concrete or symbolic) to be embedded directly in a cell's inputs without routing through a named wire. `resolve-signal` just passes it through.

### Cell Evaluation (`yosys-cells.rkt`)
`eval-cell : cell × env → hash[port-name → bv]`

Pure, combinational. Reads inputs from the environment via `resolve-signal`, applies the cell's operation, returns a map of output port names to bitvector results.

Key conventions matching Yosys semantics:
- `A_SIGNED`/`B_SIGNED` parameters control sign vs. zero extension to `Y_WIDTH`
- For binary ops (`$and`, `$add`, etc.) both operands are extended to `Y_WIDTH` before operating
- Comparison results are 1-bit, zero-extended to `Y_WIDTH`
- `$div`/`$mod` return `0` on division by zero (matches Yosys)
- `$pmux`: bit 0 of `S` is highest priority

### Module Evaluation (`yosys-interpreter.rkt`)
- **Topological sort**: combinational cells are ordered so every cell's inputs are resolved before it runs. Detects combinational loops.
- **Sequential split**: `$dff` and friends are separated from combinational cells. Their `Q` outputs are seeded from a `state` hash before combinational evaluation; their `D` inputs are sampled after to produce the next state.
- **Clock abstraction**: clock signals are not modeled; one call to `step-module` = one clock edge.
- **`step-module` evaluation order**: combinational logic is evaluated twice per cycle — once with current state to find DFF D inputs (`pre-env`), then once with the new state after the clock edge (`post-env`). Outputs are read from `post-env`. This is an implementation artifact of separating combinational and sequential cells, not a named technique. The design choice it reflects is standard RTL simulation convention: outputs are observed after DFFs have updated.

### Public API

```racket
;; Purely combinational module
(eval-module mod inputs-hash) → outputs-hash

;; One clock cycle of a sequential module
(step-module mod state inputs) → (values outputs next-state)

;; N clock cycles from a list of input vectors
(eval-module* mod init-state input-list) → final-outputs

;; All DFF Q outputs initialized to 0
(initial-state mod) → state-hash
```

## Supported Cell Types

### Logic
`$not` `$and` `$or` `$xor` `$xnor`

### Reduction (fold over bits of A)
`$reduce_and` `$reduce_or` `$reduce_xor` `$reduce_xnor` `$reduce_bool`

### Arithmetic
`$pos` `$neg` `$add` `$sub` `$mul` `$div` `$mod`

### Comparison (1-bit result, zero-extended to Y_WIDTH)
`$lt` `$le` `$gt` `$ge` `$eq` `$ne` `$eqx` `$nex`

### Shift
`$shl` `$sshl` `$shr` `$sshr` `$shift`

### Mux
`$mux` (1-bit select) `$pmux` (priority mux, N-bit select + N×WIDTH data)

### Concat / Slice
`$concat` (Y = {B, A}) `$slice` (Y = A[OFFSET+Y_WIDTH-1 : OFFSET])

### Sequential
`$dff` `$dffe` `$adff` `$adffe` `$sdff` `$sdffe` `$dlatch` `$_DFF_P_` `$_DFF_N_`

### Technology-Mapped Gates (single-bit)
`$_BUF_` `$_NOT_` `$_AND_` `$_NAND_` `$_OR_` `$_NOR_` `$_XOR_` `$_XNOR_`
`$_MUX_` `$_NMUX_` `$_AOI3_` `$_OAI3_` `$_AOI4_` `$_OAI4_`

## Examples

Six examples in `examples.rkt`:

1. **8-bit AND** — concrete evaluation and symbolic `solve` to find inputs where `Y = 0xFF`
2. **4-bit adder** — `verify` proves `SUM`/`COUT` are always correct for all 4-bit inputs
3. **8-bit shift register** — multi-cycle `step-module` simulation showing serial-in/parallel-out
4. **Equivalence checking** — `verify` proves `A+B == B+A` for all 8-bit inputs
5. **Synthesis** — `solve` finds `A,B` satisfying `A - B = 42`
6. **Counter safety** — `verify` proves a resettable counter equals 5 after reset + 5 steps, regardless of initial register state

## Potential Extensions

- **Memory cells**: `$mem`, `$mem_rd`, `$mem_wr` using Rosette arrays (`define-symbolic` over array type)
- **RTLIL text parser**: parse `yosys -p "write_rtlil"` output into the data structures here
- **JSON import**: parse `yosys -p "write_json"` output (easier than text RTLIL)
- **Bounded model checking**: loop `step-module` k times with symbolic inputs + state, assert invariants
- **`$pow`**: exponentiation via repeated squaring (rarely emitted by synthesis)
- **RTLIL processes**: case/switch blocks that appear before `proc` pass flattens them
