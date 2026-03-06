// tiny_cpu.v
// Minimal single-cycle 8-bit RISC processor.
// Register file and data memory are fully flattened (no Verilog arrays)
// so that Yosys emits only DFF cells — no $mem cells.
//
// ISA — 16-bit fixed-width instructions
// ──────────────────────────────────────
//   R-type  [15:12]=op  [11:9]=rd  [8:6]=rs1  [5:3]=rs2  [2:0]=---
//   I-type  [15:12]=op  [11:9]=rd  [8:6]=rs1  [5:0]=imm6
//
//   Mnemonic               Semantics
//   ADD  rd, rs1, rs2      rd = rs1 + rs2
//   SUB  rd, rs1, rs2      rd = rs1 - rs2
//   AND  rd, rs1, rs2      rd = rs1 & rs2
//   OR   rd, rs1, rs2      rd = rs1 | rs2
//   LOAD  rd, rs1, imm6    rd = mem[rs1 + imm6]
//   STORE rd, rs1, imm6    mem[rs1 + imm6] = rd
//
// Registers : R0–R7, 8-bit, all general-purpose.
// Data mem  : 20 × 8-bit (addresses 0–19; out-of-range reads return 0,
//             out-of-range writes are ignored).
// Inst mem  : supplied as a 1024-bit input bus (64 × 16-bit words packed),
//             so the program is an input rather than internal state.
// Control   : no branches; PC increments by 1 every cycle.

module tiny_cpu (
    input             clk,
    input             rst,
    input [1023:0]    imem    // 64 × 16-bit instruction words, packed
);

// ── Opcodes ───────────────────────────────────────────────────────────────
localparam ADD   = 4'h0,
           SUB   = 4'h1,
           AND   = 4'h2,
           OR    = 4'h3,
           LOAD  = 4'h4,
           STORE = 4'h5;

// ── Architectural state ───────────────────────────────────────────────────
reg [5:0] pc;

// Register file: R0–R7
reg [7:0] r0, r1, r2, r3, r4, r5, r6, r7;

// Data memory: 20 × 8-bit entries
reg [7:0] dm0,  dm1,  dm2,  dm3,  dm4,
          dm5,  dm6,  dm7,  dm8,  dm9,
          dm10, dm11, dm12, dm13, dm14,
          dm15, dm16, dm17, dm18, dm19;

// ── Fetch ─────────────────────────────────────────────────────────────────
// imem is packed little-endian: word i occupies bits [16*i+15 : 16*i].
wire [15:0] instr = imem[16*pc +: 16];

// ── Decode ────────────────────────────────────────────────────────────────
wire [3:0] op   = instr[15:12];
wire [2:0] rd   = instr[11:9];
wire [2:0] rs1  = instr[8:6];
wire [2:0] rs2  = instr[5:3];
wire [5:0] imm6 = instr[5:0];

// ── Register file reads ───────────────────────────────────────────────────
wire [7:0] va =   (rs1 == 3'd0) ? r0 : (rs1 == 3'd1) ? r1 :
                  (rs1 == 3'd2) ? r2 : (rs1 == 3'd3) ? r3 :
                  (rs1 == 3'd4) ? r4 : (rs1 == 3'd5) ? r5 :
                  (rs1 == 3'd6) ? r6 : r7;

wire [7:0] vb =   (rs2 == 3'd0) ? r0 : (rs2 == 3'd1) ? r1 :
                  (rs2 == 3'd2) ? r2 : (rs2 == 3'd3) ? r3 :
                  (rs2 == 3'd4) ? r4 : (rs2 == 3'd5) ? r5 :
                  (rs2 == 3'd6) ? r6 : r7;

wire [7:0] vd =   (rd  == 3'd0) ? r0 : (rd  == 3'd1) ? r1 :
                  (rd  == 3'd2) ? r2 : (rd  == 3'd3) ? r3 :
                  (rd  == 3'd4) ? r4 : (rd  == 3'd5) ? r5 :
                  (rd  == 3'd6) ? r6 : r7;          // source for STORE

// ── Effective address ─────────────────────────────────────────────────────
wire [7:0] ea = va + {2'b00, imm6};

// ── Data memory read ──────────────────────────────────────────────────────
wire [7:0] dmem_rdata =
    (ea == 8'd0)  ? dm0  : (ea == 8'd1)  ? dm1  :
    (ea == 8'd2)  ? dm2  : (ea == 8'd3)  ? dm3  :
    (ea == 8'd4)  ? dm4  : (ea == 8'd5)  ? dm5  :
    (ea == 8'd6)  ? dm6  : (ea == 8'd7)  ? dm7  :
    (ea == 8'd8)  ? dm8  : (ea == 8'd9)  ? dm9  :
    (ea == 8'd10) ? dm10 : (ea == 8'd11) ? dm11 :
    (ea == 8'd12) ? dm12 : (ea == 8'd13) ? dm13 :
    (ea == 8'd14) ? dm14 : (ea == 8'd15) ? dm15 :
    (ea == 8'd16) ? dm16 : (ea == 8'd17) ? dm17 :
    (ea == 8'd18) ? dm18 : (ea == 8'd19) ? dm19 :
    8'h00;

// ── ALU ───────────────────────────────────────────────────────────────────
wire [7:0] alu_out = (op == ADD) ? va + vb :
                     (op == SUB) ? va - vb :
                     (op == AND) ? va & vb :
                  /* op == OR  */ va | vb ;

// ── Writeback data / enables ──────────────────────────────────────────────
wire [7:0] rf_wdata = (op == LOAD) ? dmem_rdata : alu_out;
wire       rf_we    = (op == ADD || op == SUB ||
                       op == AND || op == OR  || op == LOAD);

// ── Sequential writeback ──────────────────────────────────────────────────
always @(posedge clk) begin
    if (rst) begin
        pc <= 0;
        r0 <= 0; r1 <= 0; r2 <= 0; r3 <= 0;
        r4 <= 0; r5 <= 0; r6 <= 0; r7 <= 0;
        dm0  <= 0; dm1  <= 0; dm2  <= 0; dm3  <= 0; dm4  <= 0;
        dm5  <= 0; dm6  <= 0; dm7  <= 0; dm8  <= 0; dm9  <= 0;
        dm10 <= 0; dm11 <= 0; dm12 <= 0; dm13 <= 0; dm14 <= 0;
        dm15 <= 0; dm16 <= 0; dm17 <= 0; dm18 <= 0; dm19 <= 0;
    end else begin
        pc <= pc + 1;

        // Register file write
        if (rf_we) begin
            case (rd)
                3'd0: r0 <= rf_wdata;  3'd1: r1 <= rf_wdata;
                3'd2: r2 <= rf_wdata;  3'd3: r3 <= rf_wdata;
                3'd4: r4 <= rf_wdata;  3'd5: r5 <= rf_wdata;
                3'd6: r6 <= rf_wdata;  3'd7: r7 <= rf_wdata;
            endcase
        end

        // Data memory write
        if (op == STORE) begin
            case (ea)
                8'd0:  dm0  <= vd;  8'd1:  dm1  <= vd;
                8'd2:  dm2  <= vd;  8'd3:  dm3  <= vd;
                8'd4:  dm4  <= vd;  8'd5:  dm5  <= vd;
                8'd6:  dm6  <= vd;  8'd7:  dm7  <= vd;
                8'd8:  dm8  <= vd;  8'd9:  dm9  <= vd;
                8'd10: dm10 <= vd;  8'd11: dm11 <= vd;
                8'd12: dm12 <= vd;  8'd13: dm13 <= vd;
                8'd14: dm14 <= vd;  8'd15: dm15 <= vd;
                8'd16: dm16 <= vd;  8'd17: dm17 <= vd;
                8'd18: dm18 <= vd;  8'd19: dm19 <= vd;
                // out-of-range writes silently ignored
            endcase
        end
    end
end

endmodule
