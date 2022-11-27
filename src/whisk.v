// ============================================================================
// Whisk: a 16-bit bit-serial RISC processor (c) Luke Wren 2022
// Designed in a hurry for Tiny Tapeout 2
// SPDX-License-Identifier: Apache-2.0
// ============================================================================

`default_nettype none

// ============================================================================

// whisk_cpu: top-level for the Whisk processor, minus the IO wrapper and the
// SPI serdes

module whisk_cpu (
	input  wire       clk,
	input  wire       rst_n,

	output wire       spi_sck_en_next,
	output wire       spi_sdo_next,
	output wire       spi_csn_next,
	input  wire       spi_sdi_prev
);

`include "whisk_const.vh"

reg [15:0] instr;
reg [3:0]  bit_ctr;
reg [2:0]  state;
reg        instr_cond_true;
reg        instr_has_imm_operand;

// ----------------------------------------------------------------------------
// Main control state machine

wire [WHISK_INSTR_OPC_MSB -WHISK_INSTR_OPC_LSB :0] instr_opc  = instr[WHISK_INSTR_OPC_MSB  : WHISK_INSTR_OPC_LSB ];
wire [WHISK_INSTR_COND_MSB-WHISK_INSTR_COND_LSB:0] instr_cond = instr[WHISK_INSTR_COND_MSB : WHISK_INSTR_COND_LSB];
wire [WHISK_INSTR_RT_MSB  -WHISK_INSTR_RT_LSB  :0] instr_rt   = instr[WHISK_INSTR_RT_MSB   : WHISK_INSTR_RT_LSB  ];
wire [WHISK_INSTR_RS_MSB  -WHISK_INSTR_RS_LSB  :0] instr_rs   = instr[WHISK_INSTR_RS_MSB   : WHISK_INSTR_RS_LSB  ];
wire [WHISK_INSTR_RD_MSB  -WHISK_INSTR_RD_LSB  :0] instr_rd   = instr[WHISK_INSTR_RD_MSB   : WHISK_INSTR_RD_LSB  ];

localparam [2:0] S_FETCH      = 2'd0;
localparam [2:0] S_EXEC       = 2'd1;
localparam [2:0] S_PC_NONSEQ0 = 2'd2;
localparam [2:0] S_PC_NONSEQ1 = 2'd3;
localparam [2:0] S_LS_ADDR0   = 2'd4;
localparam [2:0] S_LS_ADDR1   = 2'd5;
localparam [2:0] S_LS_DATA    = 2'd6;

always @ (posedge clk or negedge rst_n) begin
	if (!rst_n) begin
		state <= S_PC_NONSEQ0;
		bit_ctr <= 4'h0;
	end else begin
		bit_ctr <= bit_ctr + 4'h1;
		case (state)
		S_FETCH: if (&bit_ctr) begin
			if (!instr_cond_true) begin
				// Dump it!
				state <= S_FETCH;
			end else if (instr_opc[3] && !instr_opc[0]) begin
				// Load/store with no preincrement, go straight to address state
				state <= S_LS_ADDR0;
				bit_ctr <= instr_opc[2] ? 4'h8 : 4'h6; 
			end else begin
				state <= S_EXEC;
			end
		end
		S_EXEC: if (&bit_ctr) begin
			if ((instr_opc[3]) begin
				state <= S_LS_ADDR0;
				bit_ctr <= instr_opc[2] ? 4'h8 : 4'h6; 
			end else if (instr_rd == 3'd7) begin
				state <= S_PC_NONSEQ0;
				bit_ctr <= 4'h6;
			end else begin
				state <= S_FETCH;
			end
		end
		S_PC_NONSEQ0: if (&bit_ctr) begin
			state <= S_PC_NONSEQ1;
		end
		S_PC_NONSEQ1: if (&bit_ctr) begin
			state <= S_FETCH;
		end
		S_LS_ADDR0: if (&bit_ctr) begin
			state <= S_LS_ADDR1;
		end
		S_LS_ADDR1: if (&bit_ctr) begin
			state <= S_LS_DATA;
		end
		S_LS_DATA: if (&bit_ctr) begin
			if (instr_opc[1]) begin
				state <= S_LS_POSTDEC;
			end else begin
				state <= S_PC_NONSEQ0;
				bit_ctr <= 4'h6;
			end
		end
// FIXME: LS with an immediate post-decrement needs to read the immediate in
// again for the decrement, so probably go LS_DATA -> PC_NONSEQx ->
// LS_POSTDEC!
		S_LS_POSTDEC: if (&bit_ctr) begin
			state <= S_PC_NONSEQ0;
			bit_ctr <= 4'h6;
		end
	end
end


// ----------------------------------------------------------------------------
// Instruction shifter and early decode

always @ (posedge clk) begin
	if (state == S_FETCH) begin
		instr <= {spi_sdi_prev, instr[15:1]};
	end
end

// Decode condition and imm operand flags as the instruction comes in, so we
// can use them to steer the state machine at the end of S_FETCH.

reg flag_c;
reg flag_z;
reg flag_n;

wire [3:0] condition_vector = {flag_z, flag_c, flag_n, 1'b1};

always @ (posedge clk or negedge rst_n) begin
	if (!rst_n) begin
		instr_has_imm_operand <= 1'b0;
		instr_cond_true <= 1'b0;
	end else if (instr_has_imm_operand && !instr_cond_true) begin
		// In this case we must be in S_FETCH. Hold instr_cond_true for an
		// additional fetch cycle so that the immediate operand is also
		// dumped, but clear the operand flag so we don't loop forever.
		if (&bit_ctr) begin
			instr_has_imm_operand <= 1'b0;
		end
	end else if (state == S_FETCH) begin
		if (bit_ctr == (WHISK_INSTR_RT_MSB + 1)) begin
			// Grab rt as it goes past (this is why rt is not the MSBs!)
			instr_has_imm_operand <= instr[W_INSTR-1 -: 3] == 3'd6;
		end
		if (bit_ctr == (WHISK_INSTR_COND_MSB + 1)) begin
			// Decode condition as it goes past
			instr_cond_true <= condition_vector[instr[W_INSTR-1 -: 2]] ^ instr[W_INSTR-3];
		end
	end
end

// ----------------------------------------------------------------------------
// Regfile instantiation and direction control

wire regfile_shift_l_nr;

wire reg_rd_ql;
wire reg_rd_qr;
wire reg_rd_wen;
wire reg_rd_d;

wire reg_rs_ql;
wire reg_rs_qr;

wire reg_rt_ql;
wire reg_rt_qr;

whisk_regfile #(
	.W (W_DATA),
	.N (N_REGS)
) regfile_u (
	.clk    (clk,
	.l_nr   (regfile_shift_l_nr),

	.rd     (instr_rd),
	.rd_ql  (reg_rd_ql),
	.rd_qr  (reg_rd_qr),
	.rd_wen (reg_rd_wen),
	.rd_d   (reg_rd_d),

	.rs     (instr_rs),
	.rs_ql  (reg_rs_ql),
	.rs_qr  (reg_rs_qr),

	.rt     (instr_rt),
	.rt_ql  (reg_rt_ql)
	.rt_qr  (reg_rt_qr)
);

// On every cycle, the GPRs are shifted or rotated either to the left or the
// right. There is no shift enable, because enables cost money.
//
// - In all but LS_ADDR0/LS_ADDR1, we shift to right, and qr (rightmost flop
//   in each register chain) is the output. This lets us propagate carries
//   serially.
//
// - Total shift amount through LS_ADDR0/LS_ADDR1 must be a multiple of 16, to
//   avoid permanently rotating a register!
//
// - Total shift amount at cycle n, mod 2, is always n mod 2. (The total shift
//   amount always increments or decrements.) Get around this by using qr/ql
//   outputs of regfile.
//
// - Loads: Read address MSB-first. LSB must be available on penultimate cycle
//   of LS_ADDR1, so load data is available first cycle of LS_DATA. Shift to
//   left for last 2 cycles of ADDR0 and first 14 cycles of ADDR1, output is
//   qr. Jiggle back and forth for remaining cycles to keep 2-cycle shift sum
//   at 0.
//
// - Stores: Read address MSB-first. LSB of address must be available on final
//   cycle of LS_ADDR1, store data follows immmediately in LS_DATA. Rotate
//   left for entirety of LS_ADDR1, use regfile ql as output.

wire instr_str = instr_opc[2];
assign regfile_shift_l_nr =
	state == S_LS_ADDR0 && !instr_str ? (&bit_ctr[3:1] ? 1'b1 : bit_ctr[0]) :
	state == S_LS_ADDR1 && !instr_str ? (&bit_ctr[3:1] ? bit_ctr[0] : 1'b1) :
	state == S_LS_ADDR1 &&  instr_str ? 1'b1                                : 1'b0;


// ----------------------------------------------------------------------------
// Program counter

wire alu_out;

wire pc_l_nr;
wire pc_dl;
wire pc_qr;
wire pc_dr;
wire pc_ql;

whisk_shiftreg_leftright #(
	.W (16)
) pc_u (
	.clk  (clk),
	.l_nr (pc_l_nr),
	.dl   (pc_dl),
	.qr   (pc_qr),
	.dr   (pc_dr),
	.ql   (pc_ql)
);

// PC is incremented by 2 during FETCH phase. (shift-to-right)

reg pc_ci;
wire pc_co, pc_sum;
assign {pc_co, pc_sum} = pc_qr + (~|bit_ctr[3:1] ? bit_ctr[0] : pc_ci);
always @ (posedge clk) begin
	pc_ci <= pc_co;
end

// Similar shift rules to register file shift rules for loads. LSB of addr
// must be available on q_r on the penultimate cycle of PC_NONSEQ1.
// FIXME: avoid pc rotation in shortened LS_ADDR0

wire pc_l_nr =
	state == S_PC_NONSEQ0 ? (&bit_ctr[3:1] ? 1'b1 : bit_ctr[0]) :
	state == S_PC_NONSEQ1 ? (&bit_ctr[3:1] ? bit_ctr[0] : 1'b1) : 1'b0;

assign pc_dr = pc_ql;

assign pc_dl =
	state == S_FETCH                    ? pc_sum  :
	state == S_EXEC && instr_rd == 3'd7 ? alu_out : pc_qr;


// ----------------------------------------------------------------------------
// ALU

wire op_s =
	instr_rs == 3'd7 ? pc_qr        :
	instr_rs == 3'd6 ? spi_sdi_prev : reg_rs_qr;

wire op_t =
	instr_rs == 3'd7 ? pc_qr        : reg_rt_qr;

reg alu_ci;
wire alu_co, alu_result;

// TODO shift instructions

assign {alu_co, alu_result} =
	state == S_LS_POSTDEC             ? op_s + !op_t + (~|bit_ctr ? 1'b1 : alu_ci) :
	// state != S_EXEC                       ?  -> don't care because there is a write enable
	(instr_opc & 4'h9) WHISK_OPC_LOAD_PREINC ? op_s +  op_t + (~|bit_ctr ? 1'b0 : alu_ci) :
	instr_opc == WHISK_OPC_ADD               ? op_s +  op_t + (~|bit_ctr ? 1'b0 : alu_ci) :
	instr_opc == WHISK_OPC_SUB               ? op_s + !op_t + (~|bit_ctr ? 1'b1 : alu_ci) :
	instr_opc == WHISK_OPC_ANDN              ? op_s && !op_t                              :
	instr_opc == WHISK_OPC_XOR               ? op_s ^ op_t                                : reg_rd_q;

wire update_flags = state == S_EXEC && instr_opc < WHISK_OPC_IN;

always @ (posedge clk) begin
	alu_ci <= alu_co;
	if (update_flags) begin
		flag_z <= (flag_z || ~|bit_ctr) && !alu_result;
		flag_n <= alu_result;
		flag_c <= alu_co;
	end
end

// ----------------------------------------------------------------------------
// SPI controls

// Deassert CSn before issuing a nonsequential address, only.
assign spi_csn_next =
	instr_cond_true && state == S_EXEC && &bit_ctr && instr_opc < WHISK_OPC_OUT && instr_rd == 3'd7;
	instr_cond_true && state == S_EXEC && &bit_ctr && instr_opc[3] ||
	instr_cond_true && state == S_FETCH && &bit_ctr && instr_opc[3] && !instr_opc[0] ||
	state == S_LS_DATA && &bit_ctr && !instr_opc[1] ||
	state == S_LS_POSTDEC;

// Pedal to the metal on SCK except when pulling CSn for a nonsequential
// access, or when executing an instruction with no immediate.
assign spi_sck_en_next = !(spi_csn_next || (
	state == (&bit_ctr[3:1] ? S_FETCH : S_EXEC) && !instr_has_imm_operand


localparam [15:0] SPI_INSTR_READ  = 16'h0003 << 6;
localparam [15:0] SPI_INSTR_WRITE = 16'h0002 << 8;

wire spi_sdo_ls_addr0 =
	instr_opc[2]  ? SPI_INSTR_WRITE[bit_ctr] :
	&bit_ctr[3:1] ? rs_qr                    : SPI_INSTR_READ[bit_ctr];

assign spi_sdo_next =
	state == S_PC_NONSEQ0 ? (&bit_ctr[3:1] ? pc_qr : SPI_INSTR_READ[bit_ctr]) :
	state == S_PC_NONSEQ1 ? pc_qr                                             :
	state == S_LS_ADDR0   ? spi_sdo_ls_addr0                                  :
	state == S_LS_ADDR1   ? (instr_opc[2] ? rs_ql : rs_qr)                    :
	state == S_LS_DATA    ? rd_qr                                             : 1'b0;

// ----------------------------------------------------------------------------
// Writeback

assign reg_rd_wen =
	state == S_EXEC ||
	(state == S_LS_DATA && !instr_opc[2]) ||
	state == S_LS_POSTDEC;

assign reg_rd_d = state == S_LS_DATA ? spi_sdi_prev : alu_result;

endmodule

// ============================================================================

// whisk_regfile: a register file of multiple shift registers, with 3 read
// ports (rd/rs/rt) and one write port (rd). No enable, so try to do things
// in multiples of 16 cycles. Registers not being written to are
// recirculated.
//
// qr is the value of the rightmost flop in a shift register (usually what you
// want when shifting out to right) and ql is the value of the leftmost flop
// in a shift register (usually what you want when shifting out to left).

module whisk_regfile #(
	parameter W = 16,
	parameter N = 6
) (
	input  wire                 clk,
	input  wire                 l_nr,

	input  wire [$clog2(N)-1:0] rd,
	output wire                 rd_ql,
	output wire                 rd_qr,
	input  wire                 rd_wen,
	input  wire                 rd_d,

	input  wire [$clog2(N)-1:0] rs,
	output wire                 rs_ql,
	output wire                 rs_qr,

	input  wire [$clog2(N)-1:0] rt,
	output wire                 rt_ql,
	output wire                 rt_qr
);

wire [N-1:0] dl;
wire [N-1:0] dr;
wire [N-1:0] ql;
wire [N-1:0] qr;

assign rd_ql = ql[rd];
assign rs_ql = ql[rs];
assign rt_ql = ql[rt];

assign rd_qr = qr[rd];
assign rs_qr = qr[rs];
assign rt_qr = qr[rt];

genvar g;
generate
for (g = 0; g < N; g = g + 1) begin: gpr_shifter

	// Recirculate unless this register is addressed as rd.
	assign dl[g] = rd_wen && rd == g ? rd_d : qr[g];
	assign dr[g] = rd_wen && rd == g ? rd_d : ql[g];

	whisk_shiftreg_leftright(
		.clk  (clk),
		.l_nr (l_nr),
		.dl   (dl[g]),
		.ql   (ql[g]),
		.ql   (ql[g]),
		.qr   (qr[g])
	);

end
endgenerate

endmodule

// ============================================================================

// whisk_shiftreg_leftright: a shift register that always shifts left or right
// each cycle.
//
// Note there is no enable because the underlying scan flops do not have an
// enable (there is an enable version, but it's larger, and more routing
// required!). If you don't want to shift, just shift back and forth for an
// even number of cycles, or do a full loop :) Shifting by an odd number of
// bits in an even number of cycles requires a delay flop to be patched in.
//
// dl and ql are the leftmost inputs and outputs. If l_nr is high (left), ql
// becomes dl on every posedge of clk.
//
// dr and qr are the rightmost inputs and outputs. If l_nr is low (right), qr
// becomes dr on every posedge of clk.

module whisk_shiftreg_leftright #(
	parameter W = 16
) (
	input  wire clk,
	input  wire l_nr,
	input  wire dl,
	output wire qr,
	input  wire dr,
	output wire ql
);

wire [W+1:0] chain_q;

assign chain_q[0    ] = dl;
assign chain_q[W + 1] = dr;

assign ql = chain_q[1];
assign qr = chain_q[W];

genvar g;
generate
for (g = 1; g < W + 1; g = g + 1) begin: shift_stage
	whisk_scanflop flop_u (
		.clk (clk),
		.sel (l_nr),
		.d   ({chain_q[g - 1], chain_q[g + 1]})
		.q   (chain_q[g])
	);
end
endgenerate

endmodule

// ============================================================================

// whisk_scanflop: a flop with a mux on its input. Usually reserved for DFT
// scan insertion, but we don't need that where we're going >:)

module whisk_scanflop (
	input  wire clk,
	input  wire sel,
	input  wire [1:0] d,
	output reg q
)

`ifdef SKY130

// (scanchain in TT2 uses sky130_fd_sc_hd__sdfxtp, a simple flop with scan
// mux. An enable version, sky130_fd_sc_hd__sedfxtp, is also available, but
// this is significantly larger. Instantiate the unit-drive version because
// we have a ridiculously long clock period; not sure whether the backend is
// allowed to change the drive.)

sky130_fd_sc_hd__sdfxtp_1 dff_u (
	.CLK        (clk),
	.D          (d[0]),
	.SCD        (d[1]),
	.SCE        (sel),
	.Q          (q),
	.VPWR       (1'b1),
	.VGND       (1'b0)
);

`else

// Synthesisable model

always @ (posedge clk) begin
	q <= d[sel];
end

`endif

endmodule

// TODO: add a reset version (sky130_fd_sc_hd__sdfrtn)

// ============================================================================

// whisk_spi_serdes: handle the timing of the SPI interface, and provide a
// slightly abstracted interface to the whisk core, with all signals on
// posedge of clk.

module whisk_spi_serdes(
	input  wire clk,
	input  wire rst_n,

	// Core
	input  wire sdo,
	input  wire sck_en,
	input  wire csn,
	output wire sdi,

	// IOs
	output wire padout_sck,
	output wire padout_csn,
	output wire padout_sdo,
	input  wire padin_sdi
);

// ----------------------------------------------------------------------------
// Output paths

reg sdo_r;
reg sck_en_r;
reg csn_r;

always @ (posedge clk or negedge rst_n) begin
	if (!rst_n) begin
		sdo_r <= 1'b0
		csn_r <= 1'b1;
		sck_en_r <= 1'b0;
	end else begin
		sdo_r <= sdo;
		csn_r <= csn;
		sck_en_r <= sck_en;
	end
end

assign padout_sdo = sdo_r;
assign padout_csn = csn_r;

// Through-path for clock input to clock output. No clock gating cell required
// as this is sampled by the scan flops at the tile output:
assign padout_sck = sck_en_r && !clk;

// ----------------------------------------------------------------------------
// Input paths

`ifdef SKY130

// ASIC version

// TODO find a suitable delay buffer cell for hold buffering, and decide how to
// dimension it against i[7:0] skew

// TODO find a suitable latch cell (possibly sky130_fd_sc_hd__dlxtp)

wire padin_sdi_delay;

delay_buf_dummy_find_a_cell (
	.i (padin_sdi),
	.z (padin_sdi_delay)
);

reg sdi_latch;

always @ (*) begin
	if (clk) begin
		sdi_latch <= padin_sdi_delay;
	end
end

assign sdi = sdi_latch;

`else

// Dodgy sim-only version

reg padin_sdi_reg;
always @ (negedge clk)
	padin_sdi_reg <= padin_sdi;
end

assign sdi = padin_sdi_reg;

`endif

endmodule
