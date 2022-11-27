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

	// SPI SRAM interface
	output wire       mem_sck_en_next,
	output wire       mem_sdo_next,
	output wire       mem_csn_next,
	input  wire       mem_sdi_prev,

	// Shift registers for IO port
	output wire       ioport_sck_en_next,
	output wire       ioport_sdo_next,
	input  wire       ioport_sdi_prev,
	output wire       ioport_latch_i_next,
	output wire       ioport_latch_o_next
);

`include "whisk_const.vh"

reg [15:0] instr;

wire [WHISK_INSTR_OP_MSB  -WHISK_INSTR_OP_LSB  :0] instr_op;
wire [WHISK_INSTR_COND_MSB-WHISK_INSTR_COND_LSB:0] instr_cond;
wire [WHISK_INSTR_RT_MSB  -WHISK_INSTR_RT_LSB  :0] instr_rt;
wire [WHISK_INSTR_RS_MSB  -WHISK_INSTR_RS_LSB  :0] instr_rs;
wire [WHISK_INSTR_RD_MSB  -WHISK_INSTR_RD_LSB  :0] instr_rd;

assign {instr_rd, instr_rs, instr_rt, instr_cond, instr_op} = instr;

wire instr_op_ls     = instr_op[3]; // Whether an instruction is a ldr/str
wire instr_op_st_nld = instr_op[2]; // Whether a ldr/str is a load or store
wire instr_op_ls_da  = instr_op[1]; // Whether a ldr/str has decrement-after
wire instr_op_ls_ib  = instr_op[0]; // Whether a ldr/str has increment-before

// ----------------------------------------------------------------------------
// Main control state machine

reg [3:0]  bit_ctr;
reg [2:0]  state;
reg        instr_cond_true;
reg        instr_has_imm_operand;

// Note there is a 2 cycle delay from issuing a bit on SDO to getting a bit
// back on SDI. This is handled with a 2-cycle stall after issuing a read
// address, so that e.g. S_FETCH always has the first instruction bit
// available on the first cycle.

localparam [2:0] S_FETCH      = 2'd0; // Sample 16 instr bits, increment PC
localparam [2:0] S_EXEC       = 2'd1; // Loop all GPRs, write one GPR
localparam [2:0] S_PC_NONSEQ0 = 2'd2; // Issue cmd, then issue 2 PC bits
localparam [2:0] S_PC_NONSEQ1 = 2'd3; // Issue rest of PC bits, stall 2 cycles
localparam [2:0] S_LS_ADDR0   = 2'd4; // Issue cmd; if load, issue 2 addr bits
localparam [2:0] S_LS_ADDR1   = 2'd5; // Issue addr; if load, stall 2 cycles
localparam [2:0] S_LS_DATA    = 2'd6; // Issue store data, or sample load data
localparam [2:0] S_LS_IMMPD   = 2'd7; // Re-read imm for imm post-decrement

reg [3:0] bit_ctr_nxt;
reg [2:0] state_nxt_wrap;
reg [2:0] state_nxt;

always @ (*) begin
	bit_ctr_nxt = bit_ctr + 4'h1;
	state_nxt_wrap = state;
	case (state)
		S_FETCH: begin
			if (!instr_cond_true) begin
				// Dump it!
				state_nxt_wrap = S_FETCH;
			end else if (instr_op_ls && !instr_op_ls_ib) begin
				// Load/store with no preincrement, go straight to address state
				state_nxt_wrap = S_LS_ADDR0;
				bit_ctr_nxt = instr_op_st_nld ? 4'h8 : 4'h6;
			end else begin
				state_nxt_wrap = S_EXEC;
			end
		end
		S_EXEC: begin
			if (instr_op_ls) begin
				state_nxt_wrap = S_LS_ADDR0;
				bit_ctr_nxt = instr_op_st_nld ? 4'h8 : 4'h6;
			end else if (instr_rd == 3'd7) begin
				state_nxt_wrap = S_PC_NONSEQ0;
				bit_ctr_nxt = 4'h6;
			end else begin
				state_nxt_wrap = S_FETCH;
			end
		end
		S_PC_NONSEQ0: begin
			state_nxt_wrap = S_PC_NONSEQ1;
		end
		S_PC_NONSEQ1: begin
			if (!instr_cond_true) begin
				// Have just been reset, instr is invalid
				state_nxt_wrap = S_FETCH;
			end else if (instr_has_imm_operand && instr_op_ls && instr_op_ls_da) begin
				state_nxt_wrap = S_LS_IMMPD;
			end else begin
				state_nxt_wrap = S_FETCH;
			end
		end
		S_LS_ADDR0: begin
			state_nxt_wrap = S_LS_ADDR1;
		end
		S_LS_ADDR1: begin
			state_nxt_wrap = S_LS_DATA;
		end
		S_LS_DATA: begin
			state_nxt_wrap = S_PC_NONSEQ0;
			bit_ctr_nxt = 4'h6;
		end
		S_LS_IMMPD: begin
			state_nxt = S_PC_NONSEQ0;
			bit_ctr_nxt = 4'h6;
		end
	endcase
	state_nxt = &bit_ctr ? state_nxt_wrap : state;
end

// Start of day:
//
// - The only resettable flops are state, bit_ctr, and instr_cond_true.
//
// - We reset state/bit_ctr to a nonsequential fetch, and reset
//   instr_cond_true=0 (usually unreachable)
//
// - instr_cond_true=0 masks the fetch address to 0, regardless of PC
//
// - The first instruction must be `add pc, zero, #4` to initialise PC
//
// - You may then want to clear all the GPRs, though it's not necessary as
//   they will always be written before first read.

always @ (posedge clk or negedge rst_n) begin
	if (!rst_n) begin
		state <= S_PC_NONSEQ0;
		bit_ctr <= 4'h5;
	end else begin
		state <= state_nxt;
		bit_ctr <= bit_ctr_nxt;
	end
end

// ----------------------------------------------------------------------------
// Instruction shifter and early decode

always @ (posedge clk) begin
	if (state == S_FETCH) begin
		instr <= {mem_sdi_prev, instr[15:1]};
	end
end

// Decode condition and imm operand flags as the instruction comes in, so we
// can use them to steer the state machine at the end of S_FETCH.

reg instr_has_imm_operand_nxt;
reg instr_cond_true_nxt;

// From ALU:
wire [7:0] condition_vec8

always @ (*) begin
	instr_has_imm_operand_nxt = instr_has_imm_operand;
	instr_cond_true_nxt = instr_cond_true;

	if (instr_has_imm_operand && !instr_cond_true) begin
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
			instr_cond_true <= condition_vec8[instr[W_INSTR-1 -: 3]];
		end
	end
end

// instr_cond_true must reset to 0, because we use it to recognise the first
// fetch after reset. We don't care about instr_has_imm_operand, because it
// is initialised during S_FETCH before first use.

always @ (posedge clk or negedge rst_n) begin
	if (!rst_n) begin
		instr_cond_true <= 1'b0;
	end else begin
		instr_cond_true <= instr_cond_true_nxt;
	end
end

always @ (posedge clk) begin
	instr_has_imm_operand <= instr_has_imm_operand_nxt;
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
// - Normally we shift to right, and qr (rightmost flop in each register
//   chain) is the output. This lets us propagate carries serially.
//   Exceptions are: EXEC (instr: SRL/SRA only), LS_ADDR0 and LS_ADDR1.
//
// - For EXEC of SRL/SRA we reverse the GPR rotation to get the opposite shift
//   direction from a SLL. (See signal: alu_shift)
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

wire instr_is_right_shift = instr_op == WHISK_OP_SHIFT && !instr_rt[2];

assign regfile_shift_l_nr =
	state == S_EXEC && instr_is_right_shift ? 1'b1                                :
	state == S_LS_ADDR0 && !instr_op_st_nld ? (&bit_ctr[3:1] ? 1'b1 : bit_ctr[0]) :
	state == S_LS_ADDR1 && !instr_op_st_nld ? (&bit_ctr[3:1] ? bit_ctr[0] : 1'b1) :
	state == S_LS_ADDR1 &&  instr_op_st_nld ? 1'b1                                : 1'b0;

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

// We increment PC at the following times, noting that at the beginning of
// S_FETCH we do not know whether the instruction has an immediate, or
// whether its condition is true:
//
// - S_FETCH: +2 (Note: if there is an immediate, and cond is false, we go
//   through S_FETCH twice to dump the immediate, so +4 total).
//
// - S_EXEC: +2 if there is an immediate, UNLESS instruction is a load/store
//   with post-decrement.
//
// - S_LS_IMMPD: +2 (only reachable for load/store with immediate
//   post-decrement). Note: these instructions need special handling because
//   they fetch the immediate twice, so PC needs to point to the immediate
//   after S_EXEC.

wire pc_increment =
	state == S_FETCH ||
	state == S_EXEC && !(instr_has_imm_operand && instr_op_ls && instr_op_ls_da) ||
	state == S_LS_IMMPD;

reg pc_ci;
wire pc_co, pc_sum;

assign {pc_co, pc_sum} = pc_qr + (~|bit_ctr[3:1] ? bit_ctr[0] && pc_increment : pc_ci);

always @ (posedge clk) begin
	pc_ci <= pc_co;
end

// Similar shift rules to register file shift rules for loads. LSB of addr is
// available on q_r on the penultimate cycle of PC_NONSEQ1. Also jiggle the
// PC during LS_ADDR0, as this state is not 16 cycles long, and we don't want
// to permanently rotate the PC.

wire pc_l_nr =
	state == S_PC_NONSEQ0 ? (&bit_ctr[3:1] ? 1'b1 : bit_ctr[0]) :
	state == S_PC_NONSEQ1 ? (&bit_ctr[3:1] ? bit_ctr[0] : 1'b1) :
	state == S_LS_ADDR0   ? bit_ctr[0]                          : 1'b0;

assign pc_dr = pc_ql;

assign pc_dl =
	state == S_FETCH                                           ? pc_sum       :
	state == S_EXEC    && instr_rd != 3'd7                     ? pc_sum       :
	state == S_EXEC    && instr_rd == 3'd7                     ? alu_out      :
	state == S_LS_DATA && instr_rd == 3'd7 && !instr_op_st_nld ? mem_sdi_prev :
	state == S_LS_IMMPD                                        ? pc_sum       : pc_qr;


// ----------------------------------------------------------------------------
// ALU

wire op_s =
	instr_rs == 3'd7 ? pc_qr        :
	instr_rs == 3'd6 ? mem_sdi_prev : reg_rs_qr;

wire op_t =
	instr_rs == 3'd7 ? pc_qr        : reg_rt_qr;

reg alu_ci;
wire [1:0] alu_add = op_s +  op_t + (~|bit_ctr ? 1'b0 : alu_ci);
wire [1:0] alu_sub = op_s + !op_t + (~|bit_ctr ? 1'b1 : alu_ci);

// Shift uses the ALU carry flop as a 1-cycle delay. SRL/SRA rotate the
// regfile to the left, SLL rotates the regfile to the right, and the delay
// produces a shift opposite to the regfile's rotation.

wire [1:0] alu_shift = {
	instr_is_right_shift ? reg_rs_qr : reg_rs_ql,
	|bit_ctr ? alu_ci : reg_rs_ql && instr_rt[0]
};

wire ls_early_postdec = state == S_PC_NONSEQ1 && instr_op_ls &&
	instr_op_ls_da && !instr_has_imm_operand;

wire alu_co, alu_result;
assign {alu_co, alu_result} =
	state == S_LS_IMMPD           ? alu_sub           :
	ls_early_postdec              ? alu_sub           :
	// state == S_EXEC:
	instr_op_ls && instr_op_ls_ib ? alu_add           :
	instr_op == WHISK_OP_ADD      ? alu_add           :
	instr_op == WHISK_OP_SUB      ? alu_sub           :
	instr_op == WHISK_OP_ANDN     ? op_s && !op_t     :
	instr_op == WHISK_OP_XOR      ? op_s ^ op_t       :
	instr_op == WHISK_OP_SHIFT    ? alu_shift         :
	instr_op == WHISK_OP_INOUT    ? ioport_sdi_prev   : reg_rd_qr;

always @ (posedge clk) begin
	alu_ci <= alu_co;
end

// ----------------------------------------------------------------------------
// Flags

reg flag_z;
reg flag_c;
reg flag_n;

wire update_flags = (state == S_EXEC || state == S_LS_DATA) && ~|instr_cond;

// TODO sensible flags for load/store
always @ (posedge clk) begin
	if (update_flags) begin
		flag_z <= (flag_z || ~|bit_ctr) && !alu_result;
		flag_n <= alu_result;
		flag_c <= alu_co;
	end
end

assign condition_vec8 = {
	!flag_z, flag_z,
	!flag_c, flag_c,
	!flag_n, flag_n,
	1'b1,    1'b1
};

// ----------------------------------------------------------------------------
// Memory SPI controls

// Deassert CSn before issuing a nonsequential address, only.
assign mem_csn_next =
	&bit_ctr && state_nxt_wrap == S_PC_NONSEQ0 ||
	&bit_ctr && state_nxt_wrap == S_LS_ADDR;

// Pedal to the metal on SCK except when pulling CSn for a nonsequential
// access, or when executing an instruction with no immediate.
assign mem_sck_en_next = !(
	mem_csn_next ||
	state == (&bit_ctr[3:1] ? S_FETCH : S_EXEC) && !instr_has_imm_operand
);

// ldr issues addresses two cycles earlier than str, due to in->out delay.
localparam [15:0] SPI_INSTR_READ  = 16'h0003 << 6;
localparam [15:0] SPI_INSTR_WRITE = 16'h0002 << 8;

wire mem_sdo_ls_addr0 =
	instr_op_st_nld ? SPI_INSTR_WRITE[bit_ctr] :
	&bit_ctr[3:1]   ? rs_qr                    : SPI_INSTR_READ[bit_ctr];

assign mem_sdo_next =
	state == S_PC_NONSEQ0 ? (&bit_ctr[3:1] ? pc_qr : SPI_INSTR_READ[bit_ctr]) :
	state == S_PC_NONSEQ1 ? pc_qr                                             :
	state == S_LS_ADDR0   ? mem_sdo_ls_addr0                                  :
	state == S_LS_ADDR1   ? (instr_op_st_nld ? rs_ql : rs_qr)                 :
	state == S_LS_DATA    ? rd_qr                                             : 1'b0;

// ----------------------------------------------------------------------------
// Writeback

assign reg_rd_wen =
	state == S_EXEC ||
	(state == S_LS_DATA && !instr_op_st_nld) ||
	state == S_PC_NONSEQ1 && ls_early_postdec ||
	state == S_LS_IMMPD;

assign reg_rd_d = state == S_LS_DATA ? mem_sdi_prev : alu_result;

// ----------------------------------------------------------------------------
// IO port

// Expected IO setup is a 1x 8-bit PISO shift register for input, and 2x 8-bit
// SIPO shift registers for output:
//
// - IN: A latch_i pulse, then 8 clocks, sampling 8 data bits. Input is in the
//   8 MSBs of the destination register (sorry!), garbage in LSBs.
//
// - OUT: 15 clocks with data, then a latch_o pulse. Only 15 clocks to avoid
//   an additional flop for delaying latch_o, so only 15 bits are usable.
//
// The IN interface is still driven when executing an OUT, with more clocks.
// Abusable for a few extra inputs with another PISO shift register.
//
// Some data is still clocked out on an IN, there's just no latch_o pulse.
// Abusable to drive longer SIPO chains using multiple INs and a final OUT.

wire do_io_instr = state == S_EXEC && instr_op == WHISK_OP_INOUT;

wire io_instr_out = (instr_rt & (WHISK_OP2_OUT | WHISK_OP2_IN)) == WHISK_OP2_OUT;

assign ioport_latch_i_next = do_io_instr && ~|bit_ctr;
assign ioport_latch_o_next = do_io_instr && io_instr_out && &bit_ctr;

assign ioport_sck_en_next  = do_io_instr && (
	(bit_ctr >= 4'h6 && bit_ctr < 4'he) ||
	(io_instr_out && ~&bit_ctr)
);

assign ioport_sdo_next = do_io_instr && rs_ql;

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
//
// Out-of-range indices read as 0, and ignore writes.

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

localparam N_PADDED = 1 << $clog2(N);

wire [N-1:0]        dl;
wire [N-1:0]        dr;
wire [N_PADDED-1:0] ql;
wire [N_PADDED-1:0] qr;

assign rd_ql = ql[rd];
assign rs_ql = ql[rs];
assign rt_ql = ql[rt];

assign rd_qr = qr[rd];
assign rs_qr = qr[rs];
assign rt_qr = qr[rt];

genvar g;
generate
for (g = 0; g < N_PADDED; g = g + 1) begin: loop_gprs
	if (g >= N) begin: gpr_tieoff

		assign ql[g] = 1'b0;
		assign qr[g] = 1'b0;

	end else begin: gpr_shifter

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

// Through-path for clock input to SCK output. TODO clock gating cell
// required? This is sampled by the scan flops at the tile output.
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
