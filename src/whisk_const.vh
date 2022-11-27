// ============================================================================
// Whisk: a 16-bit bit-serial RISC processor (c) Luke Wren 2022
// Designed in a hurry for Tiny Tapeout 2
// SPDX-License-Identifier: Apache-2.0
// ============================================================================

localparam       WHISK_W_INSTR        = 16;

localparam W_DATA                     = 16;
localparam N_REGS                     = 6;

// Instruction layout
localparam       WHISK_INSTR_OP_LSB   = 0;
localparam       WHISK_INSTR_OP_MSB   = 3;
localparam       WHISK_INSTR_COND_LSB = 4;
localparam       WHISK_INSTR_COND_MSB = 6;
localparam       WHISK_INSTR_RT_LSB   = 7;
localparam       WHISK_INSTR_RT_MSB   = 9;
localparam       WHISK_INSTR_RS_LSB   = 10;
localparam       WHISK_INSTR_RS_MSB   = 12;
localparam       WHISK_INSTR_RD_LSB   = 13;
localparam       WHISK_INSTR_RD_MSB   = 15;

// Major opcodes (instr[3:0])
localparam [3:0] WHISK_OP_ADD         = 4'h0; // rd = rs + rt
localparam [3:0] WHISK_OP_SUB         = 4'h1; // rd = rs - rt
localparam [3:0] WHISK_OP_ANDN        = 4'h2; // rd = rs & ~rt
localparam [3:0] WHISK_OP_XOR         = 4'h3; // rd = rs ^ rt
localparam [3:0] WHISK_OP_SHIFT       = 4'h4; // Minor opcode in rt
localparam [3:0] WHISK_OP_INOUT       = 4'h6; // Minor opcode in rt

localparam [3:0] WHISK_OP_LDR         = 4'h8; //               rd = mem[rs]; 
localparam [3:0] WHISK_OP_LDR_IB      = 4'h9; // rs = rs + rt; rd = mem[rs];
localparam [3:0] WHISK_OP_LDR_DA      = 4'ha; //               rd = mem[rs]; rs = rs - rt
localparam [3:0] WHISK_OP_LDR_IB_DA   = 4'hb; // rs = rs + rt; rd = mem[rs]; rs = rs - rt

localparam [3:0] WHISK_OP_STR         = 4'hc; //               mem[rs] = rd; 
localparam [3:0] WHISK_OP_STR_IB      = 4'hd; // rs = rs + rt; mem[rs] = rd;
localparam [3:0] WHISK_OP_STR_DA      = 4'he; //               mem[rs] = rd; rs = rs - rt
localparam [3:0] WHISK_OP_STR_IB_DA   = 4'hf; // rs = rs + rt; mem[rs] = rd; rs = rs - rt

// Minor opcodes (rt)
localparam [2:0] WHISK_OP2_SRL = 3'h0;
localparam [2:0] WHISK_OP2_SRA = 3'h1;
localparam [2:0] WHISK_OP2_SLL = 3'h4;
localparam [2:0] WHISK_OP2_IN  = 3'h0;
localparam [2:0] WHISK_OP2_OUT = 3'h4;
