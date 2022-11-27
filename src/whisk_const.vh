// ============================================================================
// Whisk: a 16-bit bit-serial RISC processor (c) Luke Wren 2022
// Designed in a hurry for Tiny Tapeout 2
// SPDX-License-Identifier: Apache-2.0
// ============================================================================

localparam       WHISK_W_INSTR                = 16;

localparam W_DATA                             = 16;
localparam N_REGS                             = 6;

// Instruction layout
localparam       WHISK_INSTR_OPC_LSB          = 0;
localparam       WHISK_INSTR_OPC_MSB          = 3;
localparam       WHISK_INSTR_COND_LSB         = 4;
localparam       WHISK_INSTR_COND_MSB         = 6;
localparam       WHISK_INSTR_RT_LSB           = 7;
localparam       WHISK_INSTR_RT_MSB           = 9;
localparam       WHISK_INSTR_RS_LSB           = 10;
localparam       WHISK_INSTR_RS_MSB           = 12;
localparam       WHISK_INSTR_RD_LSB           = 13;
localparam       WHISK_INSTR_RD_MSB           = 15;

// Opcodes
localparam [3:0] WHISK_OPC_ADD                = 4'h0; // rd = rs + rt
localparam [3:0] WHISK_OPC_SUB                = 4'h1; // rd = rs - rt
localparam [3:0] WHISK_OPC_ANDN               = 4'h2; // rd = rs & ~rt
localparam [3:0] WHISK_OPC_XOR                = 4'h3; // rd = rs ^ rt
localparam [3:0] WHISK_OPC_SLL                = 4'h4; // rd = rs << (rt & 0xf)
localparam [3:0] WHISK_OPC_SRL                = 4'h5; // rd = rs >> (rt & 0xf)
localparam [3:0] WHISK_OPC_IN                 = 4'h6; // d = iport
localparam [3:0] WHISK_OPC_OUT                = 4'h7; // oport = rs
localparam [3:0] WHISK_OPC_LDR                = 4'h8; //               rd = mem[rs]; 
localparam [3:0] WHISK_OPC_LDR_PREINC         = 4'h9; // rs = rs + rt; rd = mem[rs];
localparam [3:0] WHISK_OPC_LDR_POSTDEC        = 4'ha; //               rd = mem[rs]; rs = rs - rt
localparam [3:0] WHISK_OPC_LDR_PREINC_POSTDEC = 4'hb; // rs = rs + rt; rd = mem[rs]; rs = rs - rt
localparam [3:0] WHISK_OPC_STR                = 4'hc; //               mem[rs] = rd; 
localparam [3:0] WHISK_OPC_STR_PREINC         = 4'hd; // rs = rs + rt; mem[rs] = rd;
localparam [3:0] WHISK_OPC_STR_POSTDEC        = 4'he; //               mem[rs] = rd; rs = rs - rt
localparam [3:0] WHISK_OPC_STR_PREINC_POSTDEC = 4'hf; // rs = rs + rt; mem[rs] = rd; rs = rs - rt

// Condition codes
localparam [2:0] WHISK_COND_A                 = 3'h0;
localparam [2:0] WHISK_COND_NA                = 3'h1; // reserved I guess? TODO
localparam [2:0] WHISK_COND_N                 = 3'h2;
localparam [2:0] WHISK_COND_NN                = 3'h3;
localparam [2:0] WHISK_COND_C                 = 3'h4;
localparam [2:0] WHISK_COND_NC                = 3'h5;
localparam [2:0] WHISK_COND_Z                 = 3'h6;
localparam [2:0] WHISK_COND_NZ                = 3'h7;
