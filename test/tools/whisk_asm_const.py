OPC_ADD          = 0x0
OPC_SUB          = 0x1
OPC_AND          = 0x2
OPC_ANDN         = 0x3
OPC_OR           = 0x4
OPC_SHIFT        = 0x5
OPC_INOUT        = 0x6
OPC_LD           = 0x8
OPC_LDIA         = 0x9
OPC_LD2          = 0xa
OPC_LDIB         = 0xb
OPC_ST           = 0xc
OPC_STIA         = 0xd
OPC_ST2          = 0xe
OPC_STIB         = 0xf

OPC2_SRL         = 0x0
OPC2_SRA         = 0x1
OPC2_SLL         = 0x4

OPC2_IN          = 0x0
OPC2_OUT         = 0x4

INSTR_OPC_LSB    = 0
INSTR_OPC_MASK   = 0x000f
INSTR_COND_LSB   = 4
INSTR_COND_MASK  = 0x0070
INSTR_RT_LSB     = 7
INSTR_RT_MASK    = 0x0380
INSTR_RS_LSB     = 10
INSTR_RS_MASK    = 0x1c00
INSTR_RD_LSB     = 13
INSTR_RD_MASK    = 0xe000

OP_MASK_LDST     = 0x8
OP_MASK_ST       = 0x4
OP_MASK_LDST_AA  = 0x2
OP_MASK_LDST_AR  = 0x1