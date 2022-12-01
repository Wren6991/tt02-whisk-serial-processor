![](../../workflows/gds/badge.svg) ![](../../workflows/docs/badge.svg)


# Whisk: a 16-bit Serial RISC Processor

Whisk is a 16-bit bit-serial processor, designed in a hurry for TinyTapeout 2.

> TinyTapeout is an educational project that aims to make it easier and cheaper than ever to get your digital designs manufactured on a real chip!

> Go to https://tinytapeout.com for instructions!

## Goals

* A processor that can run real programs, with a useful amount of memory
	* Example of a "real program": a self-hosting assembler
	* 16 kiB absolute minimum
	* IO limitations mean this will be a serial memory (SPI), unified for instructions/data
	* To address this memory, internal area should be skewed towards registers rather than data path and muxing -> bit-serial implementation
* Enough performance for an interactive shell
	* Run SPI as fast as possible
	* Be limited by the serial memory bandwidth as much of the time as possible
	* Put at least a little bit of thought into the instruction set
* A minimal IO interface
	* Concrete goal: sufficient to bitbang an HD44780 display interface, plus an aux SPI to a host machine to run a terminal etc
	* Either a few bits that can be read/written directly by an instruction, or some trick to hang a shift register off of the serial memory bus to give single-instruction 16-bit IO.

## Direction

Instructions are 16-bit. Registers are 16-bit. Designed to execute from a serial SPI RAM in sequential mode (e.g. Microchip 23K256T-I). SCK is driven at the same frequency as the scan controller clkdiv (12 kHz / 2). The SPI SRAM does not power up in sequential mode, so the host will 

Each nonsequential access has an initial cost of 24 SCK cycles, plus some time to cycle the chip select. Each data bit then costs one SCK cycle, as long as addresses are sequential. Therefore we keep addresses sequential as much as possible.

Most instructions to take 32 cycles: 16 to fetch the instruction (the fetch phase), and 16 to cycle through all bits of GPRs and PC (the execute phase). If the PC update is incremental, we fetch the next instruction immediately without having to issue a new SPI address. During the execute phase, we can read a 16-bit immediate following the instruction with no extra time cost, since this is a sequential fetch. There is no need to buffer this read, it can be muxed straight into the GPR read bus. For a TT2 scan refresh rate of 12 kHz, and an SPI clock of scan / 2, this means an execution speed of 187.5 instructions per second for reg+reg and reg+imm instructions.

GPR cycling will be LSB-first, to propagate carries, so this implies the layout of bits in the SRAM will also be LSB-first in each byte. (This bit order convention is transparent to us, but host programs e.g. assemblers need to be aware of it.)

With a view to supporting larger programs or even simple operating systems in the future, we try to avoid any size or performance penalties for position-independent code. Loads and stores can be PC-relative with a full signed 16-bit displacement, and jumps and branches also have a full 16-bit offset from PC. We also try to make sure common stack idioms, like popping into PC to return from a function, map to single instructions if it does not compromise the performance or the control complexity.

> *Commentary on how I arrived at a decision, including failed design experiments, will be formatted like this paragraph, to keep it mostly separate from the factual description.*

## Registers

There are 6x 16-bit general-purpose registers, r0 through r5 (96 bits total). The program counter is also 16 bits, and can be read/written by any instruction, selected by register index r7. Reading the program counter returns the address of the current instruction plus two. To minimise cost per bit, there are no shift controls: the program counter and general purpose registers will rotate one bit to the right *every cycle*, and every operation takes a multiple of 16 cycles.

> *This means we use the simplest possible D flip-flops for our shift registers, instead of relying on scan flops for direction control, or enable flops for shift enable control. Clock gates are a low-area alternative for enabling/disabling the shift, but this design uses CXXRTL for simulation, which currently doesn't support gated clocks. In practice we lose 10 cycles per load and 12 cycles per store by making GPRs/PC completely uncontrolled.*

Non-architectural registers should be kept to a minimum: we probably do need a current instruction register, and a dedicated address register that can shift in two directions (LSB-first for serial-addition, then MSB-first to issue an address to the SPI) is just about worth it if it lets us make the GPRs and PC shift unidirectionally.

Instructions are three-address-code, because we can afford the encoding space, and we are relatively register-poor so would like to avoid unnecessary clobbering. PC is available as register index 7, for reads/writes by any instruction.

Register index r6 for the destination or first operand indicates a hardwired zero register. Using a zero register as the destination allows flags to be set without clobbering any registers, and there are some useful pseudo-ops with zero for the first operand. For the *second* operand, index r6 indicates a 16-bit literal following the instruction.

> *Of course there is nothing stopping you from executing your literals as instructions on different code paths, for bonus style points.*

## Instructions

Each instruction has 9 bits for reg specifiers (MSBs), and the 7 for the opcode.

The format of an instruction is:

```
|15 13|12 10|9   7|6         4|3            0|
| rd  | rs  | rt  | condition | major opcode |
```

> *The major opcode is the LSBs, so we have a chance to get a head start on the decode*

> *rt can't be on the far left because the processor needs to decode it before the instruction is completely fetched, to decide whether to issue more clocks for the immediate*

Of the 16 major opcodes, 15 are currently allocated:

* 0x0: ADD  `rd = rs + rt`
* 0x1: SUB  `rd = rs - rt`
* 0x2: AND  `rd = rs & rt`
* 0x3: ANDN `rd = ~rs & rt`
* 0x4: OR   `rd = rs | rt`
* 0x5: Shift (minor opcode in `rt`)
	* 0x0: SRL `rd = rs >> 1`
	* 0x1: SRA `rd = $signed(rs) >>> 1`
	* 0x4: SLL `rd = rs << 1`
* 0x6: In/out (minor opcode in `rt`):
	* 0x0: IN `rd = inport`
	* 0x4: OUT `outport = rs`
* 0x8 through 0xb: LD (4 variants, see "Memory Accesses")
* 0xc through 0xf: ST (4 variants, see "Memory Accesses")

> *No XOR. It's probably less common than clearing bits (ANDN), it can be synthesised with three instructions (ANDN, ANDN, OR) plus a register clobber. The use of XOR + imm to do NOT also works with ANDN. Since ANDN inverts its first operand, inverted registers can be ANDed with immediates; note there is no use in inverting immediates.*

> *No multi-bit shift. Efficient multi-bit shifts require control of the shifting of the register file. Even if the register file shift direction is controllable, this would take 32 cycles: 16 to get shift amount to nearest multiple of 2 by dithering the left/right shift signal, then next 16 to shift by 1 by shifting through the ALU carry flop. Don't feel this is worth the control complexity, and we can save opcode space by putting the shifts on a minor opcode.*

> *One of the 16 major opcodes is reserved for Tiny Tapeout 3, plus quite a few minor opcodes under Shift and In/out.*

Condition codes can be applied to any instruction, and may be true or false depending on the Z/C/V flag values at that point in the program (see "Flags"). Instructions with false condition codes have no side effects other than incrementing the program counter. Instructions with true condition codes execute the same as unconditional instructions, except that they do not update the flags.

### Jumps and Branches

Any instruction that writes to PC is effectively a jump instruction. After writing PC, the next instruction fetch is nonsequential, so a new address must be issued to SPI. For loads/stores, this is already the case, and there is no additional cost for writing PC. For all other instructions, there is a 32-cycle penalty for the nonsequential fetch:

* Cycles 0 through 15: Instruction is fetched as normal
* Cycles 16 through 31: Instruction executes as normal, writing to PC
* Cycles 32 through 47: Chip select is momentarily deasserted, the SPI command is issued. Simultaneously, PC is read LSB-first into the address register. The MSB of PC is issued directly to the address bus on cycle 47, without passing through the address register.
* Cycles 48 through 62: The address register shifts in reverse. Because the first PC bit already went to the bus last cycle, the *second flop* of the address register is forwarded to the bus. The last address bit is issued on cycle 62.
* Cycle 63: idle cycle to account for round trip time of SPI signals

> *The oddity of issuing the first address bit one cycle early, and then having to tap the second flop of the address register, is because the address register also needs to issue write addresses, which have different timing, because the write address directly abuts the write data on the SPI bus. This, plus the inflexibility of our 16-cycle register rotation, causes read addresses to have slightly odd timing.*

> *There are a few wasted cycles between cycle 32 and 47, and this is again due to our fixed 16-cycle rotation of the GPRs and PC.*

A branch is any jump instruction with a condition code other than `al` or `pr`. If the condition is true, it will execute as a jump instruction, and if the condition is false, the instruction is skipped.

### Flags

Bits 6:4 of the instruction are used to encode condition codes. There are
three flags: N Z C (Negative, Zero, Carry). The carry is a true carry flag
(Arm-style). The eight conditions are:

* `000: al`: Always execute, and write flags (the default)
* `001: pr`: Always execute, but preserve flags
* `010: ns`: Execute if negative
* `011: nc`: Execute if not negative
* `100: cs`: Execute if carry
* `101: cc`: Execute if no carry
* `110: zs`: Execute if zero
* `111: zc`: Execute if nonzero

(TODO: there are probably more useful combinations here, for signed comparisons etc, and not clear we can get away without a V flag. We can use double branches to test combinations, but equally we could use branch-over-jump to invert flags.)

(TODO: what value does each instruction write to the flags, for non-obvious cases)

Instructions with condition codes other than `al` do not update flags.

If a condition code is false, an instruction has no effect other than incrementing PC. The following immediate operand is still consumed, if there is one.

There is no branch instruction -- just do a conditional ADD on PC. Likewise there is no subroutine call -- just write PC + 4 to a register before jumping in.

#### Flag Results of Instructions

`Z` is always the NOR reduction of all bits of the result (or, for loads/stores, all bits of the data transferred to/from memory).

`N` is always the MSB of the result (or, for loads/stores, the MSB of the data transferred to/from memory).

`C` differs with the instruction:

* ADD/SUB carry into `C` from the sum (*not* a borrow)
* AND/ANDN/OR set `C` if and only if the result is all-ones
* SLL/SRL/SRA set `C` to the bit shifted out of the register
* Loads/stores carry into `C` from the sum of the two address operands, *even if the sum is not used for the actual load/store address*

### Memory Accesses

Load/store instructions move data between memory and the register file. This is a nonsequential SRAM access, followed by a nonsequential instruction fetch, so the best possible 16-bit load costs 16 + 1 + 24 + 1 + 16 + 24 + 1 = 84 cycles. (Fetch, pulse chip select high, load cmd + addr, sampling delay, load data, pulse chip select high, fetch cmd + addr, sampling delay.)

Addresses are issued to the SPI SRAM in MSB-first order, but we generally read the register file LSB-first so that we can propagate carries serially. If nonsequential accesses were fast, we could just accept a scramble of the address bits, but we are leaning heavily on fast sequential SPI transfers, so our addresses need to be genuinely sequential. We solve this with a dedicated address register, capable of shifting in both directions. First a register is read from the register file into the address register, optionally adding another register or an immediate as the data passes through the ALU. The address register captures this and then replays it in reverse.

> *Earlier versions of the design were instead able to reverse the shift direction of the register file and program counter. The lack of dedicated address register meant address addition had to be performed in-place in the source register, then reverted by subtraction. It was impossible to reload the immediate for subtraction if PC was used as the base address, or if the load was into PC. Replacing the scan flops in PC/GPRs with simple DFFs just about pays for the cost of a dedicated address register.*

The address register captures either the sum of the two operands, or just the first operand. There is also an option to write back the sum to the first operand register. This supports the following four cases:

| Address capture | Address writeback | Mnemonic      | Operation                                                            |
| --------------- | ----------------- | --------      | -------------------------------------------------------------------  |
| First operand   | None              | `ld`/`st`     | Load/store addressed by register                                     |
| First operand   | Sum of operands   | `ldia`/`stia` | Load/store with post-increment/decrement, e.g. stack pop             |
| Sum of operands | None              | `ld2`/`st2`   | Load/store addressed by register + register, or register + immediate |
| Sum of operands | Sum of operands   | `ldib`/`stib` | Load/store with pre-increment/decrement, e.g. stack push             |

If the same register is used for both address writeback and store data, the address writeback is visible to the store data.

The actual cost of a load/store is 96 cycles: the command + address sections are rounded up to 32 cycles to keep up with our fixed 16-cycle register rotation. The LSB-first register read and address addition can be done concurrently with the issue of the SPI command, so there is no additional penalty for these, *unless* the second operand is an immediate, in which case this can not be overlapped and there is an additional cost of 16 cycles.

#### Byte Accesses

We only have halfword (16-bit, register-sized) accesses, but these can be byte-aligned.

A byte load is:

```
	ld  rdata, raddr
	and rdata, rdata, #0xff
```

A byte store is:

```
	ld  rscratch, raddr
	and rscratch, rscratch, #0xff00
	or  rdata, rdata, rscratch
	st  rdata, raddr
```

This doesn't seem too unreasonable, but adding support for byte accesses would be fairly inexpensive:

* Byte loads would still load the full 16 bits, but mask or sign-extend into bits 15:8 of the result
* Byte stores would still shift out the full 16 bits, but would gate the SPI clock for the last 8 cycles

One suitable encoding hole for byte load/stores is the `rt` register specifier for non-incrementing non-decrementing loads, which don't use this operand. Push/pops are mostly going to be register values, so just having register-addressed byte loads/stores is probably acceptable.

Since byte accesses can easily be emulated with the existing instructions, this feels like future work.

### Pseudo-ops

Some useful operations that are subsets of machine instructions:

```
mov rd, rs    -> add rd, zero, rs      // Or rs can be #imm
ldi rd, #imm  -> add rd, zero, #imm
not rd, rs    -> andn rd, zero, rs
cmp rs, rt    -> sub zero, rs, rt      // Compare integers
tst rs, rt    -> and zero, rs, rt      // Test masked bits set
tstn rs, rt   -> andn zero, rs, rt     // Test unmasked bits set
j label       -> add pc, pc, @label    // Assembler calculates immediate offset to label
lda rd, label -> add rd, pc, @label    // Assembler calculates immediate offset to label
push rd       -> stib rd, sp, -2       // Store with negative increment-before
pop rd        -> ldia rd, sp,  2       // Load with positive increment-after
ret           -> ldia pc, sp,  2
nop           -> add.pr zero, zero, r0 // Doesn't write flags either
```

### Execution Timings

* Condition code false (skipped instruction), no immediate operand: 16 cycles
* Condition code false (skipped instruction), immediate operand: 32 cycles
* ADD, SUB, AND, ANDN, OR, SRL, SRA, SLL, IN, OUT, rd is not PC: 32 cycles
* ADD, SUB, AND, ANDN, OR, SRL, SRA, SLL, IN, OUT, rd is PC: 64 cycles
* Load/store with no immediate operand: 96 cycles
* Load/store with an immediate operand: 112 cycles

## Notes on SPI vs scan timings

References: https://github.com/TinyTapeout/tinytapeout-02/blob/tt02/verilog/rtl/scanchain/scanchain.v, https://github.com/TinyTapeout/tinytapeout-02/blob/tt02/verilog/rtl/scan_controller/scan_controller.v.

All designs in Tiny Tapeout 2 are connected in a single linear scan chain. Every full scan (approx 12 kHz), 8 input pads are sampled and latched into your design's 8 inputs, and your design's 8 outputs are sampled and registered into 8 output pads. 

The scan controller goes through the following states:

```
        ST_IDLE             = 0,    // Idle
        ST_IN_LOAD          = 1,    // Capture input
        ST_IN_SHIFT_LO      = 2,    // Shift input to design: lo-clk
        ST_IN_SHIFT_HI      = 3,    // Shift input to design: hi-clk
        ST_IN_LATCH_WAIT    = 4,    // Wait before latching
        ST_IN_LATCH         = 5,    // Latch
        ST_OUT_LOAD_PRE     = 6,    // Prepare load
        ST_OUT_LOAD         = 7,    // Clock once to load
        ST_OUT_LOAD_POST    = 8,    // Wait for load to be done
        ST_OUT_LOAD_CLR     = 9,    // Restore chain to shift mode
        ST_OUT_SHIFT_LO     = 10,   // Shift output to us: lo-clk
        ST_OUT_SHIFT_HI     = 11,   // Shift output to us: hi-clk
        ST_OUT_CAP_WAIT     = 12,   // Wait for capture
        ST_OUT_CAP          = 13;   // Capture to out local register
```

ST_IN_LATCH is the point where your design's inputs are latched from the scan chain, and ST_OUT_LOAD is the point where the design's outputs are clocked out into the scan chain. Because this involves toggling of global strobes, and the full-system STA is apparently quite limited, there are a number of cycles between these states -- seems like 11 is the default -- but the minimum is 1 cycle. Anyway this gives us at least two scan clock cycles between the inputs being latched and the outputs being registered, and *we are going to take advantage of this.*

Another point to note is how the IOs are handled at the scan controller end. The controller watches all of the output data go past, and when it sees the scan record captured from the currently active design, it captures this into a register for the output pads. At the same time, it is continuously sampling the input pads, and sending them back out to the scan chain. This means that your design's inputs for scan refresh _k_ + 1 are sampled into the scan chain at approximately the same time as your outputs from scan refresh _k_ are registered into the output pads (within a few cycles -- I didn't inspect the RTL too closely). [See here in the scan controller.](https://github.com/TinyTapeout/tinytapeout-02/blob/1ec215095fab93f8e66a090827728ea3b90d7406/verilog/rtl/scan_controller/scan_controller.v#L327) This is also important!

Design input 0 is special: the scan controller can toggle this every scan refresh cycle (or every _n_ refreshes) to provide a free-running clock for your design.

Ok, so this is the plan for "fastest possible SPI": run the design off of the free-running `slow_clk` on `io[0]`, and drive a gated version of this straight back out as the SPI SCK. SPI mode 0 (CPOL = 0 CPHA = 0) works like this:

* Falling edge of SCK (or falling edge of CSn): host and device both output data
* Rising edge of SCK: host and device both sample data

Since the data is sent centre-aligned over SCK's posedge, and our design is presumably built from posedge-clocked flops, SCK is for the most part *the inversion of* `io[0]`, so that we launch our SCK posedge one half-cycle after we launch our data. Great, for output, we just have to shift out data on every posedge, and enable SCK on cycles where we present new data.

For input, we have to take the scan delay into account. We want the sample coincident with our launch of SCK's rising edge onto the pads. Recalling that our inputs for scan refresh cycle _k_ + 1 are sampled from the input pads at approximately the same time our outputs for scan refresh cycle _k_ are registered onto the output pads, this means the serial input data from our capture point is latched into our design at the same time as the following rising edge of `io[0]`.

That was a lot of words but the upshot is this:

* Our design is clocked by free-running `slow_clk` on `i[0]`, toggling once per scan
* SCK is `io[0]` inverted, and gated by a posedge flop in our design.
	* This is an in2out path through our design with a max delay of two scan clocks
	* Can just be an AND gate, rather than a proper clock gate, because our output "clock" is sampled by the scan flops
* Serial output data is registered into a posedge output flop at the same time the corresponding SCK enable is registered
* Serial input data is available on the design's input latches simultaneous with the rising edge of `i[0]` following the edge where we registered SDO and the SCK enable

This is almost perfect, except the serial input is only valid for a single refresh cycle (half an `i[0]` period), and there are no constraints between the input latch Qs, nor can we insert constraints, since the scan chain is synthesised after our designs. Probably the answer is to transparently latch SDI when `i[0]` is high, and add a ton of delay buffers on SDI to minimise chance of hold violations when `i[0]` falls. We have a 80 us clock period so it's not like we're concerned about setup.

Alternatively I can look a bit closer at the sampling logic, and try to get the input sample to fall sufficiently far after the rising edge of SCK that the SDI input to my design is valid for two cycles, and I can just sample on the rising edge of `i[0]` like a normal person.

# TODO

* Write simulator
* Tests!
* Check polarity and timing of shift register strobes
* Fully understand timing of SPI input
* Add timing mux options to SPI input if things are still not fully clear
