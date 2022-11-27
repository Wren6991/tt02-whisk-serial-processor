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

(*general braindump section*)

Instructions are 16-bit. Registers are 16-bit. Designed to execute from a serial SPI RAM in sequential mode (e.g. Microchip 23K256T-I). Hopefully with a gated through-path from `i[0]` to `o[0]` we will be able to drive SCK at the same frequency as the scan controller clkdiv (12 kHz / 2). Note that this SPI SRAM does not power up in sequential mode, but we'll need a host machine to load the initial program into SRAM anyway, so it seems reasonable to have the host flip the SRAM into sequential mode.

Each nonsequential access has an initial cost of 24 SCK cycles, plus some time to cycle the chip select. There is then a cost of one cycle per data bit for as long as your accesses remain sequential. A sequential halfword has less than 40% the cost of a nonsequential one, and a nonsequential byte has more than 80% the cost of a nonsequential halfword.

The aim is for most instructions to take 32 cycles: 16 to fetch the instruction (the fetch phase), and 16 to cycle through all bits of GPRs and PC (the execute phase). If the PC update is incremental, we fetch the next instruction immediately without having to issue a new SPI address. During the execute phase, we can read a 16-bit immediate following the instruction with no extra time cost, since this is a sequential fetch. There is no need to buffer this read, it can be muxed straight into the GPR read bus. For a TT2 scan refresh rate of 12 kHz, and an SPI clock of scan / 2, this means an execution speed of 187.5 instructions per second for reg+reg and reg+imm instructions. Hopefully they will move away from scanning for TT3, or at least allow some segmentation of the scan chain.

GPR cycling will be LSB-first, to propagate carries, so this implies the layout of bits in the SRAM will also be LSB-first in each byte. (This bit order convention is transparent to us, but host programs e.g. assemblers need to be aware of it.)

## Registers

The high cost of nonsequential accesses means using zero-page memory as registers would be quite slow, so we should allocate as much area as possible to the general-purpose register file. We probably do need a current instruction register of some kind, but other than that, avoid having any shift registers that are non-architectural. Everything is in/out of the GPRs, the PC, and SRAM, with no marshalling of data in between.

Aspirationally: 6x 16-bit registers, r0..r5 (approx 100 bits). Ideally these will be flops, but it's possible to drop down to latches if necessary, e.g. in groups of 4+1 where the last latch is sacrificial, at the cost of some execution speed.

Instructions are three-address-code, because we can afford the encoding space, and we are relatively register-poor so would like to avoid unnecessary clobbering. PC is available as register index 7, for reads/writes by any instruction.

Register index 6 for the destination or first operand indicates a hardwired zero register. Using a zero register as the destination is useful for generIt functions differently for the second operand: index 6 indicates a 16-bit literal following the instruction. (Of course there is nothing stopping you from executing your literals as instructions on different code paths, for bonus style points.)

## Memory Accesses

There will be some form of load/store instruction to move between memory and the register file. This is unavoidably a nonsequential SRAM access, and is followed by a nonsequential instruction fetch, so 16-bit loads/stores have a minimum cost of 16 + 24 + 16 + 24 = 80 cycles. (Fetch, load/store addr, load/store data, address of next fetch).

Addresses are issued to the SPI SRAM in MSB-first order, but we generally read the register file LSB-first so that we can propagate carries serially. If nonsequential accesses were fast, we could just accept a scramble of the address bits, but we are leaning heavily on fast sequential SPI transfers, so our addresses need to be genuinely sequential. I don't see any way around this other than allowing the GPR shifters to shift in both directions, possibly by abusing scan flops for the direction mux.

This bit order problem means we can't do addition on load/store addresses, without a separate addition. So our load/store instructions could just be `reg = mem[reg]` and `mem[reg] = reg`. This is pretty painful, because we now can't do stack frame accesses without clobbering a register, and those stack frame accesses are likely done to spill/fill a register in the first place! (I guess you would just adjust SP up/down around the access, so no actual clobber.) Alternatives I can see are:

* Add into the address GPR beforehand, then issue the address, then subtract back out of the address GPR
* Steal part of the CIR as an address register which we can add into and then issue from
* Allow just a few LSBs of the address to be taken from another register, so that aligned stack frames can be indexed within their alignment

Of the first two, the first is preferable, because there is no performance difference (the subtract can be done whilst issuing the next instruction address), it avoids extra flops, and it keeps CIR single-purpose. The second avoids the 16-cycle cost of the add, but is not all that useful.

A benefit of the built-in add is that by suppressing the fixup subtract, we get pre-incrementing versions of load/store, and by suppressing the initial add, we get post-decrementing versions. The increment and decrement are a full 16-bit operand, so these can also be used as pre-decrement and post-increment, which gives us all the useful increments for stack and array operations. Also by suppressing both, we can avoid the need to materialise a zero for the register-only variant.

So proposing:

LDR: `rs ?= rs + rt; rd = [rs]; rs ?= rs - rt` (where `?=` is an optional assignment)
STR: `rs ?= rs + rt; [rs] = rd; rs ?= rs - rt`

Execution time: 96 cycles or 80 cycles depending on whether the first add takes place. Note STR is using `rd` as an input operand, which is irregular, but since we're bit-serial the extra mux is cheap. We already have the ability to shift `rd`.

Caveat to all of this is that I need to examine the cell library and see if the area overhead of the two-shift-direction GPRs is greater than just having 16 D-latches for the address.

Another ugly thing I noticed when I started actually writing the logic was that when we have a post-decrement with an immediate operand, we need to read the immedate operand in for the post-decrement, as well as potentially reading it in for the pre-increment. This means restarting our sequential fetch one unit earlier, which is fine unless the instruction is a load into PC, in which case the re-fetched immediate will actually be the loaded PC value!

## Instructions

Each instruction has 9 bits for reg specifiers (MSBs), and the 7 for the opcode. The opcode will be the LSBs, so we have a chance to get a head start on the decode if it's profitable.

14 major opcodes:

* ADD  `rd = rs + rt`
* SUB  `rd = rs - rt`
* ANDN `rd = rs & ~rt`
* XOR  `rd = rs ^ rt` (performs NOT if immediate is all-ones)
* Shift (minor opcode in `rt`)
	* SRL `rd = rs >> 1`
	* SRA `rd = $signed(rs) >>> 1`
	* SLL `rd = rs << 1`
* In/out (minor opcode in `rt`):
	* OUT `outport = rs`
	* IN `rd = inport`
* LDR  `rs ?= rs + rt; rd = [rs]; rs ?= rs - rt` (4 variants)
* STR  `rs ?= rs + rt; [rs] = rd; rs ?= rs - rt` (4 variants)

Note for shifts we might forbid rd == rs, or forbid rd != rs, to avoid a special case. (Not sure which is more useless!)

No AND/OR. (I might regret this!) A lot of ANDs have constant masks, in which case the inversion doesn't matter.

No multi-bit shift. Due to the lack of shift enable on the register file, this would take 32 cycles: 16 to get shift amount to nearest multiple of 2 by dithering the left/right shift signal, then next 16 to shift by 1 by shifting through the ALU carry flop. Don't feel this is worth the control complexity, and we can save opcode space by putting the shifts on a minor opcode.

We have 14 instructions in the instruction bits 3:0, counting the 4 variants each of LDR/STR. 1/8th of the encoding space is reserved for Tiny Tapeout 3.

### Flags

Bits 6:4 of the instruction are used to encode condition codes. There are
three flags: N Z C (Negative, Zero, Carry). The carry is a true carry flag
(Arm-style). The eight conditions are:

* `000: al`: Always execute, and write flags (the default)
* `001: pr`: Always execute, but preserve flags
* `010: n `: Execute if negative
* `011: nn`: Execute if not negative
* `100: c `: Execute if carry
* `101: nc`: Execute if no carry
* `110: z `: Execute if zero
* `111: nz`: Execute if nonzero

(TODO: there are probably more useful combinations here, for signed comparisons etc, and not clear we can get away without a V flag. We can use double branches to test combinations, but equally we could use branch-over-jump to invert flags.)

(TODO: what value does each instruction write to the flags, for non-obvious cases)

Instructions with condition codes other than `al` do not update flags.

If a condition code is false, an instruction has no effect other than incrementing PC. The following immediate operand is still consumed, if there is one.

There is no branch instruction -- just do a conditional ADD on PC. Likewise there is no subroutine call -- just write PC + 4 to a register before jumping in.

### Pseudo-ops

```
mov rd, rs    -> add rd, zero, rs
not rd, rs    -> andn rd, zero, rs
cmp rs, rt    -> sub zero, rs, rt
ldi rd, #imm  -> add rd, zero, #imm
j label       -> add pc, pc, @label // Assembler calculates immediate offset to label
lda rd, label -> add rd, pc, @label // Assembler calculates immediate offset to label
push rd       -> strib rd, sp, -2   // Store with negative increment-before
pop rd        -> ldrda rd, sp, -2   // Load with negative decrement-after
ret           -> ldrda pc, sp, -2
```

### Execution Timings

* Condition codes are false: 16 cycles (no immediate operand) or 32 cycles (immediate operand)
* ADD, SUB, ANDN, XOR, SLL, SRL, rd is not PC: 32 cycles
* ADD, SUB, ANDN, XOR, SLL, SRL, rd is PC: 58 cycles
* LDR/STR with add or pre-increment/decrement: 98 cycles (STR) or 100 cycles (LDR), plus 16 cycles if an immediate operand is used for the post-subtract.
* LDR/STR with no add, or with post-increment/decrement: 82 cycles (STR) or 84 cycles (LDR), plus 16 cycles if an immediate operand is used for the post-subtract.
* IN/OUT: probably 32 cycles, TBD based on details of IO port.

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

* Run some placement density sanity tests with a big shift register to see how many flops I can squeeze into the project footprint
