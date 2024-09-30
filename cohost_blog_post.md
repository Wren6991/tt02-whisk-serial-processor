# Designing a terrible processor in 6 days

[Tiny Tapeout](https://tinytapeout.com/) is an annual-ish chance to get 100 x 100 um of your own 130 nm silicon manufactured and delivered to you on a circuit board. In classic ADHD fashion I started designing a processor 6 days before the deadline. The repository is here: https://github.com/Wren6991/tt02-whisk-serial-processor

Tiny Tapeout is interesting because it's so limited:

- Space for around 500 cells (where a cell is a flip-flop, a NAND gate, etc)
- 8 input pins
- 8 output pins

For a sense of scale, the attached picture is the final layout, with a single D flip-flop highlighted in green. The tools struggle with high placement density in small spaces, so this is actually rather full.

I wanted to build something "useful", which I arbitrarily define as meaning "this computer could run a self-hosting assembler and linker". (I have a truly marvellous demonstration of this, which this margin is too narrow to contain.) So, I set out with the following precepts:

- 16-bit processor, spiritually RISC-like
- Enough general purpose registers to not be a pain in the ass to write assembly for
- Bit-serial, so that most of the limited area can be spent on the register file
- Instructions and data unified in a single SPI SRAM (up to 64 kiB)
- Be reasonably good at position-independent code, for DOS-style multitasking
- For tough ISA decisions, always pick the funniest option

I was able to get the design finished, as well as a rudimentary assembler and simulator and some testcases, on the day of the deadline. I spent far too long futzing with the instruction set instead of focusing on important things like the timing of the SPI interface. So as to not break with that trend, I'll mostly talk about the instruction set in this post. Here are some highlights:

- 16-bit instructions, with optional 16-bit literal following each instruction
    - Same as the machine word size
    - Will thank myself for this when I'm writing a self-hosted assembler
- Load/store architecture
    - Pre/post-increment are available on 16-bit load/stores
    - Byte-addressed, with no alignment requirements since it's a serial SRAM
- 6 general purpose registers r0..r5, a zero register r6, and PC mapped as register r7
    - PC-relative loads and adds are great for PIC
    - All instructions can write PC (in fact a load with pre-increment can write PC *twice*)
    - Jumps are adds to PC, just like Sophie Wilson intended
    - Instructions are three-address-code (usually 2-read 1-write, though I do have 3-read stores and 2-write loads)
- Three flags: **Z**ero, **C**arry, **N**ot less than 32768
    - I do not believe in signed arithmetic, therefore it can't hurt me
    - Consequently there is no need for an O**V**erflow flag since this is simply blessed unsigned wrapping behaviour and not something that needs to be branched on
    - (Short instruction sequences do exist for signed comparisons)
- All instructions are conditional
    - 3-bit condition code, selecting any polarity of N/Z/C, plus always-execute
    - The 8th encoding slot is used for "always execute, but preserve flags". Less funny than "never execute" but sort of useful.
- Two-operand ALU operations: ADD SUB AND ANDN OR
- One-operand ALU operations: SLL SRL SRA ROR (the four shifts, by 1 bit each)
- IN and OUT instructions for GPIO through external shift registers

The rest of this post will be some deeper dives into the instruction set, with plenty of sidetracking into the implementation concerns that drove the decisions, then a conclusion about the whole experience. There is more complete documentation in the [project readme on GitHub.](https://github.com/Wren6991/tt02-whisk-serial-processor/blob/main/README.md)

### Instruction Format

There is a single, fixed 16-bit instruction format. For reasons that will become clear later, I *need* the instruction size to be a multiple of the register size. I also need to start incrementing PC before the instruction has been fetched, since the increment is bit-serial, and the PC needs to have some sensibly defined value by the time I start actually executing the instruction, so variable instruction width would be painful. The instructions look like this: 

```
|15 13|12 10|9   7|6         4|3            0|
| rd  | rs  | rt  | condition | major opcode |
```

`rd`/`rs`/`rt` are the destination operand and the two source operands. `condition` indicates what flags, if any, are required for this instruction to execute with side effects, as well as indicating whether the instruction sets flags. The four LSBs are the actual instruction opcode. I have more than 16 instructions total, but some of these only require one operand, so borrow the `rs` or `rt` bits for minor opcodes. 

One of the nice things about a bit-serial processor is the significant span of time between fetching the first and last bit of the instruction. The instruction fields are ordered to take advantage of this. For example, the second read operand `rt` is not at the end of the instruction, because the encoding `rt=7` indicates a 16-bit literal follows the instruction, which I need to know in advance so that I keep toggling the SPI SRAM's clock line. Similarly, the condition field is placed close to the LSBs.

### Register File

There are 6 16-bit general purpose registers, plus the program counter `pc`, which can be addressed as a GPR. The read value of `pc` is the address of the current instruction plus two, since it's incremented during instruction fetch.

Conventionally `r4` is the stack pointer (`sp`) and `r5` is the link register (`lr`), but there is nothing special about these registers in hardware. Four argument registers, a stack pointer and a return address register feels just barely tolerable to program. For example, the most important workload on hobby processors:

```
// Argument r0: index n (0-based)
// Clobber: r1, r2, r3
// Return r0: the nth Fibonacci number
fib:
	mov r1, #1                // A pseudo-op for add r1, zero, #1
	mov r2, #1
	add pc, pc, @fib_test    // Jump to fib_test
fib_loop:
	add r3, r1, r2
	mov r1, r2
	mov r2, r3
fib_test:
	sub r0, r0, #1            // carry set if sub did not wrap
	add.cs pc, pc, @fib_loop
	mov r0, r1
	mov pc, lr
```

To minimise the area spent on muxing and routing the register values, and for the smallest ALU, the registers are implemented as shift registers. You do not see the entire register at once, rather each cycle you see a different bit at one end of the register, and have the opportunity to either push a new bit in the other end, or just recirculate the existing value.

There's a free choice here for which direction to shift the register file in. The most natural order is LSB-first, since this is the direction that carries propagate in. MSB-first serial addition, by iteratively propagating the carries, is quadratic time in the worst case, and the control logic is more complex. On the other hand, the SPI SRAM main memory requires its address to be issued in MSB-first order, and there is no getting around this because I heavily rely on the addresses being sequential (not bit-scrambled) because sequential SPI accesses are so much faster than nonsequential ones. So, I need to be able to read out addresses in the *reverse order* to the addition.

There is a component in the SkyWater 130 nm library called a "scan flop", which is usually used for scan chain insertion as part of a design-for-test flow, but is really just a flop with an integrated 2:1 multiplexer. It's significantly smaller than a separate flop and mux. I experimented early on (3 days into the 6-day window) with using these scan flops to build bidirectional shift registers, i.e. registers that controllably shift either left or right by 1 every cycle. This means I can do my additions and `pc` increments LSB-first, but read out MSB-first. Note I really do mean shift left/right *every* cycle -- if you want to be able to stop in place for one cycle, that has a >20% area penalty because that library component (scan flop with enable) has an additional mux to recirculate the old value. If an instruction takes a non-multiple-of-16 number of cycles to execute, you have to "jiggle" the register in place by shifting alternately left and right, otherwise bit 0 will not be at position 0 when you next use the register's contents.

I did get the jiggly register file working, but the fact that the address addition for a register + register load had to be done *into* the base register (and then optionally reverted afterward) caused interesting edge cases when the base register or destination register was `pc`, since changing `pc` makes it impossible to re-load the immediate and revert the addition. Not being able to do `pop pc` is slightly annoying, but losing `pc`-relative loads would totally compromise my goal of making position-independent code reasonably efficient, so with 3 days remaining I abandoned the jiggly register file as a dead end.

In the end I implemented GPRs and `pc` with plain D flip-flops, less than 80% the area of scan flops, and used the recovered area to add a separate address register which is capable of shifting in both directions. The GPRs and `pc` shift right *every* cycle, but this is fine as long as everything takes multiples of 16 cycles. The address register captures the LSB-first addition from the ALU, then replays it in reverse to issue an MSB-first address to the SPI SRAM. I implemented the address register with scan flops, so some small part of the jiggly register file lives on.

### Flags

The N and Z flags are always equal to `result[15]` and `~|result` respectively (Verilog notation), but the C flag is far more interesting, since only add and subtract have a natural definition for it. There was much consultation with a friend about what would be the funniest carry flag for a load or store. An early leader was odd/even parity (respectively), and a likely latecomer was for load to give you the LSB of the next word after the loaded one, since this is the "next bit" in the same spirit as carry being the 16th bit of the addition result. Eventually we settled on loads setting the load carry flag to bit 7 of the result, since this can be used for byte sign extension with a conditional OR instruction like so:

```
    lh r0, [r1]      // Load into r0
    or.cs r0, 0xff00 // Set bits 15:8 if bit 7 was set
```

On the day before tapeout I rendered all of this discussion pointless by adding byte loads/stores to the architecture, including a sign-extending byte load. However, because I borrow the ALU's carry flop to propagate the sign, it happens that loads naturally set the carry to bit 7 of the result, so all is well with the world.

For bitwise operations, the carry flag is set if the result is all-ones. This is useful for testing for zero-bits using an inverted OR mask, in the same way that the zero flag is useful for testing for one-bits using an AND mask.

Shifts set the carry to the bit "shifted out of" the register, i.e. the prior MSB for left shifts, and the prior LSB for right shifts. This is useful for branching on LSB-first Huffman codes, or for putting the two MSBs of a register into two flags in one operation.

I spent a while trying to come up with a useful set of flags for stores that was approximately free to implement, but in the end I gave up and said that all flags are "undefined". This is actually not quite true, I know exactly what they do, it's just useless. The store flags are set to the same as the load flags, sampling the serial data coming back from the SPI, but this bus is high-impedance at this point since we're issuing a write command. Maybe I'll find a use for this with some IO device, pulling SDI high/low based on its status. Then I can call it a "feature".

Conditional instructions don't set flags. In future I would like to have a processor that is Turing-complete using only the flags, but this time I lacked the encoding space.

### Loads and Stores

6 of the 16 major opcodes are dedicated to halfword loads and stores. They're byte-addressed, and there are no alignment requirements since I'm accessing a serial memory. One slightly fun fact is that to maintain little-endianness on a bit-serial processor I have to shift data LSB-first, but SPI flash/SRAM are conventionally documented as being MSB-first, so before you load a `.bin` file into the computer you first have to bit-reverse each byte.

There are three variants each for halfword load/store: register plus register (`lh`/`sh`), pre-increment (`lhib`/`shib`), and post-increment (`lhia`/`shia`). Besides being useful for array operations, pre-increment and post-increment are useful for some stack idioms:

```
    shib rd, sp, -2 // push rd
    lhia rd, sp, 2  // pop rd
    lhia pc, sp, 2  // ret
```

This is quite a bit faster than a separate add and load instruction, because you avoid (serially!) reading in a separate instruction, and if the offset is not immediate then it's possible to issue the SPI command prefix whilst you perform the address addition. (This is not possible if there is an immediate, since the SPI bus is still occupied with reading in the immediate.)

At the last minute I added byte loads and stores, using the 4th 0-0 encoding combination of (use adder for address? / writeback addition to register file?). This combination would otherwise be useless because you can just form a register-addressed load from a register-plus-register load using the zero register.

As a quick example, here is memcpy:

```
// r0: dst, r1: src, r2: len, r3 is clobbered

memcpy_hword_loop:  // Copy halfwords until fewer than 2 bytes remain
    lhia r3, r1, #2 // Load with post-increment by 2
    shia r3, r0, #2 // Store with post-increment by 2
memcpy:             // <- Entry point!
    sub r2, r2, #2  // Carry set if subtraction does not wrap
    add.cs pc, pc, @memcpy_hword_loop

    tst r2, #1      // Odd number of bytes?
    lb.zc r3, r1    // If LSB nonzero, load a byte
    sb.zc r3, r0    // ...and then store it

    mov pc, lr
```

Note `tst` is a pseudo-op for `and` with the destination as the zero register. This runs at a blazing 160 cycles per byte:

- `lhia`/`shia` with immediate: 112 cycles each
- `sub`: 32 cycles
- `add` to `pc`: 64 cycles if taken

The lower bound is 96 cycles per byte, which you approach by caching the immediate `#2` in a register (avoiding a 16-cycle penalty on the load/store) and unrolling the halfword copy loop. Note most of this time is spent issuing nonsequential addresses to the SPI SRAM, which is why I lean as hard as possible on sequential accesses.

### Does it work?

Maybe. There are [some tests](https://github.com/Wren6991/tt02-whisk-serial-processor/tree/main/test). I'm more worried about the SPI timing due to the weird scan-chain IO setup on Tiny Tapeout 2, but if that fails then I can definitely bring up this processor on a small FPGA and run some real code on it.

### That's it lmao

This was a really fun project, I made a cute little instruction set which, though terrible in many ways, I could probably write a reasonable self-hosted assembler for.

This round of Tiny Tapeout is limited to a maximum clock frequency of 10 kHz or so, because the connection between the user projects and IO is through a single linear scan chain snaking through all the user designs. I'd like to bring up the processor I taped out, but in the mean time it would be much more practical to instead target a small FPGA like an iCE40 LP384. This is, as the academics say, "future work". I'd also like to tape out a revised version of this processor on a future Tiny Tapeout, which should have substantially (like >3 orders of magnitude) better IO performance.

If anyone read to the end of this post, thanks for sticking around. Even though it was a silly project I gave it my all for those 6 days. I signed up for cohost a few weeks ago and felt like it was time to break the silence; I'd like to post things like this again, but sporadically, and I'll be experimenting with the format and trying to find a voice.
