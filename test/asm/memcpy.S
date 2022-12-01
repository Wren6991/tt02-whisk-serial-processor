// Mem copy test program

entry:
	mov pc, #4

	add r1, pc, @src_buf
	add r0, pc, @dst_buf
	mov r2, #8
	add lr, pc, #6
	add pc, pc, @memcpy

	out r0
	out r1
	out r2
	out lr

	add r1, r1, #1
	add r0, r0, #3
	mov r2, #3
	add lr, pc, #6
	add pc, pc, @memcpy

	out r0
	out r1
	out r2
	out lr

	add r1, r1, #5
	add r0, r0, #2
	mov r2, #1
	add lr, pc, #6
	add pc, pc, @memcpy

	out r0
	out r1
	out r2
	out lr

	// Read out memory 
	add r0, pc, @dst_buf
	mov r1, #16
	out #0xaaaa
	out #0xaaaa
	add pc, pc, @readout_test
readout_loop:
	lhia r2, r0, #2
	out r2
readout_test:
	sub r1, r1, #1
	add.cs pc, pc, @readout_loop

	// End test
	out #0x7357
	out #0xdead
	out #0

// r0: dst, r1: src, r2: len, r3 is clobbered
memcpy_hword_loop:
	// Copy halfwords until fewer than 2 bytes remain
	lhia r3, r1, #2
	shia r3, r0, #2
memcpy:
	sub r2, r2, #2
	add.cs pc, pc, @memcpy_hword_loop

	// If there's a byte remaining, copy it
	tst r2, #1
	lb.zc r3, r1
	sb.zc r3, r0

	mov pc, lr

src_buf:
.hword 0x0100
.hword 0x0302
.hword 0x0504
.hword 0x0706
.hword 0x0908
.hword 0x0b0a
.hword 0x0d0c
.hword 0x0f0e
.hword 0x1110
.hword 0x1312
.hword 0x1514
.hword 0x1716
.hword 0x1918
.hword 0x1b1a
.hword 0x1d1c
.hword 0x1f1e
.hword 0x2120
.hword 0x2322
.hword 0x2524
.hword 0x2726
.hword 0x2928
.hword 0x2b2a
.hword 0x2d2c
.hword 0x2f2e
.hword 0x3130
.hword 0x3332
.hword 0x3534
.hword 0x3736
.hword 0x3938
.hword 0x3b3a
.hword 0x3d3c
.hword 0x3f3e

dst_buf:
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee

