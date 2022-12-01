// Cover 4 main variants of load instructions

entry:
	mov pc, #4

	add r0, pc, @test_data

	// Try each add mode
	ld r1, r0
	out r1

	ld2 r2, r0, #2
	out r2

	ldib r3, r0, #4
	out r3

	// ldia will appear to do nothing at first, but doing a plain load will
	// show that the register has incremented.
	ldia r4, r0, #2
	out r4
	out r0
	ld r5, r0
	out r5

	// Negative displacement -- make sure all bits are getting to adder
	mov r5, r0
	ld2 r0, r5, #-6
	out r0

	// PC-relative, clear regs to avoid possibility of accidentally using them
	mov r0, #0
	mov r1, #0
	mov r2, #0
	mov r3, #0
	mov r4, #0
	mov r5, #0
	ld2 r0, pc, @test_data
	out pc
	out r0

	out #0x7357
	out #0xdead
	out #0

test_data:
.hword 0x1234
.hword 0x5678
.hword 0xabcd
.hword 0xdef3
