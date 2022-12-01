// Cover 4 main variants of store instructions

entry:
	mov pc, #4

	add r0, pc, @test_data
	mov r1, #1
	sh r1, r0

	mov r1, #2
	shib r1, r0, #2

	add r3, r0, #2
	mov r4, #3
	shia r4, r3, #2

	// Check increment-after took place
	mov r5, #4
	sh r5, r3

	mov r0, #5
	sh r0, r3, #2

	// Negative displacement
	add r3, r3, #6
	mov r1, #6
	sh r1, r3, #-2

	// Read out memory into IO port
	add r0, pc, @test_data
	lhia r1, r0, #2
	out r1
	lhia r1, r0, #2
	out r1
	lhia r1, r0, #2
	out r1
	lhia r1, r0, #2
	out r1
	lhia r1, r0, #2
	out r1
	lhia r1, r0, #2
	out r1
	lhia r1, r0, #2
	out r1
	lhia r1, r0, #2
	out r1

	// End test
	out #0x7357
	out #0xdead
	out #0

test_data:
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
.hword 0xeeee
