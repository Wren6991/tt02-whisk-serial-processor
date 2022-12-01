// Cover 4 main variants of store instructions

entry:
	mov pc, #4

	add r0, pc, @test_data
	mov r1, #1
	st r1, r0

	mov r1, #2
	stib r1, r0, #2

	add r3, r0, #2
	mov r4, #3
	stia r3, r4, #2

	// Check increment-after took place
	mov r5, #4
	st r3, r5

	mov r0, #5
	st2 r0, r3, #2

	// Negative displacement
	add r3, r3, #6
	mov r1, #6
	st2 r1, r3, #-2

	// Read out memory into IO port
	add r0, pc, @test_data
	ldia r1, r0, #2
	out r1
	ldia r1, r0, #2
	out r1
	ldia r1, r0, #2
	out r1
	ldia r1, r0, #2
	out r1
	ldia r1, r0, #2
	out r1
	ldia r1, r0, #2
	out r1
	ldia r1, r0, #2
	out r1
	ldia r1, r0, #2
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
