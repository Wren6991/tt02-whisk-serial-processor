// Test byte store (minimally!)

entry:
	mov pc, #4

	// Even address
	add r0, pc, @test_data
	mov r1, #0xa5
	sb r1, r0

	// Odd address
	add r0, r0, #5
	mov r1, #0xa5
	sb r1, r0

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
