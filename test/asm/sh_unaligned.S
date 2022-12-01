// Check a byte-aligned halfword store puts the correct data at the correct
// location, and does not write outside of it

entry:
	mov pc, #4

	add r0, pc, @test_data
	mov r1, #0xa5c3
	sh r1, r0

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
