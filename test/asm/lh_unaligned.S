// Check a byte-aligned halfword load gives correct result

entry:
	mov  pc, #4

	add  r0, pc, @test_data
	add  r0, r0, #1
	lhia r1, r0, #2
	out  r1
	lhia r1, r0, #2
	out  r1
	lh   r1, r0, #2
	out  r1

	out  #0x7357
	out  #0xdead
	out  #0

test_data:
.hword 0x1234
.hword 0x5678
.hword 0xabcd
.hword 0xdef3
.hword 0x0000