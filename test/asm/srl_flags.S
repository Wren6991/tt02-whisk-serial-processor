// Check result and flags of SRL

entry:
	mov pc, #4

	// Check carry flag is shifted-out bit: 0x1 and 0x0 should have the same
	// result but differ in the carry bit.
	mov r4, #1
	srl r4, r4
	out.pr r4
	out.cs #1
	out.cc #0

	mov r4, #0
	srl r4, r4
	out.pr r4
	out.cs #1
	out.cc #0

	// N should always be false
	mov r5, #0xabcd
	srl r5, r5
	out.ns #1
	out.nc #0
	mov r5, #0x0bcd
	srl r5, r5
	out.ns #1
	out.nc #0
	srl r5, r5
	out.ns #1
	out.nc #0
	mov r5, #0xffff
	srl r5, r5
	out.ns #1
	out.nc #0
	srl r5, r5
	out.ns #1
	out.nc #0
	srl r5, r5
	out.ns #1
	out.nc #0

	// Check zero flag starting with all-ones shifting in zeroes from right
	mov r1, #0xffff
	out r1
	mov r0, #16
	add pc, pc, @test1
loop1:
	srl r1, r1
	out.pr r1
	out.zs #1
	out.zc #0
test1:
	sub r0, r0, #1
	add.zc pc, pc, @loop1

	// Check zero flag for walking ones
	mov r1, #0x8000
	out r1
	mov r0, #16
	add pc, pc, @test2
loop2:
	srl r1, r1
	out.pr r1
	out.zs #1
	out.zc #0
test2:
	sub r0, r0, #1
	add.zc pc, pc, @loop2


	out #0x7357
	out #0xdead
	out #0
