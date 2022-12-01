// Check result and flags of SLL

entry:
	mov pc, #4

	// Check carry flag is shifted-out bit: 0x8000 and 0x0000 should have the
	// same result but differ in the carry bit.
	mov r4, #0x8000
	sll r4, r4
	out.pr r4
	out.cs #1
	out.cc #0

	mov r4, #0x0000
	sll r4, r4
	out.pr r4
	out.cs #1
	out.cc #0

	// Push some bits through neg flag
	mov r5, #0xabcd

	sll r5, r5
	out.ns #1
	out.nc #0
	sll r5, r5
	out.ns #1
	out.nc #0
	sll r5, r5
	out.ns #1
	out.nc #0
	sll r5, r5
	out.ns #1
	out.nc #0
	sll r5, r5
	out.ns #1
	out.nc #0
	sll r5, r5
	out.ns #1
	out.nc #0

	// Check zero flag with walking-ones
	mov r1, #1
	out r1
	mov r0, #16
	add pc, pc, @test1
loop1:
	sll r1, r1
	out.pr r1
	out.zs #1
	out.zc #0
test1:
	sub r0, r0, #1
	add.zc pc, pc, @loop1

	out #0x7357
	out #0xdead
	out #0
