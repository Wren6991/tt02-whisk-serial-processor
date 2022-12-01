// Test results of OR

entry:
	mov pc, #4

	// Walking ones against all-zeroes
	mov r1, #1
	out r1
	mov r0, #16
	add pc, pc, @test1
loop1:
	sll r1, r1
	or  r2, r1, #0x0000
	out r2
test1:
	sub r0, r0, #1
	add.cs pc, pc, @loop1

	// All-zeroes against walking ones
	mov r1, #1
	mov r3, #0
	out r1
	mov r0, #16
	add pc, pc, @test2
loop2:
	sll r1, r1
	or  r2, r3, r1
	out r2
test2:
	sub r0, r0, #1
	add.cs pc, pc, @loop2

	// Walk ones through immediate
	or  r0, r3, #0x0001
	out r0
	or  r0, r3, #0x0002
	out r0
	or  r0, r3, #0x0004
	out r0
	or  r0, r3, #0x0008
	out r0
	or  r0, r3, #0x0010
	out r0
	or  r0, r3, #0x0020
	out r0
	or  r0, r3, #0x0040
	out r0
	or  r0, r3, #0x0080
	out r0
	or  r0, r3, #0x0100
	out r0
	or  r0, r3, #0x0200
	out r0
	or  r0, r3, #0x0400
	out r0
	or  r0, r3, #0x0800
	out r0
	or  r0, r3, #0x1000
	out r0
	or  r0, r3, #0x2000
	out r0
	or  r0, r3, #0x4000
	out r0
	or  r0, r3, #0x8000
	out r0

	// End test
	out #0x7357
	out #0xdead
	out #0
