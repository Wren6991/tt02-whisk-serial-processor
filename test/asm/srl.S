// Check results of SRL

entry:
	mov pc, #4

	// Zero
	mov r0, #0
	srl r0, r0
	out r0

	// Walking ones
	mov r1, #0x8000
	out r1
	mov r0, #16
	add pc, pc, @test1
loop1:
	srl r1, r1
	out r1
test1:
	sub r0, r0, #1
	add.cs pc, pc, @loop1

	// All-ones
	mov r3, #0xffff
	out r3
	mov r2, #16
	add pc, pc, @test2
loop2:
	srl r3, r3
	out r3
test2:
	sub r2, r2, #1
	add.cs pc, pc, @loop2

	out #0x7357
	out #0xdead
	out #0
