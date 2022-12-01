// Byte load test

entry:
	mov pc, #4

	add r0, pc, @test_data
	mov r1, #8
	add pc, pc, @test
loop:
	lb r2, r0
	out r2
	add r0, r0, #1
test:
	sub r1, r1, #1
	add.cs pc, pc, @loop

	out #0x7357
	out #0xdead
	out #0

test_data:
.hword 0x1234
.hword 0x5678
.hword 0xabcd
.hword 0xdef3
