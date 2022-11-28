// Multiply test program

entry:
	mov pc, #4

	mov r0, #1
	mov r1, #0
	add lr, pc, #6
	add pc, pc, @mul
	out r0

	mov r0, #1
	mov r1, #2
	add lr, pc, #6
	add pc, pc, @mul
	out r0

	mov r0, #2
	mov r1, #1
	add lr, pc, #6
	add pc, pc, @mul
	out r0

	mov r0, #5
	mov r1, #6
	add lr, pc, #6
	add pc, pc, @mul
	out r0

	mov r0, #1
	mov r1, #0x8000
	add lr, pc, #6
	add pc, pc, @mul
	out r0

	out #0xdead
	out #0x1234

// multiply r0, r1 into r0
// r0 shifts with each iteration
// r1 gradually clears as we test its bits
mul:
	mov r2, #-2            // bit mask
	mov r3, #0             // accumulator
	add pc, pc, @mul_test
mul_loop:
	add r3, r3, r1
	not r1, r1
	andn r1, r2, r1
	sll r2, r2
mul_test:
	andn zero, r1, r2
	add.zc pc, pc, @mul_loop

	mov r0, r3
	mov pc, lr
