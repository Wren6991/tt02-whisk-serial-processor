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

	mov r0, #0x8000
	mov r1, #1
	add lr, pc, #6
	add pc, pc, @mul
	out r0

	mov r0, #0xffff
	mov r1, #0xffff
	add lr, pc, #6
	add pc, pc, @mul
	out r0

	out #0x7357
	out #0xdead
	out #0

// multiply r0, r1 into r0
// r0 shifts with each iteration
// r1 gradually clears as we test its bits
mul:
	// Debug prints
	out #0xaaaa
	out r0
	out r1

	mov r2, #1 // Walking bit mask
	mov r3, #0 // Accumulator
	add pc, pc, @mul_test
mul_loop:
	tst r1, r2
	add.zc r3, r3, r0
	andn r1, r2, r1
	sll r2, r2
	sll r0, r0
mul_test:
	cmp zero, r1
	add.zc pc, pc, @mul_loop

	mov r0, r3
	mov pc, lr
