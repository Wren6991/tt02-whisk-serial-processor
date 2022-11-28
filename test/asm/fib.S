// Fibonacci test program

entry:
	mov pc, #4

test:
	mov r0, #0
	add lr, pc, #6
	add pc, pc, @fib
	out r0

	mov r0, #2
	add lr, pc, #6
	add pc, pc, @fib
	out r0

	mov r0, #10
	add lr, pc, #6
	add pc, pc, @fib
	out r0

	out #0x7357
	out #0xdead
	out #0

fib:
	out #0xf1b
	mov r1, #1
	mov r2, #1
	add pc, pc, @fib_test
fib_loop:
	add r3, r1, r2
	mov r1, r2
	mov r2, r3
fib_test:
	sub r0, r0, #1
	add.cs pc, pc, @fib_loop
	mov r0, r1
	mov pc, lr
