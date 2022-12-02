// Check lh gives correct flags, dependent on loaded data.

entry:
	mov  pc, #4

	add r0, pc, @test_data
	mov r1, #5
	add pc, pc, @test
loop:
	lh r2, r0
	out.pr r2
	out.zs #1
	out.zc #0
	out.cs #1
	out.cc #0
	out.ns #1
	out.nc #0
	add r0, r0, #2
test:
	sub r1, r1, #1
	add.cs pc, pc, @loop

	out  #0x7357
	out  #0xdead
	out  #0

test_data:
.hword 0x1234
.hword 0xab78
.hword 0x56cd
.hword 0xdef3
.hword 0x0000