// Write to a walking-ones pattern of addresses, then read them back to
// confirm they are all unique memory locations,

entry:
	mov pc, #4

	// Start at 256 bytes to avoid clobbering our own program. Address bits
	// 7:0 should already be covered by other tests. Go up to 32k.
	mov r0, #1
	sh  r0, zero, #0x0100
	mov r0, #2
	sh  r0, zero, #0x0200
	mov r0, #3
	sh  r0, zero, #0x0400
	mov r0, #4
	sh  r0, zero, #0x0800
	mov r0, #5
	sh  r0, zero, #0x1000
	mov r0, #6
	sh  r0, zero, #0x2000
	mov r0, #7
	sh  r0, zero, #0x4000
	mov r0, #8
	sh  r0, zero, #0x8000
	// And the last halfword:
	mov r0, #9
	sh  r0, zero, #0xfffe

	lh  r0, zero, #0x0100
	out r0
	lh  r0, zero, #0x0200
	out r0
	lh  r0, zero, #0x0400
	out r0
	lh  r0, zero, #0x0800
	out r0
	lh  r0, zero, #0x1000
	out r0
	lh  r0, zero, #0x2000
	out r0
	lh  r0, zero, #0x4000
	out r0
	lh  r0, zero, #0x8000
	out r0
	lh  r0, zero, #0xfffe
	out r0

	// End test
	out #0x7357
	out #0xdead
	out #0
