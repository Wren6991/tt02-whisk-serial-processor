// Called "aaa" so it runs first :)
// Just set up PC and then quit.

entry:
	add pc, zero, #4
	out #0x7357
	out #0xdead
	out #0
