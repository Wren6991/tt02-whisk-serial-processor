TOP    := whisk_top
SRCS   := ../../src/whisk.v
VDEFS  := WHISK_DEFAULT_NETTYPE_NONE WHISK_NO_CELLS

.PHONY: clean all

all: tb

SYNTH_CMD += read_verilog $(addprefix -D,$(VDEFS)) $(SRCS);
SYNTH_CMD += hierarchy -top $(TOP);
SYNTH_CMD += write_cxxrtl dut.cpp

dut.cpp: $(SRCS)
	yosys -p '$(SYNTH_CMD)' 2>&1 > cxxrtl.log

clean::
	rm -f dut.cpp cxxrtl.log tb

tb: dut.cpp tb.cpp
	clang++ -O3 -std=c++14 -I $(shell yosys-config --datdir)/include tb.cpp -o tb
