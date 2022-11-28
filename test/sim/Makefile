TEST ?= aaa
TBDIR := ../tb_cxxrtl

run: tmp/$(TEST).bin $(TBDIR)/tb
	$(TBDIR)/tb --cycles 100000 --bin tmp/$(TEST).bin --vcd tmp/$(TEST).vcd
	ln -sf $(TEST).vcd tmp/lastrun.vcd

view: run
	gtkwave tmp/$(TEST).vcd &

tmp/%.bin: ../asm/%.S
	mkdir -p tmp
	../whiskasm $< $@ --dis $@.dis

$(TBDIR)/tb: ../../src/whisk.v $(TBDIR)/tb.cpp
	make -C $(TBDIR) tb

clean:
	rm -rf tmp
	make -C $(TBDIR) clean
