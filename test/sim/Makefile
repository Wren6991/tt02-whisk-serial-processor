TEST ?= aaa
TBDIR := ../tb_cxxrtl
MAX_CYCLES := 1000000

# Rules for running a single test specified as TEST=

run: tmp/$(TEST).bin $(TBDIR)/tb
	$(TBDIR)/tb --cycles $(MAX_CYCLES) --bin tmp/$(TEST).bin --vcd tmp/$(TEST).vcd
	ln -sf $(TEST).vcd tmp/lastrun.vcd

view: run
	gtkwave tmp/$(TEST).vcd &

$(TBDIR)/tb: ../../src/whisk.v $(TBDIR)/tb.cpp $(TBDIR)/Makefile
	make -C $(TBDIR) tb


# Rules for running multiple tests, generating test traces, generating
# references, and diffing them

tmp/%.bin: ../asm/%.S ../tools/whiskasm
	@mkdir -p tmp
	../tools/whiskasm $< $@ --dis $@.dis

tmp/%.out: tmp/%.bin $(TBDIR)/tb
	$(TBDIR)/tb --cycles $(MAX_CYCLES) --bin $< > $@

tmp/%.ref: tmp/%.bin ../tools/whisksim
	../tools/whisksim $< > $@

tmp/%.diff: tmp/%.ref tmp/%.out
	diff $^

# Useless targets to stop make from deleting test output:
bins: $(patsubst ../asm/%.S,tmp/%.bin,$(wildcard ../asm/*))
refs: $(patsubst ../asm/%.S,tmp/%.ref,$(wildcard ../asm/*))
outs: $(patsubst ../asm/%.S,tmp/%.out,$(wildcard ../asm/*))

test: $(patsubst ../asm/%.S,tmp/%.diff,$(wildcard ../asm/*))
	@echo "Successfully ran $(words $^) tests."

clean:
	rm -rf tmp
	make -C $(TBDIR) clean
