TCH_PREFIX := aarch64-linux-gnu-

GCC := $(TCH_PREFIX)gcc

%.gcc.s: %.c
	$(GCC) -o $@ $^ -S
	sed -i '/cfi/d' $@

%.s: %.c target/debug/tracc
	TRACC_TRACE='' cargo run -- -o $@ $<

%.gcc: %.c
	$(GCC) -static -o $@ $^

%.tracc: %.s
	$(GCC) -static -o $@ $^

cleanall:
	find write_a_c_compiler -type f '-(' -name '*.s' -or -name '*.*cc' '-)' | parallel --bar rm
