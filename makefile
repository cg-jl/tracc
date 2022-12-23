TCH_PREFIX := ./toolchains/gcc-10.3/bin/aarch64-none-linux-gnu-

GCC := $(TCH_PREFIX)gcc

%.gcc.s: %.c
	$(GCC) -o $@ $^ -S
	sed -i '/cfi/d' $@

%.s: %.c target/debug/tracc
	cargo run -- -o $@ $<

%.gcc: %.c
	$(GCC) -o $@ $^ -static

%.tracc: %.s
	$(GCC) -o $@ $^ -static
