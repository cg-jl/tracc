TCH_PREFIX := ./toolchains/out/bin/aarch64-linux-musl-

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
