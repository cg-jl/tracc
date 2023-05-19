TCH_PREFIX := aarch64-unknown-linux-gnu-

GCC := $(TCH_PREFIX)gcc

%.gcc.s: %.c
	$(GCC) -o $@ $^ -S
	sed -i '/cfi/d' $@

%.s: %.c target/debug/tracc
	cargo run -- -o $@ $<

%.gcc: %.c
	$(GCC) -o $@ $^

%.tracc: %.s
	$(GCC) -o $@ $^
