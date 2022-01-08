# tracc

> *Thoroughly Rusty ARM64 C Compiler*

This is my approach to writing a C compiler, using [norasandler's](https://github.com/nlsandler/write_a_c_compiler) stages
It outputs ARM64 code to be compiled with the gcc-aarch64-linux toolchain.
If you have an x86\_64 host (like I do), use `qemu-static`, compiling the code with `gcc -static` and gcc from `aarch64-none-linux-gnu` (available download is [here](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads))

## Currently supported stuff

The compiler currently expects an only function, with no parameters, with a list of the following:
  - Variable declarations (only `int`s for the moment)
  - Assignments (with their optional binary operators)
  - if statements (with else-if chaining)
  - blocks inside blocks for scoped variables
  - The not supported operators are:
    - Pre and post increment
    - Comma operator
    - Array accessing operator (compiler does not support pointers/arrays)

Input file:

```c
int foo() {
  int a = 1;
  int b = 2;
  if (a > b) {
    return a -b;
  } else {
    return a + b;
  }
}
```

Output assembly:

```armasm
	.arch armv8-a
	.global foo
	.type foo, %function
foo:
	sub sp, sp, #32
	mov w0, #1
	str w0, [sp, #20]
	mov w0, #2
	str w0, [sp, #16]
	ldr w0, [sp, #20]
	ldr w1, [sp, #16]
	cmp w0, w1
	ble .L0
	ldr w0, [sp, #20]
	ldr w1, [sp, #16]
	sub w0, w0, w1
	b   .L1
.L0:
	ldr w0, [sp, #20]
	ldr w1, [sp, #16]
	add w0, w0, w1
.L1:
	add sp, sp, #32
	ret
```
