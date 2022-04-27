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
	sub sp, sp, #16
.LBB0:
	mov w1, #1
	str w1, [sp]
	mov w3, #2
	str w3, [sp, #4]
	ldr w4, [sp]
	ldr w5, [sp, #4]
	cmp w4, w5
	cset w6, gt
	cmp w6, wzr
	beq .LBB2
	b   .LBB1
.LBB1:
	b   .LBB3
.LBB2:
	b   .LBB3
.LBB3:
	mov w0, wzr
	add sp, sp, #16
	ret
```
