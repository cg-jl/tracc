# tracc

> *Thoroughly Rusty ARM64 C Compiler*

This is my approach to writing a C compiler, using [norasandler's](https://github.com/nlsandler/write_a_c_compiler) stages
It outputs ARM64 code to be compiled with the gcc-aarch64-linux toolchain.
If you have an x86\_64 host (like I do), use `qemu-static`, compiling the code with `gcc -static` and gcc from `aarch64-none-linux-gnu` (available download is [here](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads))

## Currently supported stuff

The compiler currently expects an only function, with no parameters and a single return statement,
with a constant expression (some binary operators and number literals)

Input file:

```c
int foo() {
  return 1 + 2;
}
```

Output assembly:

```armasm
  .global foo
  .type foo, %function
main:
  mov w1, #1
  mov w0, #2
  add w0, w1, w0
  ret
```
