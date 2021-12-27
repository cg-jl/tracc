# tracc

> *Thoroughly Rusty ARM64 C Compiler*

This is my approach to writing a C compiler, using [norasandler's](https://github.com/nlsandler/write_a_c_compiler) stages
It outputs ARM64 code to be compiled with the gcc-aarch64-linux toolchain.
If you have an x86\_64 host (like I do), use `qemu-static`, compiling the code with `gcc -static` and gcc from `aarch64-none-linux-gnu` (available download is [here](https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads))

## Currently supported stuff

The compiler currently expects an only function, with no parameters. Statements supported are variable
declarations, some [expressions](#expressions-supported) and return statements.

### Expressions supported
- **Binary**:
  - **Arithmetic/Logic/Bitwise**:
  - `+`: Add
  - `-`: Subtract
  - `*`: Multiply
  - `/`: Divide
  - `%`: Modulo (unsigned division remainder)
  - `&&`: Logic and gate (logic means that it is reduced to 1/0)
  - `||`: Logic or gate
  - `>>`: shift right
  - `<<`: shift left
  - `|` : bitwise or
  - `&` : bitwise and
  - `^` : bitwise exclusive or
  - Relational: `<=`, `<`, `>`, `>=`
  - Equality: `==`, `!=`

- **Unary**:
  - `!`: Logic not
  - `~`: Bitwise not
  - `-`: Negate (`* -1`)

All combined assignment-operator operations are implemented, including all arithmetic, logic, and bitwise operators.





Input file:

```c
int foo() {
  int a = 1;
  int b = 2;
  return a + b;
}
```

Output assembly:

```armasm
	.arch armv8-a
	.global foo
	.type foo, %function
foo:
	sub sp, sp, #16
	mov w0, #1
	str w0, [sp, #12]
	mov w0, #2
	str w0, [sp, #8]
	ldr w1, [sp, #12]
	ldr w0, [sp, #8]
	add w0, w1, w0
	add sp, sp, #16
	ret
```
