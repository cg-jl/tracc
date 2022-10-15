#!/bin/bash
# TODO capture output for errors

try_run() {
  sh -c "$1" 2>test_run_failed
}

# source dest
try_compile_with_gcc() {
    try_run "~/sdks/gcc-arm/bin/aarch64-none-linux-gnu-gcc -static -o $2 $1"
}

try_compile_with_project() {
  try_run "cargo run -- -o $1.s $1" && try_compile_with_gcc "$1.s" "$1.cc"
}

try_compile_normal() {
  try_compile_with_gcc $1 "$1.gcc"
}

compare_results() {
  qemu-aarch64 ./$1.cc
  local res1=$?
  qemu-aarch64 ./$1.gcc
  local res2=$?
  if [ $res1 -eq $res2 ]; then
    return 0
  else 
    echo "$res1 (project's compiler) != $res2 (working compiler)" 1> test_run_failed
    return 1
  fi
}


test_one() {
  local res=0
  try_compile_with_project $1 && try_compile_normal $1 && compare_results $1 && echo -e '\x1b[32m'$2'\x1b[m' \
      || { echo -e '\x1b[31m'$2'\x1b[m'; echo "---- error message for $2 --------"; cat test_run_failed; echo "----------------------------------"; res=1; }
  rm test_run_failed
  return $res
}

test_stage() {
  local path="write_a_c_compiler/stage_$1/valid"
  while [ $# -gt 1 ]; do
    shift
    path="$path/$1"
  done
  local res=0
  for i in $path/*.c; do
    if ! test_one $i "`basename $i`"; then
      res=1
    fi
  done
  return $res
}

test_stage $@
