# Scalability Benchmarks

This folder contains generators for arbitrarily large LLVM IR modules to test the scalability of programs consuming LLVM-IR and identify cases of super-linear runtime.

Generators (`*.test`) are Python programs that print textual LLVM-IR. The single argument is the size parameter, which must be a positive integer.

The script `run.py <generator> <command>` will run the specified command with inputs supplied for the generator for different sizes, measure execution time and max-rss, and guesses whether the runtime growth is linear or quadratic. The script requires `/usr/bin/time` (GNU for `-f`) and `python3`; the generator output is supplied to the command on stdin, stdout of the command is discarded.

Examples for interesting commands:

- `tpde-llc` (TPDE-LLVM) (runtime should be linear)
- `llc -filetype=obj -O0` (LLVM `-O0` back-end; also try different instruction selectors) (runtime should be linear)
- `llc -filetype=obj` (LLVM `-O2` back-end)
- `opt -O2` (LLVM default optimization passes) (will frequently exhibit super-linear runtime)

Example usage:

```
$ ./run.py store-const.test llc -mtriple=aarch64 -filetype=obj -O0 -global-isel=1
1 0.0 47580.0
2 0.0 47704.0
4 0.0 47708.0
8 0.0 47708.0
16 0.0 47732.0
32 0.0 47760.0
64 0.0 47800.0
128 0.0 47944.0
256 0.0 48200.0
512 0.01 48732.0
1024 0.01 49660.0
2048 0.04 50176.0
4096 0.15 53908.0
8192 0.55 64784.0
16384 2.16 87180.0
24576 5.13 113264.0
32768 9.66 134744.0
Polynomial: 0.00181388 - 0.09064901·x + 1.08225727·x²
$ python3 store-const.test 24576 | perf record -g llc -mtriple=aarch64 -filetype=obj -O0 -global-isel=1 > /dev/null
```
