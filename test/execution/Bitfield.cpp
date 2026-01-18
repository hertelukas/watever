// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -fno-exceptions %s -o %t.ll
// RUN: watever %t.ll -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm
// RUN: wasmtime %t.wasm 21 | FileCheck %s

// RUN: /opt/wasi-sdk/bin/clang++ -S -emit-llvm --target=wasm32-wasip2 -O3 -fno-exceptions %s -o %t.O3.ll
// RUN: watever %t.O3.ll -o %t.O3.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.O3.o -o %t.O3.wasm
// RUN: wasmtime %t.O3.wasm 21 | FileCheck %s

// The G++ FE does some constant-folding at -O1 and above; it
// recognizes the 1,2,3 test can be converted into a range expression
// ((signed char)(op.datatype-1) < 3).  Alas, it generated an
// ambiguous conversion that was mis-interpreted by the GCC=>LLVM
// translation phase.  <rdar://problem/9186245>
#include <stdio.h>

typedef struct _operation {
  unsigned int datatype       : 3;

} operation;

operation op;

void __attribute__ ((__noinline__))
init() {
  op.datatype = 4;
}


int main( int argc, char *argv[] )
{
  init();
  if( (1 == op.datatype) ||
      (2 == op.datatype) ||
      (3 == op.datatype) )
    {
      printf( "1, 2 or 3: FAIL\n" );
    }
  else if (4 == op.datatype)
    {
      printf( "4: PASS\n" );
      return 0;
    }
  else
    printf( "Not 1,2,3 or 4: FAIL\n" );
  return -1;
}

// CHECK: 4: PASS
