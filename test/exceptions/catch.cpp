// RUN: /opt/wasi-sdk/bin/clang++ -c -emit-llvm --target=wasm32-wasip2 -fwasm-exceptions -mllvm -wasm-use-legacy-eh=false %s -o %t.bc
// RUN: watever %t.bc -o %t.o
// RUN: /opt/wasi-sdk/bin/clang++ %t.o -o %t.wasm -fwasm-exceptions -lunwind
// RUN: wasmtime run -W exceptions=y %t.wasm

void foo();
int main(int argc, char **argv) {
    try{foo();}
    catch (...) {

    }
}
