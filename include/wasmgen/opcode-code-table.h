/*
 * Copied from
 * https://github.com/WebAssembly/wabt/blob/main/include/wabt/opcode-code-table.h
 */

#ifndef WASMGEN_OPCODE_CODE_TABLE_H
#define WASMGEN_OPCODE_CODE_TABLE_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define WASMGEN_OPCODE_CODE_TABLE_SIZE 131072

/*
 * Number of bits required to store an opcode
 */
#define MAX_OPCODE_BITS 9

/* This structure is defined in C because C++ doesn't (yet) allow you to use
 * designated array initializers, i.e. [10] = {foo}.
 */
extern uint32_t WasmgenOpcodeCodeTable[WASMGEN_OPCODE_CODE_TABLE_SIZE];

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif // WASMGEN_OPCODE_CODE_TABLE_H
