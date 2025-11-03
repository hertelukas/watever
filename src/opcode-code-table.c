/*
 * Copied from
 * https://github.com/WebAssembly/wabt/blob/main/src/opcode-code-table.c
 */

#include "wasmgen/opcode-code-table.h"

#include <stdint.h>

typedef enum WabtOpcodeEnum {
#define WASMGEN_OPCODE(rtype, type1, type2, type3, mem_size, prefix, code,     \
                       Name, text, decomp)                                     \
  Name,
#include "wasmgen/opcode.def"
#undef WASMGEN_OPCODE
  Invalid,
} WabtOpcodeEnum;

_Static_assert((Invalid <= WASMGEN_OPCODE_CODE_TABLE_SIZE),
               "Invalid <= WASMGEN_OPCODE_CODE_TABLE_SIZE");

/* The array index calculated below must match the one in Opcode::FromCode. */
uint32_t WasmgenOpcodeCodeTable[WASMGEN_OPCODE_CODE_TABLE_SIZE] = {
#define WASMGEN_OPCODE(rtype, type1, type2, type3, mem_size, prefix, code,     \
                       Name, text, decomp)                                     \
  [(prefix << MAX_OPCODE_BITS) + code] = Name,
#include "wasmgen/opcode.def"
#undef WASMGEN_OPCODE
};
