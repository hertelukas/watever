/*
 * Copied from
 * https://github.com/WebAssembly/wabt/blob/main/src/opcode-code-table.c
 */

#include "watever/opcode-code-table.h"

#include <stdint.h>

typedef enum WabtOpcodeEnum {
#define WATEVER_OPCODE(rtype, type1, type2, type3, mem_size, prefix, code,     \
                       Name, text, decomp)                                     \
  Name,
#include "watever/opcode.def"
#undef WATEVER_OPCODE
  Invalid,
} WabtOpcodeEnum;

_Static_assert((Invalid <= WATEVER_OPCODE_CODE_TABLE_SIZE),
               "Invalid <= WATEVER_OPCODE_CODE_TABLE_SIZE");

/* The array index calculated below must match the one in Opcode::FromCode. */
uint32_t WateverOpcodeCodeTable[WATEVER_OPCODE_CODE_TABLE_SIZE] = {
#define WATEVER_OPCODE(rtype, type1, type2, type3, mem_size, prefix, code,     \
                       Name, text, decomp)                                     \
  [(prefix << MAX_OPCODE_BITS) + code] = Name,
#include "watever/opcode.def"
#undef WATEVER_OPCODE
};
