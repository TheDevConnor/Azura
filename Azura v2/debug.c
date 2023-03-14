#include <stdlib.h>
#include <stdio.h>
#include "debug.h"
#include "chunk.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);
    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n ", name);
    return offset + 2;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset+1];
    printf("%-16s %4d '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);

    if (offset > 0 && chunk->line[offset] == chunk->line[offset - 1]) {
        printf("    | ");
    } else {
        printf("%4d ", chunk->line[offset]);
    }

    uint8_t instruction = chunk->code[offset];
    switch(instruction)  {
        case OP_RETRUN:
            return simpleInstruction("OP_RETURN", offset);
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        default:
            printf("Unkown opcode %d\n", instruction);
            return offset + 1;
    }
}