#ifndef azura_chunk_h
#define azura_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    OP_CONSTANT,
    OP_RETRUN,
} OpCode;

typedef struct {
    int count;
    int capacity;
    uint8_t* code;
    int* line;
    ValueArray constants;
}Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstants(Chunk* chunk, Value value);

#endif