#include <stdlib.h>
#include "chunk.h"
#include "memory.h"
#include "vm.h"

void initChunk(Chunk* chunk){
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->line = NULL;
    initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk){
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->line, chunk->capacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line){
    if(chunk->capacity < chunk->count + 1){
        int oldCapicty = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapicty);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapicty, chunk->capacity);
        chunk->line = GROW_ARRAY(int, chunk->line, oldCapicty, chunk->capacity);
    }
    chunk->code[chunk->count] = byte;
    chunk->line[chunk->count] = line;
    chunk->count++;
}

int addConstants(Chunk* chunk, Value value) {
    push(value);
    writeValueArray(&chunk->constants, value);
    pop();
    return chunk->constants.count - 1;
}
