#ifndef azura_math_h
#define azura_math_h

#include "common.h"
#include "value.h"

Value piNative();

Value sqrtNative(int argCount, Value* args);
Value powNative(int argCount, Value* args);

Value sinNative(int argCount, Value* args);
Value cosNative(int argCount, Value* args);
Value tanNative(int argCount, Value* args);

Value asinNative(int argCount, Value* args);
Value acosNative(int argCount, Value* args);
Value atanNative(int argCount, Value* args);

Value absNative(int argCount, Value* args);

// TODO: Implement these once I have arrays added
// Value minNative(int argCount, Value* args);
// Value maxNative(int argCount, Value* args);
// Value medianNative(int argCount, Value* args);

#endif
