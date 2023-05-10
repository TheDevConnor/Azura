#ifndef azura_math_h
#define azura_math_h

#include "common.h"
#include "value.h"

Value sinNative(int argCount, Value* args);
Value cosNative(int argCount, Value* args);
Value tanNative(int argCount, Value* args);

Value asinNative(int argCount, Value* args);
Value acosNative(int argCount, Value* args);
Value atanNative(int argCount, Value* args);

#endif
