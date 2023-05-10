#include <math.h>
#include <time.h>
#include <stdio.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "math.h"
#include "vm.h"

static Value sinNative(int argCount, Value* args) {
  if (argCount != 1) {
    printf("sin() takes exactly one argument");
    return NIL_VAL;
  }
  if (!IS_NUMBER(args[0])) {
    printf("sin() takes a number as an argument");
    return NIL_VAL;
  }
  return NUMBER_VAL(sin(AS_NUMBER(args[0])));
}

static Value cosNative(int argCount, Value* args) {
  if (argCount != 1) {
    printf("cos() takes exactly one argument");
    return NIL_VAL;
  }
  if (!IS_NUMBER(args[0])) {
    printf("cos() takes a number as an argument");
    return NIL_VAL;
  }
  return NUMBER_VAL(cos(AS_NUMBER(args[0])));
}

static Value tanNative(int argCount, Value* args) {
  if (argCount != 1) {
    printf("tan() takes exactly one argument");
    return NIL_VAL;
  }
  if (!IS_NUMBER(args[0])) {
    printf("tan() takes a number as an argument");
    return NIL_VAL;
  }
  return NUMBER_VAL(tan(AS_NUMBER(args[0])));
}