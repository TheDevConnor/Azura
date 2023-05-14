#include <time.h>
#include <string.h>

#include "common.h"
#include "value.h"
#include "table.h"
#include "object.h"
#include "vm.h"

#include "math.h"
#include "strings.h"

static void defineNative(const char* name, NativeFn function) {
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function)));
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  pop();
  pop();
}

static Value clockNative() {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

void defineAllNativeFunctions() {
    defineNative("clock", clockNative);

    defineNative("pi", piNative);
    defineNative("pow", powNative);
    defineNative("sqrt", sqrtNative);

    defineNative("sin", sinNative);
    defineNative("cos", cosNative);
    defineNative("tan", tanNative);
    defineNative("asin", asinNative);
    defineNative("acos", acosNative);
    defineNative("atan", atanNative);
    defineNative("abs", absNative);
}