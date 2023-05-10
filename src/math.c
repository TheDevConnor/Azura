#include <math.h>
#include <stdio.h>

#include "common.h"
#include "math.h"
#include "value.h"

#define PI 3.14159265358979323846

static double to_radians(double degrees) {
    return degrees * (PI / 180.0);
}

Value sinNative(int argCount, Value* args) {
    double arg = AS_NUMBER(args[0]);
    if (argCount == 2 && AS_BOOL(args[1])) { 
        // Second argument is a flag to indicate degrees
        arg = to_radians(arg);
    }
    return NUMBER_VAL(sin(to_radians(AS_NUMBER(args[0]))));
}

Value cosNative(int argCount, Value* args) {
    double arg = AS_NUMBER(args[0]);
    if (argCount == 2 && AS_BOOL(args[1])) { 
        // Second argument is a flag to indicate degrees
        arg = to_radians(arg);
    }
    return NUMBER_VAL(cos(arg));
}

Value tanNative(int argCount, Value* args) {
    double arg = AS_NUMBER(args[0]);
    if (argCount == 2 && AS_BOOL(args[1])) { 
        // Second argument is a flag to indicate degrees
        arg = to_radians(arg);
    }
    return NUMBER_VAL(tan(to_radians(AS_NUMBER(args[0]))));
}

Value asinNative(int argCount, Value* args) {
    double arg = AS_NUMBER(args[0]);
    if (argCount == 2 && AS_BOOL(args[1])) { 
        // Second argument is a flag to indicate degrees
        arg = to_radians(arg);
    }
    return NUMBER_VAL(asin(arg));
}

Value acosNative(int argCount, Value* args) {
    double arg = AS_NUMBER(args[0]);
    if (argCount == 2 && AS_BOOL(args[1])) { 
        // Second argument is a flag to indicate degrees
        arg = to_radians(arg);
    }
    return NUMBER_VAL(acos(arg));
}

Value atanNative(int argCount, Value* args) {
    double arg = AS_NUMBER(args[0]);
    if (argCount == 2 && AS_BOOL(args[1])) { 
        // Second argument is a flag to indicate degrees
        arg = to_radians(arg);
    }
    return NUMBER_VAL(atan(arg));
}