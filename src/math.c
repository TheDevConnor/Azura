#include <math.h>
#include <stdio.h>

#include "common.h"
#include "math.h"
#include "value.h"

#define PI 3.14159265358979323846

static double to_radians(double degrees) {
    return degrees * (PI / 180.0);
}

Value piNative() {
    return NUMBER_VAL(PI);
}

Value sqrtNative(int argCount, Value* args) {
    if(argCount != 1) {
        printf("sqrt() takes exactly 1 argument (%d given)", argCount);
        return NIL_VAL;
    }
    return NUMBER_VAL(sqrt(AS_NUMBER(args[0])));
}

Value powNative(int argCount, Value* args) {
    if (argCount != 2) {
        printf("pow() takes exactly 2 arguments (%d given)", argCount);
        return NIL_VAL;
    }
    return NUMBER_VAL(pow(AS_NUMBER(args[0]), AS_NUMBER(args[1])));
}

Value sinNative(int argCount, Value* args) {
    double arg = AS_NUMBER(args[0]);
    if (argCount == 2 && AS_BOOL(args[1])) { 
        // Second argument is a flag to indicate degrees
        arg = to_radians(arg);
    }
    return NUMBER_VAL(sin(arg));
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
    return NUMBER_VAL(tan(arg));
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

Value absNative(int argCount, Value* args) {
    if (argCount != 1) {
        printf("abs() takes exactly 1 argument (%d given)", argCount);
        return NIL_VAL;
    }
    return NUMBER_VAL(fabs(AS_NUMBER(args[0])));
}
