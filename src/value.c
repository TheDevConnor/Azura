#include <stdio.h>
#include <string.h>

#include "object.h"
#include "memory.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapacoty = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacoty);
        array->values = GROW_ARRAY(Value, array->values, oldCapacoty, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(Value value) {
    switch (value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NIL: printf("nil"); break;
        case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
        case VAL_OBJ: printObject(value); break;
        case VAL_EMPTY: printf("<empty>"); break;
    }
}

bool valuesEqual(Value a, Value b) {
    if(a.type != b.type) return false;
    switch (a.type) {
        case VAL_BOOL: return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL: return true;
        case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_OBJ: return AS_OBJ(a) == AS_OBJ(b);
        case VAL_EMPTY: return true;
    }

    return false;
}

static uint32_t hashDouble(double value) {
    union BitCast {
        double value;
        uint32_t ints[2];
    };

    union BitCast cast;
    cast.value = (value) + 1.0;
    return cast.ints[0] + cast.ints[1];
}

uint32_t hashValue(Value value) {
    switch (value.type) {
        case VAL_BOOL:   return AS_BOOL(value) ? 3 : 5;
        case VAL_NIL:    return 7;
        case VAL_NUMBER: return hashDouble(AS_NUMBER(value));
        case VAL_OBJ:    return AS_STRING(value)->hash;
        case VAL_EMPTY:  return 0;
    }
    return 0;
}
