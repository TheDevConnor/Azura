#ifndef azura_object_h
#define azura_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value)     (AS_OBJ(value)->type)

#define IS_FUNCTION(value)  isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value)    isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)    isObjType(value, OBJ_STRING)

#define AS_FUNCTION(value)  ((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value) \
    (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value)    ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)   (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
} ObjectType;

typedef struct Obj {
    ObjectType type;
    Obj* next;
} Obj;

typedef struct ObjFunction {
    Obj obj;
    int arity; // Stores the number of parameters the function expects.
    Chunk chunk;
    ObjString* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct ObjNative {
    Obj obj;
    NativeFn function;
} ObjNative;

typedef struct ObjString {
    Obj obj;
    int length;
    char* chars;
    uint32_t hash;
} ObjString;

typedef struct ObjClosure {
    Obj obj;
    ObjFunction* function;
} ObjClosure;

ObjFunction* newFunction();
ObjNative* newNative(NativeFn function);
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjectType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
