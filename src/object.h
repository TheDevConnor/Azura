#ifndef azura_object_h
#define azura_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value)     (AS_OBJ(value)->type)

#define IS_STRING(value)    isObjType(value, OBJ_STRING)

#define AS_STRING(value)    ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)   (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_STRING,
} ObjectType;

typedef struct Obj {
    ObjectType type;
} Obj;

typedef struct ObjString {
    Obj obj;
    int length;
    char* chars;
} ObjString;

struct ObjString* takeString(char* chars, int length);
struct ObjString* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjectType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
