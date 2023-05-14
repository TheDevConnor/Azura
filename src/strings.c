#include "strings.h"
#include "value.h"
#include "object.h"

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

Value toUpperCaseNative(int argCount, Value* args) {
    if (argCount != 1) {
        printf("toUpperCase() takes exactly 1 argument (%d given)", argCount);
        return NIL_VAL;
    }

    if (!IS_STRING(args[0])) {
        printf("toUpperCase() takes a string as an argument");
        return NIL_VAL;
    }

    ObjString* string = AS_STRING(args[0]);
    char* newString = malloc(sizeof(char) * (string->length + 1));
    for (int i = 0; i < string->length; i++) {
        newString[i] = toupper(string->chars[i]);
    }
    newString[string->length] = '\0';

    return OBJ_VAL(copyString(newString, string->length));
}

Value toLowerCaseNative(int argCount, Value* args) {
    if (argCount != 1) {
        printf("toLowerCase() takes exactly 1 argument (%d given)", argCount);
        return NIL_VAL;
    }

    if (!IS_STRING(args[0])) {
        printf("toLowerCase() takes a string as an argument");
        return NIL_VAL;
    }

    ObjString* string = AS_STRING(args[0]);
    char* newString = malloc(sizeof(char) * (string->length + 1));
    for (int i = 0; i < string->length; i++) {
        newString[i] = tolower(string->chars[i]);
    }
    newString[string->length] = '\0';

    return OBJ_VAL(copyString(newString, string->length));
}

