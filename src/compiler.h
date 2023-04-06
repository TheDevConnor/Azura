#ifndef azura_compiler_h
#define azura_compiler_h

#include "object.h"
#include "vm.h"

#define MAX_CASES 256
ObjFunction* compile(const char *source);
void errorHandling(const char* source, const char* file_name);

#endif
