#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "object.h"
#include "scanner.h"
#include "terminalColors.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
  Token current;
  Token previous;
  bool hadError;
  bool panicMode;
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSINMENT,   // :=
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARASION, // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY,
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

typedef struct {
  Token name;
  int depth;
  bool isCaptured;
} Local;

typedef struct {
  uint8_t index;
  bool isLocal;
} Upvalue;

typedef enum {
  TYPE_METHOD,
  TYPE_FUNC,
  TYPE_INITIALIZER,
  TYPE_SCRIPT,
} FunctionType;

typedef struct Compiler {
  struct Compiler* enclosing;
  ObjFunction* function;
  FunctionType type;

  Local locals[UINT8_COUNT];
  int localCount;
  Upvalue upvalues[UINT8_COUNT];
  int scopeDepth;
} Compiler;

typedef struct ClassCompiler {
  struct ClassCompiler* enclosing;
  bool hasSuperClass;
} ClassCompiler;

const char* Color (TerminalColor color) {
  switch (color) {
    case BLACK:           return "\033[0;30m";
    case GRAY:            return "\033[1;30m";
    case RED:             return "\033[0;31m";
    case GREEN:           return "\033[0;32m";
    case YELLOW:          return "\033[0;33m";
    case BLUE:            return "\033[0;34m";
    case MAGENTA:         return "\033[0;35m";
    case CYAN:            return "\033[0;36m";
    case WHITE:           return "\033[0;37m";
    case RESET:           return "\033[0m";
  }
  return "";
}

Parser parser;
Compiler* current = NULL;
ClassCompiler* currentClass = NULL;

static Chunk* currentChunk() { return &current->function->chunk; }

static void errorAt(Token *token, const char *message) {
  if (parser.panicMode)
    return;

  parser.panicMode = true;

  fprintf(stderr, "\n[\033[0;33mline: %s%d%s] [\033[0;33mpos: %s%d%s] Error ", 
    Color(GREEN), token->line, Color(RESET), Color(GREEN), token->pos - 1, Color(RESET));

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, "at end\n");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, "at '%.*s'\n", token->length, token->start);
  }

  // Iterate over the tokens in the current line
  const char* lineStart = getSourceLineStart(token->line);
  const char* lineEnd = lineStart;
  while (*lineEnd != '\n' && *lineEnd != '\0') lineEnd++;

  // Print the line
  fprintf(stderr, "%s%.*s\n", Color(MAGENTA), (int)(lineEnd - lineStart), lineStart);

  // Print the pointer to the error
  int numSpaces = token->pos - 1;
  for (int i = 0; i < numSpaces; i++) fprintf(stderr, " ");
  fprintf(stderr, "\033[0;31m^^^\033[0m");

  // Print the error message
  fprintf(stderr, "\n%s\n", message);
  parser.hadError = true;
}


static void error(const char *message) { errorAt(&parser.previous, message); }

static void errorAtCurrent(const char *message) {
  errorAt(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR)
      break;

    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char *message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

static bool check(TokenType type) { return parser.current.type == type; }

static bool match(TokenType type) {
  if (!check(type))
    return false;
  advance();
  return true;
}

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

static int emitJump(OpCode opcode) {
  emitByte(opcode);
  emitByte(0xff);
  emitByte(0xff);
  return currentChunk()->count - 3; // Adjust for the three bytes emitted for the jump instruction
}


static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void emitLoop(int loopstart) {
  emitByte(OP_LOOP);

  size_t offset = currentChunk()->count - loopstart + 2;
  if (offset > UINT16_MAX) error("The loop body is to big!");

  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

static void emitReturn() { 
  if (current->type == TYPE_INITIALIZER) {
    emitBytes(OP_GET_LOCAL, 0);
  } else {
    emitByte(OP_NIL);
  }
  emitByte(OP_RETURN); 
}

static uint8_t makeConstant(Value value) {
  size_t constant = addConstants(currentChunk(), value);
  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk");
    return 0;
  }

  return (uint8_t)constant;
}

static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  currentChunk()->code[offset + 1] = (jump >> 8) & 0xff;
  currentChunk()->code[offset + 2] = jump & 0xff;
}

static void initCompiler(Compiler *compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->function = NULL;
  compiler->type = type;
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  compiler->function = newFunction();
  current = compiler;
  if (type != TYPE_SCRIPT) {
    current->function->name = copyString(parser.previous.start,
                                         parser.previous.length);
  }

  Local* local = &current->locals[current->localCount++];
  local->depth = 0;
  local->isCaptured = false;
  if(type != TYPE_FUNC) {
    local->name.start = "this";
    local->name.length = 4;
  } else {
    local->name.start = "";
    local->name.length = 0;
  }
}

static ObjFunction* endCompiler() {
  emitReturn();
  ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), function->name != NULL
      ? function->name->chars : "<script>");
  }
#endif

  current = current->enclosing;
  return function;
}

static void beginScope() { current->scopeDepth++; }

static void endScope() {
  current->scopeDepth--;

  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth > current->scopeDepth) {
    if (current->locals[current->localCount - 1].isCaptured) {
      emitByte(OP_CLOSE_UPVALUE);
    } else {
      emitByte(OP_POP);
    }
    current->localCount--;
  }
}

static void expression();
static void statement();
static void declaration();
static ParseRule *getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static void expression() { parsePrecedence(PREC_ASSINMENT); }

static uint8_t identifierConstant(Token *name) {
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifierEquals(Token *a, Token *b) {
  if (a->length != b->length)
    return false;
  return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler *compiler, Token *name) {
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local *local = &compiler->locals[i];
    if (identifierEquals(name, &local->name)) {
      if (local->depth == -1) {
        error("Can't read variable in its own initializer!");
      }
      return i;
    }
  }
  return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
  int upvalueCount = compiler->function->upvalueCount;
  for (int i = 0; i < upvalueCount; i++){
    Upvalue* upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->isLocal == isLocal) {
      return i;
    }
  }

  if (upvalueCount == UINT8_COUNT) {
    error("Too many closure variables in the function!");
    return 0;
  }
  
  compiler->upvalues[upvalueCount].isLocal = isLocal;
  compiler->upvalues[upvalueCount].index = index;
  return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
  if (compiler->enclosing == NULL) return -1;

  int local = resolveLocal(compiler->enclosing, name);
  if (local != -1) {
    compiler->enclosing->locals[local].isCaptured = true;
    return addUpvalue(compiler, (uint8_t)local, true);
  }
  int update = resolveUpvalue(compiler->enclosing, name);
  if (update != -1) {
    return addUpvalue(compiler, (uint8_t)update, false);
  }

  return -1;
}

static void addLocal(Token name) {
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in the function!");
    return;
  }

  Local *local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1;
  local->isCaptured = false;
}

static void declareVariable() {
  if (current->scopeDepth == 0)
    return;

  Token *name = &parser.previous;

  for (int i = current->localCount - 1; i >= 0; i--) {
    Local *local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break;
    }

    if (identifierEquals(name, &local->name)) {
      error("Already a variable with this name declared in the scope!");
    }
  }

  addLocal(*name);
}

static uint8_t parseVariable(const char *errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable();
  if (current->scopeDepth > 0)
    return 0;

  return identifierConstant(&parser.previous);
}

static void markInitialized() {
  if (current->scopeDepth == 0) return;
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }

  emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList() {
  uint8_t argCount = 0;
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();
      if (argCount == 255) {
        error("Can't have more than 255 arguments!");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expected ')' after the arguments list!");
  return argCount;
}

static void and_(bool canAssign) {
  int endjump = emitJump(OP_JUMP_IF_FALSE);

  emitByte(OP_POP);
  parsePrecedence(PREC_AND);

  patchJump(endjump);
}

static void grouping(bool canAssign) {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression!");
}

static void number(bool canAssign) {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endjump = emitJump(OP_JUMP);

  patchJump(elseJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endjump);
}

static void string(bool canAssign) {
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  int arg = resolveLocal(current, &name);

  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else if ((arg = resolveUpvalue(current, &name)) != -1) {
    getOp = OP_GET_UPVALUE;
    setOp = OP_SET_UPVALUE;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(setOp, (uint8_t)arg);
  } else {
    emitBytes(getOp, (uint8_t)arg);
  }
}

static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char* text) {
  Token token;
  token.start = text;
  token.length = (int)strlen(text);
  return token;
}

static void super_(bool canAssign) {
  if (currentClass == NULL) {
    error("Can't use 'super' outside of a class!");
  } else if (!currentClass->hasSuperClass) {
    error("Can't use 'super' in a class with no superclasses!");
  }

  consume(TOKEN_DOT, "Expected '.' after 'super' ");
  consume(TOKEN_IDENTIFIER, "Expected superclass method name!");
  uint8_t name = identifierConstant(&parser.previous);

  namedVariable(syntheticToken("this"), false);
  if (match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    namedVariable(syntheticToken("super"), false);
    emitBytes(OP_SUPER_INVOKE, name);
    emitByte(argCount);
  } else {
    namedVariable(syntheticToken("super"), false);
    emitBytes(OP_GET_SUPER, name);
  }
}

static void this_(bool canAssign) {
  if (currentClass == NULL) {
    error("Can't use 'this' operation outside of a class!");
    return;
  }
  variable(false);
}

static void literal(bool canAssign) {
  switch (parser.previous.type) {
  case TOKEN_FALSE:
    emitByte(OP_FALSE);
    break;
  case TOKEN_TRUE:
    emitByte(OP_TRUE);
    break;
  case TOKEN_NIL:
    emitByte(OP_NIL);
    break;
  default:
    return; // unreachable
  }
}

static void binary(bool canAssign) {
  TokenType operatorType = parser.previous.type;
  ParseRule* rule = getRule(operatorType);
  parsePrecedence((Precedence)(rule->precedence + 1));

  switch (operatorType) {
    // Types of Value comparison-operators
    case TOKEN_BANG_EQUALS:   emitBytes(OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
    case TOKEN_GREATER:       emitByte(OP_GREATER); break;
    case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
    case TOKEN_LESS:          emitByte(OP_LESS); break;
    case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
    // Types of Value arithmetic-operators
    case TOKEN_PLUS:          emitByte(OP_ADD); break;
    case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
    case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
    case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
    case TOKEN_PERCENT:       emitByte(OP_MODULO); break;
    default: return; // Unreachable.
  }
}


static void call(bool canAssign) {
  uint8_t argCount = argumentList();
  emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'");
  uint8_t name = identifierConstant(&parser.previous);

  if (canAssign && match(TOKEN_EQUAL))  {
    expression();
    emitBytes(OP_SET_PROPERTY, name);
  } else if(match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    emitBytes(OP_INVOKE, name);
    emitByte(argCount);
  }else {
    emitBytes(OP_GET_PROPERTY, name);
  }
}

static void unary(bool canAssign) {
  TokenType operationType = parser.previous.type;

  // compile the operand
  parsePrecedence(PREC_UNARY);

  // Emite te operation instrustion
  switch (operationType) {
  case TOKEN_BANG: emitByte(OP_NOT); break;
  case TOKEN_MINUS: emitByte(OP_NEGATE); break;
  default: return; // Unreachable;
  }
}


ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},

    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE]  = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA]       = {NULL, NULL, PREC_NONE},

    [TOKEN_DOT] = {NULL, dot, PREC_CALL},

    [TOKEN_MINUS]     = {unary, binary, PREC_TERM},
    [TOKEN_PLUS]      = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH]     = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR]      = {NULL, binary, PREC_FACTOR},
    [TOKEN_PERCENT]   = {NULL, binary, PREC_FACTOR},

    [TOKEN_BANG]        = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUALS] = {NULL, binary, PREC_EQUALITY},

    [TOKEN_WALRUS] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL]  = {NULL, NULL, PREC_NONE},

    [TOKEN_EQUAL_EQUAL]   = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL, binary, PREC_COMPARASION},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARASION},
    [TOKEN_LESS]          = {NULL, binary, PREC_COMPARASION},
    [TOKEN_LESS_EQUAL]    = {NULL, binary, PREC_COMPARASION},

    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING]     = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER]     = {number, NULL, PREC_NONE},

    [TOKEN_AND] = {NULL, and_, PREC_AND},

    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE]  = {NULL, NULL, PREC_NONE},

    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},

    [TOKEN_FOR]  = {NULL, NULL, PREC_NONE},
    [TOKEN_FUNC] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF]   = {NULL, NULL, PREC_NONE},

    [TOKEN_NIL] = {literal, NULL, PREC_NONE},

    [TOKEN_OR] = {NULL, or_, PREC_OR},

    [TOKEN_INFO]   = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},

    [TOKEN_SUPER] = {super_, NULL, PREC_NONE},

    [TOKEN_THIS]  = {this_, NULL, PREC_NONE},

    [TOKEN_TRUE]  = {literal, NULL, PREC_NONE},

    [TOKEN_VAR]    = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE]  = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR]  = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF]    = {NULL, NULL, PREC_NONE},
    [TOKEN_SWITCH] = {NULL, NULL, PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }

  bool canAssign = precedence <= PREC_ASSINMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target!");
  }
}

static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
  Compiler compiler;
  initCompiler(&compiler, type);
  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  // Check for parameters
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      current->function->arity++;
      // Max number of parameters is 255
      if (current->function->arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
      }
      // Parse the parameter name
      uint8_t paramConstant = parseVariable("Expect parameter name.");
      defineVariable(paramConstant);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  block();

  ObjFunction* function = endCompiler();
  emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));
  for (int i = 0; i < function->upvalueCount; i++) {
    emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
    emitByte(compiler.upvalues[i].index);
  }
}

static void method() {
  consume(TOKEN_IDENTIFIER, "Expected a method name!");
  uint8_t constant = identifierConstant(&parser.previous);
  
  FunctionType type = TYPE_METHOD;
  if(parser.previous.length == 4 && 
     memcmp(parser.previous.start, "init", 4) 
     == 0) {
      type = TYPE_INITIALIZER;
  }
  function(type);
  
  emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
  consume(TOKEN_IDENTIFIER, "Expected a class name!");
  Token className = parser.previous;
  uint8_t nameConstant = identifierConstant(&parser.previous);
  declareVariable();

  emitBytes(OP_CLASS, nameConstant);
  defineVariable(nameConstant);

  ClassCompiler classCompiler;
  classCompiler.hasSuperClass = false;
  classCompiler.enclosing = currentClass;
  currentClass = &classCompiler;

  // For super classes
  if (match(TOKEN_INHERITANCE)) {
    consume(TOKEN_IDENTIFIER, "expected a super class name!");
    variable(false);

    // This is were we need to check and see if it inherits self
    if (identifierEquals(&className, &parser.previous)) {
      error("A class can not inherit from itself!");
    }

    beginScope();
    addLocal(syntheticToken("super"));
    defineVariable(0);

    namedVariable(className, false);
    emitByte(OP_INHERIT);
    classCompiler.hasSuperClass = true;
  }

  namedVariable(className, false);
  consume(TOKEN_LEFT_BRACE, "Expected a '{' before class body!");
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    method();
  }
  consume(TOKEN_RIGHT_BRACE, "Expected a '}' after class body!");
  emitByte(OP_POP);

  if (classCompiler.hasSuperClass) {
    endScope();
  }

  currentClass = currentClass->enclosing;
}

static void funDeclaration() {
  uint8_t global = parseVariable("Expected a function name.");
  markInitialized();
  function(TYPE_FUNC);
  defineVariable(global);
}

static void varDeclaration() {
  uint8_t global = parseVariable("Expected a variable name!");

  if (match(TOKEN_WALRUS)) {
    expression();
  } else {
    emitByte(OP_NIL);
  }

  if (match(TOKEN_EQUAL)) {
    error("For assining a variable use the operation of ':='.\nFor example "
          "'have add := 45.2 + 2'. Happy coding!");
  }

  consume(TOKEN_SEMICOLON,
          "Expected ';' after the declaration! \nTry something "
          "like 'have add := 45.2 + 2;' happy coding!");

  defineVariable(global);
}

static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expected ';' after expression!");
  emitByte(OP_POP);
}

static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expected a '(' after the 'for' keyword!");

  if (match(TOKEN_SEMICOLON)) {
    // no initializer.
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }

  int loopStart = currentChunk()->count;

  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expected a ';' after the loop conditions");

    // Jump out of the loop if condition is false!
    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // condition
  }

  if (!match(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;
    expression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expected a ')' after the clause!");

    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  statement();
  emitLoop(loopStart);
  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP);
  }
  endScope();
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after if condition.");

  // Jump to the else branch if the condition is false.
  int elseJump = emitJump(OP_JUMP_IF_FALSE);

  // Emit code for the true branch.
  statement();

  // If there's an else block, jump to the end of the if statement.
  int endJump = emitJump(OP_JUMP);

  // Patch the else jump.
  patchJump(elseJump);

  // If there's an else block, emit code for it.
  if (match(TOKEN_ELSE)) {
    statement();
  }

  // Patch the end jump.
  patchJump(endJump);
}

static void synchronize() {
  parser.panicMode = false;

  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON) return;
    switch (parser.current.type) {
    case TOKEN_CLASS:
    case TOKEN_FUNC:
    case TOKEN_VAR:
    case TOKEN_FOR:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_INFO:
    case TOKEN_RETURN:
      return;

    default: // Do Nothing
        ;
    }

    advance();
  }
}

static void declaration() {
  if (match(TOKEN_CLASS)) {
    classDeclaration();
  } else if (match(TOKEN_FUNC)) {
    funDeclaration();
  } else if(match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    statement();
  }

  if (parser.panicMode) synchronize();
}

static void infoStatement() {
  expression();
  consume(TOKEN_SEMICOLON,
          "Expected ';' after the value in the info statement! \nTry something "
          "like 'info 1 + 1;' happy coding!");
  emitByte(OP_INFO);
}

static void returnStatement() {
  if (current->type == TYPE_SCRIPT) {
    error("Can't return from top-level code!");
  }
  if (match(TOKEN_SEMICOLON)) {
    emitReturn();
  } else {
    if (current->type == TYPE_INITIALIZER) {
      error("Can't return a value from an initializer!");
    }
    expression();
    consume(TOKEN_SEMICOLON,
            "Expected ';' after the return value!" 
            "\nTry this 'return 1 + 1;' Happy Coding!");
    emitByte(OP_RETURN);
  }
}

static void whileStatement() {
  int loopstart = currentChunk()->count;
  consume(TOKEN_LEFT_PAREN, "Expected a '(' after the 'while' keyword!");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expected a ')' after the while condition!");

  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();
  emitLoop(loopstart);

  patchJump(exitJump);
  emitByte(OP_POP);
}

static void switchStatement() {
  consume(TOKEN_LEFT_PAREN, "Expected a '(' after the switch keyword!");
  expression();
  consume(TOKEN_RIGHT_PAREN,
          "Expected a ')' after the value of the switch statement!");
  consume(TOKEN_LEFT_BRACE, "Expected a '{' before the switch cases!");

  int state =
      0; // 0: before all of the cases, 1: before default, 2: after the default:
  int casesEnd[MAX_CASES];
  int caseCount = 0;
  int previousCaseSkip = -1;

  while (!match(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    if (match(TOKEN_CASE) || match(TOKEN_DEFAULT)) {
      TokenType caseType = parser.previous.type;

      if (state == 2) {
        error("Can not have another case or default! After the default case!");
      }

      if (state == 1) {
        // At the end of the previous case, jump over the others.
        casesEnd[caseCount++] = emitJump(OP_JUMP);

        // Patch its condition to jump to the next case (this one).
        patchJump(previousCaseSkip);
        emitByte(OP_POP);
      }

      if (caseType == TOKEN_CASE) {
        state = 1;

        // See if the case is equal to the value
        emitByte(OP_DUP);
        expression();

        consume(TOKEN_COLON, "Expected a ':' after the case values!");

        emitByte(OP_EQUAL);
        previousCaseSkip = emitJump(OP_JUMP_IF_FALSE);

        // Pop the comparison result.
        emitByte(OP_POP);
      } else {
        state = 2;
        consume(TOKEN_COLON, "Expected a ':' after the default case!");
        previousCaseSkip = -1;
      }
    } else {
      // Otherwise, it is a statement inside trhe current case.
      if (state == 0) {
        error("Can not have statements before any cases!");
      }
      statement();
    }
  }

  // If we ended without a default case, patch its jump condition
  if (state == 1) {
    patchJump(previousCaseSkip);
    emitByte(OP_POP);
  }

  // Patch all the cases of jump to the end
  for (int i = 0; i < caseCount; i++) {
    patchJump(casesEnd[i]);
  }

  emitByte(OP_POP); // The switch value
}

static void statement() {
  if (match(TOKEN_INFO)) {
    infoStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_RETURN)) {
    returnStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_SWITCH)) {
    switchStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

static ParseRule *getRule(TokenType type) { return &rules[type]; }

ObjFunction* compile(const char* source) {
  initScanner(source);
  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);

  parser.hadError = false;
  parser.panicMode = false;

  advance();

  while (!match(TOKEN_EOF)) {
    declaration();
  }

  ObjFunction* function = endCompiler();
  return parser.hadError ? NULL : function;
}