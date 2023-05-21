#ifndef azura_scanner_h
#define azura_scanner_h

typedef enum {
  // Single-character tokens;
  TOKEN_LEFT_PAREN, // (
  TOKEN_RIGHT_PAREN, // )
  TOKEN_LEFT_BRACE,
  TOKEN_RIGHT_BRACE,
  TOKEN_COMMA,
  TOKEN_DOT,
  TOKEN_MINUS,
  TOKEN_PLUS,
  TOKEN_SEMICOLON,
  TOKEN_SLASH,
  TOKEN_PERCENT,
  TOKEN_STAR,
  TOKEN_COLON,
  TOKEN_HASTAG,
  // One or two character tokens
  TOKEN_INHERITANCE,
  TOKEN_BANG,
  TOKEN_BANG_EQUALS,
  TOKEN_EQUAL,
  TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER,
  TOKEN_GREATER_EQUAL,
  TOKEN_LESS,
  TOKEN_LESS_EQUAL,
  TOKEN_WALRUS,
  // Literals
  TOKEN_IDENTIFIER,
  TOKEN_STRING,
  TOKEN_NUMBER,
  // Keywords
  TOKEN_AND,
  TOKEN_CLASS,
  TOKEN_ELSE,
  TOKEN_FALSE,
  TOKEN_FOR,
  TOKEN_FUNC,
  TOKEN_IF,
  TOKEN_NIL,
  TOKEN_OR,
  TOKEN_INFO,
  TOKEN_RETURN,
  TOKEN_SUPER,
  TOKEN_THIS,
  TOKEN_TRUE,
  TOKEN_VAR,
  TOKEN_WHILE,
  TOKEN_CONTINUE,
  TOKEN_INTERPOLATEION,
  TOKEN_SWITCH,
  TOKEN_DEFAULT,
  TOKEN_CASE,

  TOKEN_ERROR,
  TOKEN_EOF
} TokenType;

typedef struct {
  TokenType type;
  const char *start;
  int length;
  int line;
  int pos;
} Token;

void initScanner(const char *source);
const char* getSourceLineStart(int line);
Token scanToken();

#endif
