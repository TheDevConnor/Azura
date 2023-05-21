#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "common.h"
#include "scanner.h"

typedef struct {
  const char *start;
  const char *current;
  int line;
  const char* source; // New field to store the source code
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
  scanner.source = source;
}

const char* getSourceLineStart(int line) {
  const char* current = scanner.source;
  int currentLine = 1;

  while (currentLine < line && *current != '\0') {
    if (*current == '\n') {
      currentLine++;
    }
    current++;
  }

  // Include leading whitespaces on the line
  while (*current != '\n' && *current != '\0' && isspace(*current)) {
    current--;
  }
  if (*current == '\n') {
    current++;
  }

  return current;
}

static bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool isDigit(char c) { return c >= '0' && c <= '9'; }

static bool isAtEnd() { return *scanner.current == '\0'; }

static char advance() {
  scanner.current++;
  return scanner.current[-1];
}

static char peek() { return *scanner.current; }

static char peekNext() {
  if (isAtEnd())
    return '\0';
  return scanner.current[1];
}

static bool match(char expected) {
  if (isAtEnd())
    return false;
  if (*scanner.current != expected)
    return false;
  scanner.current++;
  return true;
}

static Token makeToken(TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner.start;
  token.length = (int)(scanner.current - scanner.start);
  token.line = scanner.line;
  token.pos = (int)(scanner.start - getSourceLineStart(token.line));
  return token;
}

static Token errorToken(const char *message) {
  Token token;
  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = (int)strlen(message);
  token.line = scanner.line;
  return token;
}

static void skipWhiteSpace() {
  for (;;) {
    char c = peek();
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        advance();
        break;
      case '\n':
        scanner.line++;
        advance();
        break;
      case '/':
        if (peekNext() == '/') {
          while (peek() != '\n' && !isAtEnd())
            advance();
        } else if (peekNext() == '*') {
          // TODO: Add block comments handling
          return;
        } else {
          return;
        }
        break;
      default:
        return;
    }
  }
}

static TokenType checkKeyword(int start, int length, const char *rest,
                              TokenType type) {
  if (scanner.current - scanner.start == start + length &&
      memcmp(scanner.start + start, rest, length) == 0) {
    return type;
  }
  return TOKEN_IDENTIFIER;
}

static TokenType identifierType() {
  switch (scanner.start[0]) {
  case 'a':
    return checkKeyword(1, 2, "nd", TOKEN_AND);
  case 'c':
    if (scanner.current - scanner.start > 1) {
      switch (scanner.start[1]) {
      case 'l':
        return checkKeyword(2, 3, "ass", TOKEN_CLASS);
      case 'a':
        return checkKeyword(2, 2, "se", TOKEN_CASE);
      }
    }
    break;
  case 'd':
    return checkKeyword(1, 6, "efault", TOKEN_DEFAULT);
  case 'e':
    return checkKeyword(1, 3, "lse", TOKEN_ELSE);
  case 'i':
    if (scanner.current - scanner.start > 1) {
      switch (scanner.start[1]) {
      case 'f':
        return checkKeyword(2, 0, "", TOKEN_IF);
      case 'n':
        return checkKeyword(2, 2, "fo", TOKEN_INFO);
      }
    }
    break;
  case 'f':
    if (scanner.current - scanner.start > 1) {
      switch (scanner.start[1]) {
      case 'a':
        return checkKeyword(2, 3, "lse", TOKEN_FALSE);
      case 'o':
        return checkKeyword(2, 1, "r", TOKEN_FOR);
      case 'u':
        return checkKeyword(2, 2, "nc", TOKEN_FUNC);
      }
    }
    break;
  case 't':
    if (scanner.current - scanner.start > 1) {
      switch (scanner.start[1]) {
      case 'h':
        return checkKeyword(2, 2, "is", TOKEN_THIS);
      case 'r':
        return checkKeyword(2, 2, "ue", TOKEN_TRUE);
      }
    }
    break;
  case 'n':
    return checkKeyword(1, 2, "il", TOKEN_NIL);
  case 'o':
    return checkKeyword(1, 1, "r", TOKEN_OR);
  case 'r':
    return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
  case 's':
    if (scanner.current - scanner.start > 1) {
      switch (scanner.start[1]) {
      case 'u':
        return checkKeyword(2, 3, "per", TOKEN_SUPER);
      case 'w':
        return checkKeyword(2, 4, "itch", TOKEN_SWITCH);
      }
    }
    break;
  case 'h':
    return checkKeyword(1, 3, "ave", TOKEN_VAR);
  case 'w':
    return checkKeyword(1, 4, "hile", TOKEN_WHILE);
  }
  return TOKEN_IDENTIFIER;
}

static Token identifier() {
  while (isAlpha(peek()) || isDigit(peek()))
    advance();
  return makeToken(identifierType());
}

static Token number() {
  while (isDigit(peek()))
    advance();

  // Look for the fractional part
  if (peek() == '.' && isDigit(peekNext())) {
    // cosumes the dot
    advance();

    while (isDigit(peek()))
      advance();
  }
  return makeToken(TOKEN_NUMBER);
}

static Token string() {
  while ((peek() != '"') && !isAtEnd()) {
    if (peek() == '\n')
      scanner.line++;
    advance();
  }

  if (isAtEnd())
    return errorToken("Unterminated String");

  advance();
  return makeToken(TOKEN_STRING);
}

Token scanToken() {
  skipWhiteSpace();
  scanner.start = scanner.current;

  if (isAtEnd())
    return makeToken(TOKEN_EOF);

  char c = advance();

  if (isAlpha(c))
    return identifier();
  if (isDigit(c))
    return number();

  switch (c) {
  case ')':
    return makeToken(TOKEN_RIGHT_PAREN);
  case '(':
    return makeToken(TOKEN_LEFT_PAREN);
  case '{':
    return makeToken(TOKEN_LEFT_BRACE);
  case '}':
    return makeToken(TOKEN_RIGHT_BRACE);
  case ';':
    return makeToken(TOKEN_SEMICOLON);
  case ',':
    return makeToken(TOKEN_COMMA);
  case '.':
    return makeToken(TOKEN_DOT);
  case '+':
    return makeToken(TOKEN_PLUS);
  case '/':
    return makeToken(TOKEN_SLASH);
  case '*':
    return makeToken(TOKEN_STAR);
  case '%':
    return makeToken(TOKEN_PERCENT);
  case '#':
    return makeToken(TOKEN_HASTAG);
  case '-':
    return makeToken(match('>') ? TOKEN_INHERITANCE : TOKEN_MINUS);
  case '!':
    return makeToken(match('=') ? TOKEN_BANG_EQUALS : TOKEN_BANG);
  case '=':
    return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
  case ':':
    return makeToken(match('=') ? TOKEN_WALRUS : TOKEN_COLON);
  case '>':
    return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
  case '<':
    return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
  case '"':
    return string();
  }
  errorToken("Unexpected character.");
  return makeToken(TOKEN_ERROR);
}