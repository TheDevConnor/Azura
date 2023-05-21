typedef enum {
    BLACK,
    GRAY,
    RED,
    GREEN,
    YELLOW,
    BLUE,
    MAGENTA,
    CYAN,
    WHITE,
    RESET
} TerminalColor;

const char* Color(TerminalColor color);