#include <stdlib.h>
#include <string.h>

typedef enum {
    LEX_EOF,
    LEX_ERROR,
    LEX_IDENTIFIER,
    LEX_EQUAL,
    LEX_POINTER_ASSIGN,
    LEX_INTEGER,
    LEX_NEWLINE,
    LEX_OTHER
} TokenType;

typedef struct {
    TokenType type;
    const char *start;
    const char *end;
    int line;
} Token;

typedef struct {
    const char *buffer;
    int length;
    int pos;
    int line;
} LexerState;

/*!re2c
    re2c:api:style = free-form;
    re2c:define:YYCTYPE = "unsigned char";
    re2c:define:YYCURSOR = cur;
    re2c:define:YYLIMIT = lim;
    re2c:define:YYMARKER = mar;
    re2c:define:YYCTXMARKER = ctx;
    re2c:yyfill:enable = 0;

    end = "\x00";
    ws = [ \t];
    nl = "\n" | "\r\n" | ";";
    digit = [0-9];
    letter = [a-zA-Z];
    underscore = "_";
    ident = (letter | underscore) (letter | digit | underscore)*;
    integer = digit+;
    comment = "!" [^\n\r]*;
*/

static void skip_line_continuation(const char **cur, const char *lim, int *line) {
    const char *mar, *ctx;
    const char *p = *cur;
    
skip:
    /*!re2c
    re2c:define:YYCURSOR = p;
    re2c:define:YYLIMIT = lim;
    re2c:yyfill:enable = 0;

    ws+ { goto skip; }
    comment { goto skip; }
    nl { *line += 1; goto skip; }
    "&" { goto skip; }
    * { *cur = p - 1; return; }
    */
}

Token lex(const char *str, int len, int *line) {
    static Token token;
    const char *cur = str, *lim = str + len;
    const char *mar, *ctx;
    
    while (1) {
        token.start = cur;
        token.line = *line;
        
        /*!re2c
        re2c:define:YYCURSOR = cur;
        re2c:define:YYLIMIT = lim;
        re2c:define:YYMARKER = mar;
        re2c:define:YYCTXMARKER = ctx;
        re2c:yyfill:enable = 0;
        
        ws+ { continue; }
        nl { *line += 1; token.type = LEX_NEWLINE; goto token_end; }
        comment { continue; }
        
        "&" { skip_line_continuation(&cur, lim, line); continue; }
        
        "=>" { token.type = LEX_POINTER_ASSIGN; goto token_end; }
        "=" { token.type = LEX_EQUAL; goto token_end; }
        
        ident { token.type = LEX_IDENTIFIER; goto token_end; }
        integer { token.type = LEX_INTEGER; goto token_end; }
        
        end { token.type = LEX_EOF; goto token_end; }
        * { token.type = LEX_OTHER; goto token_end; }
        */
    }
    
token_end:
    token.end = cur;
    return token;
}

void next_token_c(const char* buffer, int length, int* line, 
                  int* type, int* start, int* end) {
    Token t = lex(buffer, length, line);
    *type = t.type;
    *start = t.start - buffer;  // Calculate offset
    *end = t.end - buffer;
}
