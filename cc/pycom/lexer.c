/*
 * Python Lexer (Tokenizer) Implementation
 */

#include "pycom.h"
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

/* Keyword table */
static struct {
    const char *name;
    TokenType type;
} keywords[] = {
    {"def", TOK_DEF},
    {"class", TOK_CLASS},
    {"if", TOK_IF},
    {"elif", TOK_ELIF},
    {"else", TOK_ELSE},
    {"while", TOK_WHILE},
    {"for", TOK_FOR},
    {"in", TOK_IN},
    {"return", TOK_RETURN},
    {"pass", TOK_PASS},
    {"break", TOK_BREAK},
    {"continue", TOK_CONTINUE},
    {"import", TOK_IMPORT},
    {"from", TOK_FROM},
    {"as", TOK_AS},
    {"and", TOK_AND},
    {"or", TOK_OR},
    {"not", TOK_NOT},
    {"True", TOK_TRUE},
    {"False", TOK_FALSE},
    {"None", TOK_NONE},
    {"lambda", TOK_LAMBDA},
    {"try", TOK_TRY},
    {"except", TOK_EXCEPT},
    {"finally", TOK_FINALLY},
    {"raise", TOK_RAISE},
    {"with", TOK_WITH},
    {"yield", TOK_YIELD},
    {"assert", TOK_ASSERT},
    {"global", TOK_GLOBAL},
    {"nonlocal", TOK_NONLOCAL},
    {"del", TOK_DEL},
    {"is", TOK_IS},
    {NULL, 0}
};

Lexer *lexer_new(const char *source) {
    Lexer *lexer = xmalloc(sizeof(Lexer));
    lexer->source = source;
    lexer->pos = 0;
    lexer->line = 1;
    lexer->column = 1;
    lexer->indent_stack = xmalloc(sizeof(int) * 32);
    lexer->indent_stack[0] = 0;
    lexer->indent_count = 1;
    lexer->indent_capacity = 32;
    lexer->pending_dedents = 0;

    /* Initialize lookahead */
    lexer->current.type = TOK_EOF;
    lexer->current.value = NULL;

    return lexer;
}

void lexer_free(Lexer *lexer) {
    if (lexer) {
        free(lexer->indent_stack);
        free(lexer);
    }
}

static char lexer_current_char(Lexer *lexer) {
    if (lexer->pos >= strlen(lexer->source)) {
        return '\0';
    }
    return lexer->source[lexer->pos];
}

static char lexer_peek_char(Lexer *lexer, int offset) {
    size_t pos = lexer->pos + offset;
    if (pos >= strlen(lexer->source)) {
        return '\0';
    }
    return lexer->source[pos];
}

static void lexer_advance(Lexer *lexer) {
    if (lexer->pos < strlen(lexer->source)) {
        if (lexer->source[lexer->pos] == '\n') {
            lexer->line++;
            lexer->column = 1;
        } else {
            lexer->column++;
        }
        lexer->pos++;
    }
}

static void lexer_skip_whitespace(Lexer *lexer) {
    while (lexer_current_char(lexer) == ' ' ||
           lexer_current_char(lexer) == '\t' ||
           lexer_current_char(lexer) == '\r') {
        lexer_advance(lexer);
    }
}

static void lexer_skip_comment(Lexer *lexer) {
    if (lexer_current_char(lexer) == '#') {
        while (lexer_current_char(lexer) != '\n' && lexer_current_char(lexer) != '\0') {
            lexer_advance(lexer);
        }
    }
}

static Token make_token(TokenType type, const char *value, int line, int column) {
    Token token;
    token.type = type;
    token.value = value ? xstrdup(value) : NULL;
    token.line = line;
    token.column = column;
    return token;
}

static TokenType lookup_keyword(const char *name) {
    for (int i = 0; keywords[i].name != NULL; i++) {
        if (strcmp(keywords[i].name, name) == 0) {
            return keywords[i].type;
        }
    }
    return TOK_IDENTIFIER;
}

static Token lexer_read_identifier(Lexer *lexer) {
    int start_line = lexer->line;
    int start_column = lexer->column;
    size_t start = lexer->pos;

    while (isalnum(lexer_current_char(lexer)) || lexer_current_char(lexer) == '_') {
        lexer_advance(lexer);
    }

    size_t length = lexer->pos - start;
    char *value = xmalloc(length + 1);
    strncpy(value, &lexer->source[start], length);
    value[length] = '\0';

    TokenType type = lookup_keyword(value);
    Token token = make_token(type, value, start_line, start_column);
    free(value);

    return token;
}

static Token lexer_read_number(Lexer *lexer) {
    int start_line = lexer->line;
    int start_column = lexer->column;
    size_t start = lexer->pos;

    while (isdigit(lexer_current_char(lexer))) {
        lexer_advance(lexer);
    }

    /* Handle floating point */
    if (lexer_current_char(lexer) == '.' && isdigit(lexer_peek_char(lexer, 1))) {
        lexer_advance(lexer); /* skip '.' */
        while (isdigit(lexer_current_char(lexer))) {
            lexer_advance(lexer);
        }
    }

    /* Handle scientific notation */
    if (lexer_current_char(lexer) == 'e' || lexer_current_char(lexer) == 'E') {
        lexer_advance(lexer);
        if (lexer_current_char(lexer) == '+' || lexer_current_char(lexer) == '-') {
            lexer_advance(lexer);
        }
        while (isdigit(lexer_current_char(lexer))) {
            lexer_advance(lexer);
        }
    }

    size_t length = lexer->pos - start;
    char *value = xmalloc(length + 1);
    strncpy(value, &lexer->source[start], length);
    value[length] = '\0';

    Token token = make_token(TOK_NUMBER, value, start_line, start_column);
    token.literal.int_val = atoll(value);
    free(value);

    return token;
}

static Token lexer_read_string(Lexer *lexer) {
    int start_line = lexer->line;
    int start_column = lexer->column;
    char quote = lexer_current_char(lexer);
    lexer_advance(lexer); /* skip opening quote */

    size_t start = lexer->pos;

    while (lexer_current_char(lexer) != quote && lexer_current_char(lexer) != '\0') {
        if (lexer_current_char(lexer) == '\\') {
            lexer_advance(lexer); /* skip escape */
        }
        lexer_advance(lexer);
    }

    size_t length = lexer->pos - start;
    char *value = xmalloc(length + 1);
    strncpy(value, &lexer->source[start], length);
    value[length] = '\0';

    if (lexer_current_char(lexer) == quote) {
        lexer_advance(lexer); /* skip closing quote */
    }

    Token token = make_token(TOK_STRING, value, start_line, start_column);
    free(value);

    return token;
}

static int lexer_count_indent(Lexer *lexer) {
    int indent = 0;
    size_t start_pos = lexer->pos;

    while (lexer_current_char(lexer) == ' ' || lexer_current_char(lexer) == '\t') {
        if (lexer_current_char(lexer) == ' ') {
            indent++;
        } else { /* tab */
            indent += 8;
        }
        lexer_advance(lexer);
    }

    /* If line is empty or comment, ignore indent */
    if (lexer_current_char(lexer) == '\n' ||
        lexer_current_char(lexer) == '#' ||
        lexer_current_char(lexer) == '\0') {
        lexer->pos = start_pos;
        return -1;
    }

    return indent;
}

Token lexer_next_token(Lexer *lexer) {
    /* Handle pending dedents */
    if (lexer->pending_dedents > 0) {
        lexer->pending_dedents--;
        return make_token(TOK_DEDENT, NULL, lexer->line, lexer->column);
    }

    lexer_skip_whitespace(lexer);
    lexer_skip_comment(lexer);

    if (lexer_current_char(lexer) == '\0') {
        /* Generate dedents for remaining indent levels */
        if (lexer->indent_count > 1) {
            lexer->indent_count--;
            return make_token(TOK_DEDENT, NULL, lexer->line, lexer->column);
        }
        return make_token(TOK_EOF, NULL, lexer->line, lexer->column);
    }

    /* Handle newlines and indentation */
    if (lexer_current_char(lexer) == '\n') {
        int line = lexer->line;
        int column = lexer->column;
        lexer_advance(lexer);

        /* Skip blank lines and comments */
        while (lexer_current_char(lexer) == '\n' || lexer_current_char(lexer) == '#') {
            if (lexer_current_char(lexer) == '#') {
                lexer_skip_comment(lexer);
            }
            if (lexer_current_char(lexer) == '\n') {
                lexer_advance(lexer);
            }
        }

        int indent = lexer_count_indent(lexer);
        if (indent == -1) {
            /* Empty line, continue */
            return lexer_next_token(lexer);
        }

        int current_indent = lexer->indent_stack[lexer->indent_count - 1];

        if (indent > current_indent) {
            /* INDENT */
            if (lexer->indent_count >= lexer->indent_capacity) {
                lexer->indent_capacity *= 2;
                lexer->indent_stack = xrealloc(lexer->indent_stack,
                                              sizeof(int) * lexer->indent_capacity);
            }
            lexer->indent_stack[lexer->indent_count++] = indent;
            return make_token(TOK_INDENT, NULL, line, column);
        } else if (indent < current_indent) {
            /* DEDENT(s) */
            int dedents = 0;
            while (lexer->indent_count > 1 &&
                   lexer->indent_stack[lexer->indent_count - 1] > indent) {
                lexer->indent_count--;
                dedents++;
            }

            if (lexer->indent_stack[lexer->indent_count - 1] != indent) {
                error("Indentation error at line %d", lexer->line);
            }

            lexer->pending_dedents = dedents - 1;
            return make_token(TOK_DEDENT, NULL, line, column);
        }

        return make_token(TOK_NEWLINE, NULL, line, column);
    }

    int line = lexer->line;
    int column = lexer->column;
    char c = lexer_current_char(lexer);

    /* Identifiers and keywords */
    if (isalpha(c) || c == '_') {
        return lexer_read_identifier(lexer);
    }

    /* Numbers */
    if (isdigit(c)) {
        return lexer_read_number(lexer);
    }

    /* Strings */
    if (c == '"' || c == '\'') {
        return lexer_read_string(lexer);
    }

    /* Operators and delimiters */
    lexer_advance(lexer);

    switch (c) {
        case '+':
            if (lexer_current_char(lexer) == '=') {
                lexer_advance(lexer);
                return make_token(TOK_PLUS_ASSIGN, NULL, line, column);
            }
            return make_token(TOK_PLUS, NULL, line, column);
        case '-':
            if (lexer_current_char(lexer) == '=') {
                lexer_advance(lexer);
                return make_token(TOK_MINUS_ASSIGN, NULL, line, column);
            }
            if (lexer_current_char(lexer) == '>') {
                lexer_advance(lexer);
                return make_token(TOK_ARROW, NULL, line, column);
            }
            return make_token(TOK_MINUS, NULL, line, column);
        case '*':
            if (lexer_current_char(lexer) == '*') {
                lexer_advance(lexer);
                return make_token(TOK_POWER, NULL, line, column);
            }
            if (lexer_current_char(lexer) == '=') {
                lexer_advance(lexer);
                return make_token(TOK_STAR_ASSIGN, NULL, line, column);
            }
            return make_token(TOK_STAR, NULL, line, column);
        case '/':
            if (lexer_current_char(lexer) == '/') {
                lexer_advance(lexer);
                return make_token(TOK_FLOOR_DIV, NULL, line, column);
            }
            if (lexer_current_char(lexer) == '=') {
                lexer_advance(lexer);
                return make_token(TOK_SLASH_ASSIGN, NULL, line, column);
            }
            return make_token(TOK_SLASH, NULL, line, column);
        case '%':
            return make_token(TOK_PERCENT, NULL, line, column);
        case '=':
            if (lexer_current_char(lexer) == '=') {
                lexer_advance(lexer);
                return make_token(TOK_EQ, NULL, line, column);
            }
            return make_token(TOK_ASSIGN, NULL, line, column);
        case '!':
            if (lexer_current_char(lexer) == '=') {
                lexer_advance(lexer);
                return make_token(TOK_NE, NULL, line, column);
            }
            break;
        case '<':
            if (lexer_current_char(lexer) == '=') {
                lexer_advance(lexer);
                return make_token(TOK_LE, NULL, line, column);
            }
            if (lexer_current_char(lexer) == '<') {
                lexer_advance(lexer);
                return make_token(TOK_LSHIFT, NULL, line, column);
            }
            return make_token(TOK_LT, NULL, line, column);
        case '>':
            if (lexer_current_char(lexer) == '=') {
                lexer_advance(lexer);
                return make_token(TOK_GE, NULL, line, column);
            }
            if (lexer_current_char(lexer) == '>') {
                lexer_advance(lexer);
                return make_token(TOK_RSHIFT, NULL, line, column);
            }
            return make_token(TOK_GT, NULL, line, column);
        case '&':
            return make_token(TOK_AMPERSAND, NULL, line, column);
        case '|':
            return make_token(TOK_PIPE, NULL, line, column);
        case '^':
            return make_token(TOK_CARET, NULL, line, column);
        case '~':
            return make_token(TOK_TILDE, NULL, line, column);
        case '(':
            return make_token(TOK_LPAREN, NULL, line, column);
        case ')':
            return make_token(TOK_RPAREN, NULL, line, column);
        case '[':
            return make_token(TOK_LBRACKET, NULL, line, column);
        case ']':
            return make_token(TOK_RBRACKET, NULL, line, column);
        case '{':
            return make_token(TOK_LBRACE, NULL, line, column);
        case '}':
            return make_token(TOK_RBRACE, NULL, line, column);
        case ',':
            return make_token(TOK_COMMA, NULL, line, column);
        case ':':
            return make_token(TOK_COLON, NULL, line, column);
        case ';':
            return make_token(TOK_SEMICOLON, NULL, line, column);
        case '.':
            return make_token(TOK_DOT, NULL, line, column);
    }

    error("Unexpected character '%c' at line %d, column %d", c, line, column);
    return make_token(TOK_EOF, NULL, line, column);
}

Token lexer_peek_token(Lexer *lexer) {
    /* Save current state */
    size_t saved_pos = lexer->pos;
    size_t saved_line = lexer->line;
    size_t saved_column = lexer->column;
    int saved_pending = lexer->pending_dedents;

    Token token = lexer_next_token(lexer);

    /* Restore state */
    lexer->pos = saved_pos;
    lexer->line = saved_line;
    lexer->column = saved_column;
    lexer->pending_dedents = saved_pending;

    return token;
}
