/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "cbolgrammar.y"

/*
 * COBOL grammar with OO extensions
 * Supports DEC, IBM, HP, and Microsoft dialects
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

extern int yylex(void);
extern void yyerror(const char *s);

static struct cobol_class *current_class = NULL;
static struct cobol_method *current_method = NULL;


#line 90 "gram.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "gram.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_IDENTIFIER = 3,                 /* IDENTIFIER  */
  YYSYMBOL_STRING_LITERAL = 4,             /* STRING_LITERAL  */
  YYSYMBOL_INTEGER = 5,                    /* INTEGER  */
  YYSYMBOL_LEVEL_NUMBER = 6,               /* LEVEL_NUMBER  */
  YYSYMBOL_FLOAT = 7,                      /* FLOAT  */
  YYSYMBOL_IDENTIFICATION_DIVISION = 8,    /* IDENTIFICATION_DIVISION  */
  YYSYMBOL_ENVIRONMENT_DIVISION = 9,       /* ENVIRONMENT_DIVISION  */
  YYSYMBOL_DATA_DIVISION = 10,             /* DATA_DIVISION  */
  YYSYMBOL_PROCEDURE_DIVISION = 11,        /* PROCEDURE_DIVISION  */
  YYSYMBOL_CONFIGURATION_SECTION = 12,     /* CONFIGURATION_SECTION  */
  YYSYMBOL_INPUT_OUTPUT_SECTION = 13,      /* INPUT_OUTPUT_SECTION  */
  YYSYMBOL_FILE_SECTION = 14,              /* FILE_SECTION  */
  YYSYMBOL_WORKING_STORAGE_SECTION = 15,   /* WORKING_STORAGE_SECTION  */
  YYSYMBOL_LOCAL_STORAGE_SECTION = 16,     /* LOCAL_STORAGE_SECTION  */
  YYSYMBOL_LINKAGE_SECTION = 17,           /* LINKAGE_SECTION  */
  YYSYMBOL_CLASS = 18,                     /* CLASS  */
  YYSYMBOL_CLASS_ID = 19,                  /* CLASS_ID  */
  YYSYMBOL_METHOD = 20,                    /* METHOD  */
  YYSYMBOL_METHOD_ID = 21,                 /* METHOD_ID  */
  YYSYMBOL_OBJECT = 22,                    /* OBJECT  */
  YYSYMBOL_INHERITS = 23,                  /* INHERITS  */
  YYSYMBOL_INTERFACE = 24,                 /* INTERFACE  */
  YYSYMBOL_INTERFACE_ID = 25,              /* INTERFACE_ID  */
  YYSYMBOL_FACTORY = 26,                   /* FACTORY  */
  YYSYMBOL_INVOKE = 27,                    /* INVOKE  */
  YYSYMBOL_NEW = 28,                       /* NEW  */
  YYSYMBOL_SELF = 29,                      /* SELF  */
  YYSYMBOL_SUPER = 30,                     /* SUPER  */
  YYSYMBOL_PROPERTY = 31,                  /* PROPERTY  */
  YYSYMBOL_STATIC = 32,                    /* STATIC  */
  YYSYMBOL_INSTANCE = 33,                  /* INSTANCE  */
  YYSYMBOL_FINAL = 34,                     /* FINAL  */
  YYSYMBOL_ABSTRACT = 35,                  /* ABSTRACT  */
  YYSYMBOL_OVERRIDE = 36,                  /* OVERRIDE  */
  YYSYMBOL_PIC = 37,                       /* PIC  */
  YYSYMBOL_VALUE = 38,                     /* VALUE  */
  YYSYMBOL_REDEFINES = 39,                 /* REDEFINES  */
  YYSYMBOL_OCCURS = 40,                    /* OCCURS  */
  YYSYMBOL_TIMES = 41,                     /* TIMES  */
  YYSYMBOL_DEPENDING_ON = 42,              /* DEPENDING_ON  */
  YYSYMBOL_INDEXED_BY = 43,                /* INDEXED_BY  */
  YYSYMBOL_USAGE = 44,                     /* USAGE  */
  YYSYMBOL_COMP = 45,                      /* COMP  */
  YYSYMBOL_COMP_1 = 46,                    /* COMP_1  */
  YYSYMBOL_COMP_2 = 47,                    /* COMP_2  */
  YYSYMBOL_COMP_3 = 48,                    /* COMP_3  */
  YYSYMBOL_COMP_4 = 49,                    /* COMP_4  */
  YYSYMBOL_COMP_5 = 50,                    /* COMP_5  */
  YYSYMBOL_BINARY = 51,                    /* BINARY  */
  YYSYMBOL_PACKED_DECIMAL = 52,            /* PACKED_DECIMAL  */
  YYSYMBOL_DISPLAY_USAGE = 53,             /* DISPLAY_USAGE  */
  YYSYMBOL_POINTER = 54,                   /* POINTER  */
  YYSYMBOL_OBJECT_REFERENCE = 55,          /* OBJECT_REFERENCE  */
  YYSYMBOL_ACCEPT = 56,                    /* ACCEPT  */
  YYSYMBOL_ADD = 57,                       /* ADD  */
  YYSYMBOL_CALL = 58,                      /* CALL  */
  YYSYMBOL_COMPUTE = 59,                   /* COMPUTE  */
  YYSYMBOL_DELETE = 60,                    /* DELETE  */
  YYSYMBOL_DISPLAY = 61,                   /* DISPLAY  */
  YYSYMBOL_DIVIDE_OP = 62,                 /* DIVIDE_OP  */
  YYSYMBOL_EVALUATE = 63,                  /* EVALUATE  */
  YYSYMBOL_EXIT = 64,                      /* EXIT  */
  YYSYMBOL_GOTO = 65,                      /* GOTO  */
  YYSYMBOL_IF = 66,                        /* IF  */
  YYSYMBOL_ELSE = 67,                      /* ELSE  */
  YYSYMBOL_END_IF = 68,                    /* END_IF  */
  YYSYMBOL_MOVE = 69,                      /* MOVE  */
  YYSYMBOL_MULTIPLY = 70,                  /* MULTIPLY  */
  YYSYMBOL_PERFORM = 71,                   /* PERFORM  */
  YYSYMBOL_END_PERFORM = 72,               /* END_PERFORM  */
  YYSYMBOL_READ = 73,                      /* READ  */
  YYSYMBOL_RETURN = 74,                    /* RETURN  */
  YYSYMBOL_RETURNING = 75,                 /* RETURNING  */
  YYSYMBOL_REWRITE = 76,                   /* REWRITE  */
  YYSYMBOL_SEARCH = 77,                    /* SEARCH  */
  YYSYMBOL_SET = 78,                       /* SET  */
  YYSYMBOL_STOP = 79,                      /* STOP  */
  YYSYMBOL_STRING_STMT = 80,               /* STRING_STMT  */
  YYSYMBOL_SUBTRACT = 81,                  /* SUBTRACT  */
  YYSYMBOL_UNSTRING = 82,                  /* UNSTRING  */
  YYSYMBOL_WRITE = 83,                     /* WRITE  */
  YYSYMBOL_UNTIL = 84,                     /* UNTIL  */
  YYSYMBOL_VARYING = 85,                   /* VARYING  */
  YYSYMBOL_WHEN = 86,                      /* WHEN  */
  YYSYMBOL_OTHER = 87,                     /* OTHER  */
  YYSYMBOL_THROUGH = 88,                   /* THROUGH  */
  YYSYMBOL_THRU = 89,                      /* THRU  */
  YYSYMBOL_BY = 90,                        /* BY  */
  YYSYMBOL_GIVING = 91,                    /* GIVING  */
  YYSYMBOL_REMAINDER = 92,                 /* REMAINDER  */
  YYSYMBOL_INTO = 93,                      /* INTO  */
  YYSYMBOL_FROM = 94,                      /* FROM  */
  YYSYMBOL_OPEN = 95,                      /* OPEN  */
  YYSYMBOL_CLOSE = 96,                     /* CLOSE  */
  YYSYMBOL_INPUT = 97,                     /* INPUT  */
  YYSYMBOL_OUTPUT = 98,                    /* OUTPUT  */
  YYSYMBOL_IO = 99,                        /* IO  */
  YYSYMBOL_EXTEND = 100,                   /* EXTEND  */
  YYSYMBOL_FILE = 101,                     /* FILE  */
  YYSYMBOL_FD = 102,                       /* FD  */
  YYSYMBOL_SD = 103,                       /* SD  */
  YYSYMBOL_SELECT = 104,                   /* SELECT  */
  YYSYMBOL_ASSIGN = 105,                   /* ASSIGN  */
  YYSYMBOL_ORGANIZATION = 106,             /* ORGANIZATION  */
  YYSYMBOL_ACCESS_MODE = 107,              /* ACCESS_MODE  */
  YYSYMBOL_SEQUENTIAL = 108,               /* SEQUENTIAL  */
  YYSYMBOL_INDEXED = 109,                  /* INDEXED  */
  YYSYMBOL_RELATIVE = 110,                 /* RELATIVE  */
  YYSYMBOL_RANDOM = 111,                   /* RANDOM  */
  YYSYMBOL_DYNAMIC = 112,                  /* DYNAMIC  */
  YYSYMBOL_RECORD_KEY = 113,               /* RECORD_KEY  */
  YYSYMBOL_AND = 114,                      /* AND  */
  YYSYMBOL_OR = 115,                       /* OR  */
  YYSYMBOL_NOT = 116,                      /* NOT  */
  YYSYMBOL_EQUAL = 117,                    /* EQUAL  */
  YYSYMBOL_GREATER = 118,                  /* GREATER  */
  YYSYMBOL_LESS = 119,                     /* LESS  */
  YYSYMBOL_GREATER_EQUAL = 120,            /* GREATER_EQUAL  */
  YYSYMBOL_LESS_EQUAL = 121,               /* LESS_EQUAL  */
  YYSYMBOL_NOT_EQUAL = 122,                /* NOT_EQUAL  */
  YYSYMBOL_PLUS = 123,                     /* PLUS  */
  YYSYMBOL_MINUS = 124,                    /* MINUS  */
  YYSYMBOL_POWER = 125,                    /* POWER  */
  YYSYMBOL_PROGRAM_ID = 126,               /* PROGRAM_ID  */
  YYSYMBOL_AUTHOR = 127,                   /* AUTHOR  */
  YYSYMBOL_DATE_WRITTEN = 128,             /* DATE_WRITTEN  */
  YYSYMBOL_IS = 129,                       /* IS  */
  YYSYMBOL_ARE = 130,                      /* ARE  */
  YYSYMBOL_TO = 131,                       /* TO  */
  YYSYMBOL_OF = 132,                       /* OF  */
  YYSYMBOL_IN = 133,                       /* IN  */
  YYSYMBOL_WITH = 134,                     /* WITH  */
  YYSYMBOL_ALL = 135,                      /* ALL  */
  YYSYMBOL_USING = 136,                    /* USING  */
  YYSYMBOL_TRUE = 137,                     /* TRUE  */
  YYSYMBOL_FALSE = 138,                    /* FALSE  */
  YYSYMBOL_ZERO = 139,                     /* ZERO  */
  YYSYMBOL_SPACE = 140,                    /* SPACE  */
  YYSYMBOL_NULL_TOK = 141,                 /* NULL_TOK  */
  YYSYMBOL_INITIAL = 142,                  /* INITIAL  */
  YYSYMBOL_CONTROL = 143,                  /* CONTROL  */
  YYSYMBOL_FILLER = 144,                   /* FILLER  */
  YYSYMBOL_END = 145,                      /* END  */
  YYSYMBOL_PROGRAM = 146,                  /* PROGRAM  */
  YYSYMBOL_RUN = 147,                      /* RUN  */
  YYSYMBOL_DOT = 148,                      /* DOT  */
  YYSYMBOL_COMMA = 149,                    /* COMMA  */
  YYSYMBOL_SEMICOLON = 150,                /* SEMICOLON  */
  YYSYMBOL_COLON = 151,                    /* COLON  */
  YYSYMBOL_LPAREN = 152,                   /* LPAREN  */
  YYSYMBOL_RPAREN = 153,                   /* RPAREN  */
  YYSYMBOL_DIVIDE = 154,                   /* DIVIDE  */
  YYSYMBOL_UMINUS = 155,                   /* UMINUS  */
  YYSYMBOL_YYACCEPT = 156,                 /* $accept  */
  YYSYMBOL_program = 157,                  /* program  */
  YYSYMBOL_identification_division = 158,  /* identification_division  */
  YYSYMBOL_159_1 = 159,                    /* $@1  */
  YYSYMBOL_program_id_para = 160,          /* program_id_para  */
  YYSYMBOL_identification_paras_opt = 161, /* identification_paras_opt  */
  YYSYMBOL_identification_paras = 162,     /* identification_paras  */
  YYSYMBOL_identification_para = 163,      /* identification_para  */
  YYSYMBOL_anything_until_dot = 164,       /* anything_until_dot  */
  YYSYMBOL_class_options_opt = 165,        /* class_options_opt  */
  YYSYMBOL_class_options = 166,            /* class_options  */
  YYSYMBOL_class_option = 167,             /* class_option  */
  YYSYMBOL_environment_division_opt = 168, /* environment_division_opt  */
  YYSYMBOL_environment_sections_opt = 169, /* environment_sections_opt  */
  YYSYMBOL_environment_sections = 170,     /* environment_sections  */
  YYSYMBOL_environment_section = 171,      /* environment_section  */
  YYSYMBOL_configuration_paras_opt = 172,  /* configuration_paras_opt  */
  YYSYMBOL_io_paras_opt = 173,             /* io_paras_opt  */
  YYSYMBOL_file_control_para = 174,        /* file_control_para  */
  YYSYMBOL_select_clauses_opt = 175,       /* select_clauses_opt  */
  YYSYMBOL_select_clauses = 176,           /* select_clauses  */
  YYSYMBOL_select_clause = 177,            /* select_clause  */
  YYSYMBOL_data_division_opt = 178,        /* data_division_opt  */
  YYSYMBOL_data_sections_opt = 179,        /* data_sections_opt  */
  YYSYMBOL_data_sections = 180,            /* data_sections  */
  YYSYMBOL_data_section = 181,             /* data_section  */
  YYSYMBOL_file_descriptions_opt = 182,    /* file_descriptions_opt  */
  YYSYMBOL_file_descriptions = 183,        /* file_descriptions  */
  YYSYMBOL_file_description = 184,         /* file_description  */
  YYSYMBOL_data_items_opt = 185,           /* data_items_opt  */
  YYSYMBOL_data_items = 186,               /* data_items  */
  YYSYMBOL_data_item = 187,                /* data_item  */
  YYSYMBOL_picture_clause_opt = 188,       /* picture_clause_opt  */
  YYSYMBOL_picture_clause = 189,           /* picture_clause  */
  YYSYMBOL_picture_string = 190,           /* picture_string  */
  YYSYMBOL_usage_type = 191,               /* usage_type  */
  YYSYMBOL_literal = 192,                  /* literal  */
  YYSYMBOL_procedure_division_opt = 193,   /* procedure_division_opt  */
  YYSYMBOL_parameter_list = 194,           /* parameter_list  */
  YYSYMBOL_method_body = 195,              /* method_body  */
  YYSYMBOL_method_options_opt = 196,       /* method_options_opt  */
  YYSYMBOL_method_options = 197,           /* method_options  */
  YYSYMBOL_method_option = 198,            /* method_option  */
  YYSYMBOL_method_using_opt = 199,         /* method_using_opt  */
  YYSYMBOL_statements_opt = 200,           /* statements_opt  */
  YYSYMBOL_statement_list = 201,           /* statement_list  */
  YYSYMBOL_statement = 202,                /* statement  */
  YYSYMBOL_move_stmt = 203,                /* move_stmt  */
  YYSYMBOL_add_stmt = 204,                 /* add_stmt  */
  YYSYMBOL_compute_stmt = 205,             /* compute_stmt  */
  YYSYMBOL_if_stmt = 206,                  /* if_stmt  */
  YYSYMBOL_statement_list_opt = 207,       /* statement_list_opt  */
  YYSYMBOL_end_if_opt = 208,               /* end_if_opt  */
  YYSYMBOL_perform_stmt = 209,             /* perform_stmt  */
  YYSYMBOL_display_stmt = 210,             /* display_stmt  */
  YYSYMBOL_accept_stmt = 211,              /* accept_stmt  */
  YYSYMBOL_call_stmt = 212,                /* call_stmt  */
  YYSYMBOL_invoke_stmt = 213,              /* invoke_stmt  */
  YYSYMBOL_argument_list_opt = 214,        /* argument_list_opt  */
  YYSYMBOL_argument_list = 215,            /* argument_list  */
  YYSYMBOL_exit_stmt = 216,                /* exit_stmt  */
  YYSYMBOL_stop_stmt = 217,                /* stop_stmt  */
  YYSYMBOL_condition = 218,                /* condition  */
  YYSYMBOL_expression = 219,               /* expression  */
  YYSYMBOL_literal_expr = 220,             /* literal_expr  */
  YYSYMBOL_arithmetic_expr = 221,          /* arithmetic_expr  */
  YYSYMBOL_identifier = 222,               /* identifier  */
  YYSYMBOL_end_program_opt = 223           /* end_program_opt  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  7
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   364

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  156
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  68
/* YYNRULES -- Number of rules.  */
#define YYNRULES  171
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  318

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   410


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    95,    95,   104,   108,   107,   115,   116,   119,   121,
     125,   126,   130,   131,   134,   138,   140,   144,   145,   149,
     154,   159,   167,   169,   172,   174,   178,   179,   183,   184,
     187,   191,   193,   197,   200,   202,   206,   207,   211,   215,
     217,   220,   222,   226,   227,   231,   232,   233,   234,   237,
     239,   243,   244,   248,   249,   252,   254,   258,   260,   265,
     271,   277,   283,   293,   294,   299,   301,   303,   305,   310,
     312,   317,   317,   317,   317,   317,   317,   317,   318,   318,
     318,   318,   322,   323,   324,   325,   326,   327,   331,   333,
     334,   335,   342,   343,   347,   353,   355,   359,   360,   364,
     369,   376,   378,   383,   384,   389,   391,   396,   397,   398,
     399,   400,   401,   402,   403,   404,   405,   406,   410,   418,
     424,   433,   440,   444,   452,   453,   458,   459,   463,   467,
     471,   478,   485,   492,   499,   503,   509,   511,   515,   516,
     520,   524,   531,   538,   540,   542,   544,   546,   548,   550,
     552,   554,   556,   561,   563,   567,   569,   571,   573,   575,
     580,   582,   584,   586,   588,   590,   592,   594,   599,   603,
     605,   606
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "IDENTIFIER",
  "STRING_LITERAL", "INTEGER", "LEVEL_NUMBER", "FLOAT",
  "IDENTIFICATION_DIVISION", "ENVIRONMENT_DIVISION", "DATA_DIVISION",
  "PROCEDURE_DIVISION", "CONFIGURATION_SECTION", "INPUT_OUTPUT_SECTION",
  "FILE_SECTION", "WORKING_STORAGE_SECTION", "LOCAL_STORAGE_SECTION",
  "LINKAGE_SECTION", "CLASS", "CLASS_ID", "METHOD", "METHOD_ID", "OBJECT",
  "INHERITS", "INTERFACE", "INTERFACE_ID", "FACTORY", "INVOKE", "NEW",
  "SELF", "SUPER", "PROPERTY", "STATIC", "INSTANCE", "FINAL", "ABSTRACT",
  "OVERRIDE", "PIC", "VALUE", "REDEFINES", "OCCURS", "TIMES",
  "DEPENDING_ON", "INDEXED_BY", "USAGE", "COMP", "COMP_1", "COMP_2",
  "COMP_3", "COMP_4", "COMP_5", "BINARY", "PACKED_DECIMAL",
  "DISPLAY_USAGE", "POINTER", "OBJECT_REFERENCE", "ACCEPT", "ADD", "CALL",
  "COMPUTE", "DELETE", "DISPLAY", "DIVIDE_OP", "EVALUATE", "EXIT", "GOTO",
  "IF", "ELSE", "END_IF", "MOVE", "MULTIPLY", "PERFORM", "END_PERFORM",
  "READ", "RETURN", "RETURNING", "REWRITE", "SEARCH", "SET", "STOP",
  "STRING_STMT", "SUBTRACT", "UNSTRING", "WRITE", "UNTIL", "VARYING",
  "WHEN", "OTHER", "THROUGH", "THRU", "BY", "GIVING", "REMAINDER", "INTO",
  "FROM", "OPEN", "CLOSE", "INPUT", "OUTPUT", "IO", "EXTEND", "FILE", "FD",
  "SD", "SELECT", "ASSIGN", "ORGANIZATION", "ACCESS_MODE", "SEQUENTIAL",
  "INDEXED", "RELATIVE", "RANDOM", "DYNAMIC", "RECORD_KEY", "AND", "OR",
  "NOT", "EQUAL", "GREATER", "LESS", "GREATER_EQUAL", "LESS_EQUAL",
  "NOT_EQUAL", "PLUS", "MINUS", "POWER", "PROGRAM_ID", "AUTHOR",
  "DATE_WRITTEN", "IS", "ARE", "TO", "OF", "IN", "WITH", "ALL", "USING",
  "TRUE", "FALSE", "ZERO", "SPACE", "NULL_TOK", "INITIAL", "CONTROL",
  "FILLER", "END", "PROGRAM", "RUN", "DOT", "COMMA", "SEMICOLON", "COLON",
  "LPAREN", "RPAREN", "DIVIDE", "UMINUS", "$accept", "program",
  "identification_division", "$@1", "program_id_para",
  "identification_paras_opt", "identification_paras",
  "identification_para", "anything_until_dot", "class_options_opt",
  "class_options", "class_option", "environment_division_opt",
  "environment_sections_opt", "environment_sections",
  "environment_section", "configuration_paras_opt", "io_paras_opt",
  "file_control_para", "select_clauses_opt", "select_clauses",
  "select_clause", "data_division_opt", "data_sections_opt",
  "data_sections", "data_section", "file_descriptions_opt",
  "file_descriptions", "file_description", "data_items_opt", "data_items",
  "data_item", "picture_clause_opt", "picture_clause", "picture_string",
  "usage_type", "literal", "procedure_division_opt", "parameter_list",
  "method_body", "method_options_opt", "method_options", "method_option",
  "method_using_opt", "statements_opt", "statement_list", "statement",
  "move_stmt", "add_stmt", "compute_stmt", "if_stmt", "statement_list_opt",
  "end_if_opt", "perform_stmt", "display_stmt", "accept_stmt", "call_stmt",
  "invoke_stmt", "argument_list_opt", "argument_list", "exit_stmt",
  "stop_stmt", "condition", "expression", "literal_expr",
  "arithmetic_expr", "identifier", "end_program_opt", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-142)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      57,  -101,   -65,    87,    92,   -30,   105,  -142,   -24,   108,
     -17,   -67,  -142,   -12,    80,    -7,    41,   105,    -5,    14,
    -142,   -67,  -142,  -142,    28,    31,  -142,    80,  -142,   151,
     -90,    33,    35,   -75,  -142,  -142,  -142,    22,  -142,    52,
    -142,    38,    39,    63,    64,  -142,   151,  -142,   105,   274,
     105,     1,  -142,    43,  -142,  -142,  -142,    89,    65,    68,
    -142,    22,  -142,  -142,    81,  -142,  -142,     3,   211,   211,
     211,  -142,     4,  -142,   105,   105,     5,   105,   105,     5,
       8,    -1,     5,   241,    71,  -142,   274,  -142,  -142,  -142,
    -142,  -142,  -142,  -142,  -142,  -142,  -142,  -142,  -142,    72,
     105,   105,    73,   105,  -142,  -142,  -142,    79,   105,   105,
    -142,     3,  -142,    10,  -142,   211,  -142,  -142,  -142,   274,
    -142,   219,    82,  -142,  -142,  -142,     5,  -142,  -142,     5,
       2,  -142,  -142,  -142,    91,   112,   -14,    85,  -142,    -1,
      -1,   179,   153,   101,   257,   -59,    86,  -142,    21,    93,
      94,  -142,    98,    99,   103,   104,  -142,    37,   170,  -142,
    -142,   113,  -142,   191,   -25,     5,     5,     5,     5,     5,
     105,     5,     5,  -142,  -142,  -142,   -83,    78,    -1,    -1,
     274,   -18,     5,     5,     5,     5,     5,     5,   105,   106,
      -1,  -142,  -142,   107,   109,  -142,   248,    21,  -142,  -142,
    -142,  -142,   156,   211,   211,    32,   132,   133,   115,  -142,
     105,   259,   118,     5,  -142,   -41,    -3,   -21,   -21,   -41,
     -62,   119,     5,    -3,   -11,  -142,  -142,   155,   274,   131,
    -142,  -142,    -3,    -3,    -3,    -3,    -3,    -3,   134,  -142,
     -81,  -142,  -142,   144,  -142,   105,  -142,   156,  -142,  -142,
    -142,  -142,  -142,   166,  -142,    19,   309,  -142,    37,   240,
    -142,   -57,   105,  -142,  -142,    -3,  -142,   215,  -142,  -142,
    -142,   105,   137,   181,  -142,  -142,  -142,  -142,  -142,  -142,
    -142,  -142,  -142,  -142,  -142,  -142,  -142,  -142,  -142,  -142,
    -142,  -142,  -142,  -142,  -142,   139,    37,   105,  -142,   140,
     141,   105,   274,   159,  -142,   148,   158,  -142,  -142,   147,
     105,  -142,  -142,   283,   160,   161,  -142,  -142
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,    22,     0,     0,     1,     0,    39,
       0,     8,   168,     0,    24,     0,    88,     0,     0,     0,
       3,     9,    10,     4,     0,     0,    23,    25,    26,    41,
       0,     0,   169,     0,    14,    14,    11,    15,    30,    31,
      27,     0,     0,     0,     0,    40,    42,    43,     0,   103,
       0,     0,     2,     0,     6,    12,    13,     0,     0,     0,
       5,    16,    17,    28,     0,    29,    32,    49,    55,    55,
      55,    44,     0,    92,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    89,   104,   105,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,     0,
       0,     0,     0,     0,    20,    21,    18,     0,     0,     0,
      45,    50,    51,     0,    46,    56,    57,    47,    48,   103,
      93,     0,     0,   157,   155,   156,     0,   158,   159,     0,
       0,   154,   153,   160,     0,     0,     0,     0,   140,     0,
       0,   124,     0,     0,     0,     0,     0,   106,    95,     0,
       0,     7,     0,     0,     0,     0,    52,    63,    63,    58,
      90,     0,   132,   166,     0,     0,     0,     0,     0,     0,
       0,   136,     0,   131,   141,   151,     0,     0,     0,     0,
     125,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   128,   142,     0,     0,    91,     0,    96,    97,   171,
     170,    19,    34,    55,    55,     0,     0,     0,     0,    64,
       0,     0,     0,   136,   167,   163,   164,   161,   162,   165,
       0,     0,   137,   138,     0,   152,   149,   150,   124,     0,
     126,   122,   143,   145,   146,   147,   148,   144,     0,   130,
       0,    99,   100,   101,    98,     0,    33,    35,    36,    53,
      54,    69,    70,     0,    66,     0,     0,    62,    63,     0,
      59,     0,     0,   119,   133,   139,   121,     0,   127,   118,
     129,     0,     0,     0,    37,    65,    84,    82,    83,    85,
      86,    87,    67,    72,    73,    74,    75,    76,    77,    71,
      78,    79,    80,    81,    68,     0,    63,     0,   135,     0,
       0,   102,   103,     0,    60,     0,     0,   120,   123,     0,
       0,    61,   134,     0,     0,     0,    38,    94
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -142,  -142,  -142,  -142,  -142,  -142,  -142,   290,   282,  -142,
    -142,   258,  -142,  -142,  -142,   295,  -142,  -142,  -142,  -142,
    -142,    77,  -142,  -142,  -142,   279,  -142,  -142,   216,   -55,
    -142,   222,  -141,  -142,    88,  -142,  -142,  -142,    75,  -142,
    -142,  -142,   142,  -142,  -118,   -78,   -64,  -142,  -142,  -142,
    -142,   114,  -142,  -142,  -142,  -142,  -142,  -142,   121,  -142,
    -142,  -142,  -100,     6,  -142,  -142,    -6,  -142
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     3,     4,    37,    11,    20,    21,    22,    55,    60,
      61,    62,     9,    26,    27,    28,    63,    65,    66,   246,
     247,   248,    16,    45,    46,    47,   110,   111,   112,   114,
     115,   116,   208,   209,   254,   294,   282,    32,    72,   195,
     196,   197,   198,   272,    85,    86,    87,    88,    89,    90,
      91,   181,   231,    92,    93,    94,    95,    96,   221,   222,
      97,    98,   141,   142,   131,   132,   133,    52
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      13,   160,    12,   123,   124,   144,   125,    12,    12,   123,
     124,    33,   125,    12,   117,   118,   165,   212,   297,   100,
     165,   166,   147,   276,   277,   190,   278,   165,   137,   262,
     165,   178,   179,   178,   179,   251,   252,   166,   165,   175,
     176,   166,    73,   165,    99,    57,    48,     5,   166,   228,
     229,   166,    30,   193,    53,   194,    58,    59,    49,   166,
      18,    19,    31,   180,   166,     1,   120,   270,   121,   122,
     225,   134,   135,    54,   205,   206,     2,   145,   226,   227,
     147,   207,   130,     6,   169,   136,   263,     7,   143,   191,
     240,   298,    24,    25,   149,   150,    10,   152,   167,   168,
     169,     8,   154,   155,   169,   108,   109,   158,    12,   167,
     168,   169,   167,   168,   169,   139,   147,   295,    15,   165,
     167,   168,   169,   126,    14,   167,   168,   169,   214,   126,
     230,    17,   163,   170,   173,   164,    23,   266,   127,   128,
     166,    29,   165,    34,   127,   128,   177,   101,   249,   250,
     180,   140,   119,    64,   157,   305,   138,   129,   279,   280,
     281,   253,    35,   166,   220,    41,    42,    43,    44,   251,
     252,   215,   216,   217,   218,   219,    38,   223,   224,    39,
      51,    50,   238,   103,   309,   102,    67,    68,   232,   233,
     234,   235,   236,   237,   165,   182,   183,   184,   185,   186,
     187,   167,   168,   169,   258,   107,    74,   205,   206,   210,
     211,    69,    70,   104,   207,   166,   105,   113,   146,   223,
     148,   151,   153,   161,   167,   168,   169,   171,   265,   172,
     162,   214,   188,   174,   192,    75,    76,    77,    78,   273,
      79,   199,   200,    80,    12,    81,   201,   202,    82,   213,
      83,   203,   204,   166,   239,   241,   299,   242,    84,   243,
     245,   255,   256,   257,   259,    73,   260,   264,    74,   178,
     182,   183,   184,   185,   186,   187,   167,   168,   169,   268,
     271,   296,   269,   300,    74,   302,   303,   304,   307,   308,
     310,   306,   313,   178,   179,   120,   311,    75,    76,    77,
      78,    74,    79,   315,   314,    80,   312,    81,   316,   317,
      82,    36,    83,    75,    76,    77,    78,    56,    79,   106,
      84,    80,    40,    81,   274,    71,    82,   156,    83,   189,
      75,    76,    77,    78,   261,    79,    84,   159,    80,   244,
      81,   275,   267,    82,     0,    83,   301,     0,     0,     0,
       0,     0,     0,    84,   283,   284,   285,   286,   287,   288,
     289,   290,   291,   292,   293
};

static const yytype_int16 yycheck[] =
{
       6,   119,     3,     4,     5,    83,     7,     3,     3,     4,
       5,    17,     7,     3,    69,    70,    41,   158,    75,    18,
      41,    62,    86,     4,     5,    84,     7,    41,    20,    91,
      41,   114,   115,   114,   115,     3,     4,    62,    41,   139,
     140,    62,    48,    41,    50,    23,   136,   148,    62,    67,
      68,    62,    11,    32,   129,    34,    34,    35,   148,    62,
     127,   128,    21,   141,    62,     8,    72,   148,    74,    75,
     153,    77,    78,   148,    37,    38,    19,    83,   178,   179,
     144,    44,    76,   148,   125,    79,   148,     0,    82,   148,
     190,   148,    12,    13,   100,   101,   126,   103,   123,   124,
     125,     9,   108,   109,   125,   102,   103,   113,     3,   123,
     124,   125,   123,   124,   125,   116,   180,   258,    10,    41,
     123,   124,   125,   124,   148,   123,   124,   125,   153,   124,
     148,   148,   126,   131,   148,   129,   148,   148,   139,   140,
      62,   148,    41,   148,   139,   140,   140,   146,   203,   204,
     228,   152,   148,   101,   144,   296,   148,   152,   139,   140,
     141,   129,   148,    62,   170,    14,    15,    16,    17,     3,
       4,   165,   166,   167,   168,   169,   148,   171,   172,   148,
     145,   148,   188,    94,   302,   142,   148,   148,   182,   183,
     184,   185,   186,   187,    41,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   210,   124,    27,    37,    38,    39,
      40,   148,   148,   148,    44,    62,   148,     6,   147,   213,
     148,   148,   143,     4,   123,   124,   125,   136,   222,   117,
     148,   153,   131,   148,   148,    56,    57,    58,    59,   245,
      61,   148,   148,    64,     3,    66,   148,   148,    69,   136,
      71,   148,   148,    62,   148,   148,   262,   148,    79,    11,
     104,   129,   129,   148,     5,   271,   148,   148,    27,   114,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   148,
     136,    41,   148,    68,    27,   148,   105,   148,   148,   148,
     131,   297,   145,   114,   115,   301,   148,    56,    57,    58,
      59,    27,    61,    20,   310,    64,   148,    66,   148,   148,
      69,    21,    71,    56,    57,    58,    59,    35,    61,    61,
      79,    64,    27,    66,   247,    46,    69,   111,    71,    72,
      56,    57,    58,    59,   213,    61,    79,   115,    64,   197,
      66,   253,   228,    69,    -1,    71,   271,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     8,    19,   157,   158,   148,   148,     0,     9,   168,
     126,   160,     3,   222,   148,    10,   178,   148,   127,   128,
     161,   162,   163,   148,    12,    13,   169,   170,   171,   148,
      11,    21,   193,   222,   148,   148,   163,   159,   148,   148,
     171,    14,    15,    16,    17,   179,   180,   181,   136,   148,
     148,   145,   223,   129,   148,   164,   164,    23,    34,    35,
     165,   166,   167,   172,   101,   173,   174,   148,   148,   148,
     148,   181,   194,   222,    27,    56,    57,    58,    59,    61,
      64,    66,    69,    71,    79,   200,   201,   202,   203,   204,
     205,   206,   209,   210,   211,   212,   213,   216,   217,   222,
      18,   146,   142,    94,   148,   148,   167,   124,   102,   103,
     182,   183,   184,     6,   185,   186,   187,   185,   185,   148,
     222,   222,   222,     4,     5,     7,   124,   139,   140,   152,
     219,   220,   221,   222,   222,   222,   219,    20,   148,   116,
     152,   218,   219,   219,   201,   222,   147,   202,   148,   222,
     222,   148,   222,   143,   222,   222,   184,   144,   222,   187,
     200,     4,   148,   219,   219,    41,    62,   123,   124,   125,
     131,   136,   117,   148,   148,   218,   218,   219,   114,   115,
     201,   207,   117,   118,   119,   120,   121,   122,   131,    72,
      84,   148,   148,    32,    34,   195,   196,   197,   198,   148,
     148,   148,   148,   148,   148,    37,    38,    44,   188,   189,
      39,    40,   188,   136,   153,   219,   219,   219,   219,   219,
     222,   214,   215,   219,   219,   153,   218,   218,    67,    68,
     148,   208,   219,   219,   219,   219,   219,   219,   222,   148,
     218,   148,   148,    11,   198,   104,   175,   176,   177,   185,
     185,     3,     4,   129,   190,   129,   129,   148,   222,     5,
     148,   214,    91,   148,   148,   219,   148,   207,   148,   148,
     148,   136,   199,   222,   177,   190,     4,     5,     7,   139,
     140,   141,   192,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,   191,   188,    41,    75,   148,   222,
      68,   194,   148,   105,   148,   188,   222,   148,   148,   200,
     131,   148,   148,   145,   222,    20,   148,   148
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,   156,   157,   158,   159,   158,   160,   160,   161,   161,
     162,   162,   163,   163,   164,   165,   165,   166,   166,   167,
     167,   167,   168,   168,   169,   169,   170,   170,   171,   171,
     172,   173,   173,   174,   175,   175,   176,   176,   177,   178,
     178,   179,   179,   180,   180,   181,   181,   181,   181,   182,
     182,   183,   183,   184,   184,   185,   185,   186,   186,   187,
     187,   187,   187,   188,   188,   189,   189,   189,   189,   190,
     190,   191,   191,   191,   191,   191,   191,   191,   191,   191,
     191,   191,   192,   192,   192,   192,   192,   192,   193,   193,
     193,   193,   194,   194,   195,   196,   196,   197,   197,   198,
     198,   199,   199,   200,   200,   201,   201,   202,   202,   202,
     202,   202,   202,   202,   202,   202,   202,   202,   203,   204,
     204,   205,   206,   206,   207,   207,   208,   208,   209,   209,
     209,   210,   211,   212,   213,   213,   214,   214,   215,   215,
     216,   216,   217,   218,   218,   218,   218,   218,   218,   218,
     218,   218,   218,   219,   219,   220,   220,   220,   220,   220,
     221,   221,   221,   221,   221,   221,   221,   221,   222,   223,
     223,   223
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     5,     4,     0,     6,     4,     6,     0,     1,
       1,     2,     3,     3,     0,     0,     1,     1,     2,     4,
       2,     2,     0,     3,     0,     1,     1,     2,     3,     3,
       0,     0,     1,     5,     0,     1,     1,     2,     6,     0,
       3,     0,     1,     1,     2,     3,     3,     3,     3,     0,
       1,     1,     2,     4,     4,     0,     1,     1,     2,     4,
       6,     7,     4,     0,     1,     3,     2,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     3,
       5,     5,     1,     2,     8,     0,     1,     1,     2,     2,
       2,     0,     2,     0,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     5,     5,
       7,     5,     4,     7,     0,     1,     1,     2,     3,     5,
       4,     3,     3,     5,     8,     6,     0,     1,     1,     2,
       2,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     2,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     3,     3,     3,     3,     2,     3,     1,     0,
       4,     4
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 4: /* $@1: %empty  */
#line 108 "cbolgrammar.y"
          {
		current_class = define_class((yyvsp[-1].string), NULL);
	  }
#line 1575 "gram.c"
    break;

  case 19: /* class_option: INHERITS FROM identifier DOT  */
#line 150 "cbolgrammar.y"
          {
		if (current_class)
			current_class->inherits = (yyvsp[-1].string);
	  }
#line 1584 "gram.c"
    break;

  case 20: /* class_option: FINAL DOT  */
#line 155 "cbolgrammar.y"
          {
		if (current_class)
			current_class->is_final = 1;
	  }
#line 1593 "gram.c"
    break;

  case 21: /* class_option: ABSTRACT DOT  */
#line 160 "cbolgrammar.y"
          {
		if (current_class)
			current_class->is_abstract = 1;
	  }
#line 1602 "gram.c"
    break;

  case 57: /* data_items: data_item  */
#line 259 "cbolgrammar.y"
          { (yyval.symbol) = (yyvsp[0].symbol); }
#line 1608 "gram.c"
    break;

  case 58: /* data_items: data_items data_item  */
#line 261 "cbolgrammar.y"
          { (yyval.symbol) = (yyvsp[0].symbol); }
#line 1614 "gram.c"
    break;

  case 59: /* data_item: LEVEL_NUMBER identifier picture_clause_opt DOT  */
#line 266 "cbolgrammar.y"
          {
		struct cobsym *sym = install((yyvsp[-2].string), (yyvsp[-3].intval));
		sym->pic = (yyvsp[-1].pic);
		(yyval.symbol) = sym;
	  }
#line 1624 "gram.c"
    break;

  case 60: /* data_item: LEVEL_NUMBER identifier REDEFINES identifier picture_clause_opt DOT  */
#line 272 "cbolgrammar.y"
          {
		struct cobsym *sym = install((yyvsp[-4].string), (yyvsp[-5].intval));
		sym->pic = (yyvsp[-1].pic);
		(yyval.symbol) = sym;
	  }
#line 1634 "gram.c"
    break;

  case 61: /* data_item: LEVEL_NUMBER identifier OCCURS INTEGER TIMES picture_clause_opt DOT  */
#line 278 "cbolgrammar.y"
          {
		struct cobsym *sym = install((yyvsp[-5].string), (yyvsp[-6].intval));
		sym->pic = (yyvsp[-1].pic);
		(yyval.symbol) = sym;
	  }
#line 1644 "gram.c"
    break;

  case 62: /* data_item: LEVEL_NUMBER FILLER picture_clause_opt DOT  */
#line 284 "cbolgrammar.y"
          {
		struct cobsym *sym = install("FILLER", (yyvsp[-3].intval));
		sym->pic = (yyvsp[-1].pic);
		(yyval.symbol) = sym;
	  }
#line 1654 "gram.c"
    break;

  case 63: /* picture_clause_opt: %empty  */
#line 293 "cbolgrammar.y"
          { (yyval.pic) = NULL; }
#line 1660 "gram.c"
    break;

  case 64: /* picture_clause_opt: picture_clause  */
#line 295 "cbolgrammar.y"
          { (yyval.pic) = (yyvsp[0].pic); }
#line 1666 "gram.c"
    break;

  case 65: /* picture_clause: PIC IS picture_string  */
#line 300 "cbolgrammar.y"
          { (yyval.pic) = parse_picture((yyvsp[0].string)); }
#line 1672 "gram.c"
    break;

  case 66: /* picture_clause: PIC picture_string  */
#line 302 "cbolgrammar.y"
          { (yyval.pic) = parse_picture((yyvsp[0].string)); }
#line 1678 "gram.c"
    break;

  case 67: /* picture_clause: VALUE IS literal  */
#line 304 "cbolgrammar.y"
          { (yyval.pic) = NULL; /* Handle VALUE separately */ }
#line 1684 "gram.c"
    break;

  case 68: /* picture_clause: USAGE IS usage_type  */
#line 306 "cbolgrammar.y"
          { (yyval.pic) = NULL; /* Handle USAGE separately */ }
#line 1690 "gram.c"
    break;

  case 69: /* picture_string: IDENTIFIER  */
#line 311 "cbolgrammar.y"
          { (yyval.string) = (yyvsp[0].string); }
#line 1696 "gram.c"
    break;

  case 70: /* picture_string: STRING_LITERAL  */
#line 313 "cbolgrammar.y"
          { (yyval.string) = (yyvsp[0].string); }
#line 1702 "gram.c"
    break;

  case 91: /* procedure_division_opt: METHOD_ID DOT identifier DOT method_body  */
#line 336 "cbolgrammar.y"
          {
		current_method = define_method((yyvsp[-2].string), 0);
	  }
#line 1710 "gram.c"
    break;

  case 99: /* method_option: STATIC DOT  */
#line 365 "cbolgrammar.y"
          {
		if (current_method)
			current_method->is_static = 1;
	  }
#line 1719 "gram.c"
    break;

  case 100: /* method_option: FINAL DOT  */
#line 370 "cbolgrammar.y"
          {
		if (current_method)
			current_method->is_final = 1;
	  }
#line 1728 "gram.c"
    break;

  case 103: /* statements_opt: %empty  */
#line 383 "cbolgrammar.y"
          { (yyval.node) = NULL; }
#line 1734 "gram.c"
    break;

  case 104: /* statements_opt: statement_list  */
#line 385 "cbolgrammar.y"
          { (yyval.node) = (yyvsp[0].node); }
#line 1740 "gram.c"
    break;

  case 105: /* statement_list: statement  */
#line 390 "cbolgrammar.y"
          { (yyval.node) = (yyvsp[0].node); }
#line 1746 "gram.c"
    break;

  case 106: /* statement_list: statement_list statement  */
#line 392 "cbolgrammar.y"
          { (yyval.node) = buildtree(COMOP, (yyvsp[-1].node), (yyvsp[0].node)); }
#line 1752 "gram.c"
    break;

  case 118: /* move_stmt: MOVE expression TO identifier DOT  */
#line 411 "cbolgrammar.y"
          {
		NODE *dst = make_name((yyvsp[-1].string));
		(yyval.node) = gen_move((yyvsp[-3].node), dst);
	  }
#line 1761 "gram.c"
    break;

  case 119: /* add_stmt: ADD expression TO identifier DOT  */
#line 419 "cbolgrammar.y"
          {
		NODE *var = make_name((yyvsp[-1].string));
		NODE *sum = buildtree(PLUS, var, (yyvsp[-3].node));
		(yyval.node) = gen_move(sum, var);
	  }
#line 1771 "gram.c"
    break;

  case 120: /* add_stmt: ADD expression TO identifier GIVING identifier DOT  */
#line 425 "cbolgrammar.y"
          {
		NODE *sum = buildtree(PLUS, make_name((yyvsp[-3].string)), (yyvsp[-5].node));
		NODE *dst = make_name((yyvsp[-1].string));
		(yyval.node) = gen_move(sum, dst);
	  }
#line 1781 "gram.c"
    break;

  case 121: /* compute_stmt: COMPUTE identifier EQUAL expression DOT  */
#line 434 "cbolgrammar.y"
          {
		(yyval.node) = gen_builtin_compute(make_name((yyvsp[-3].string)), (yyvsp[-1].node));
	  }
#line 1789 "gram.c"
    break;

  case 122: /* if_stmt: IF condition statement_list_opt end_if_opt  */
#line 441 "cbolgrammar.y"
          {
		(yyval.node) = gen_if((yyvsp[-2].node), (yyvsp[-1].node), NULL);
	  }
#line 1797 "gram.c"
    break;

  case 123: /* if_stmt: IF condition statement_list_opt ELSE statement_list_opt END_IF DOT  */
#line 445 "cbolgrammar.y"
          {
		(yyval.node) = gen_if((yyvsp[-5].node), (yyvsp[-4].node), (yyvsp[-2].node));
	  }
#line 1805 "gram.c"
    break;

  case 124: /* statement_list_opt: %empty  */
#line 452 "cbolgrammar.y"
          { (yyval.node) = NULL; }
#line 1811 "gram.c"
    break;

  case 125: /* statement_list_opt: statement_list  */
#line 454 "cbolgrammar.y"
          { (yyval.node) = (yyvsp[0].node); }
#line 1817 "gram.c"
    break;

  case 128: /* perform_stmt: PERFORM identifier DOT  */
#line 464 "cbolgrammar.y"
          {
		(yyval.node) = gen_perform((yyvsp[-1].string), NULL);
	  }
#line 1825 "gram.c"
    break;

  case 129: /* perform_stmt: PERFORM identifier UNTIL condition DOT  */
#line 468 "cbolgrammar.y"
          {
		(yyval.node) = gen_perform((yyvsp[-3].string), (yyvsp[-1].node));
	  }
#line 1833 "gram.c"
    break;

  case 130: /* perform_stmt: PERFORM statement_list END_PERFORM DOT  */
#line 472 "cbolgrammar.y"
          {
		(yyval.node) = (yyvsp[-2].node);
	  }
#line 1841 "gram.c"
    break;

  case 131: /* display_stmt: DISPLAY expression DOT  */
#line 479 "cbolgrammar.y"
          {
		(yyval.node) = gen_builtin_display((yyvsp[-1].node));
	  }
#line 1849 "gram.c"
    break;

  case 132: /* accept_stmt: ACCEPT identifier DOT  */
#line 486 "cbolgrammar.y"
          {
		(yyval.node) = gen_builtin_accept(make_name((yyvsp[-1].string)));
	  }
#line 1857 "gram.c"
    break;

  case 133: /* call_stmt: CALL identifier USING argument_list_opt DOT  */
#line 493 "cbolgrammar.y"
          {
		(yyval.node) = gen_call((yyvsp[-3].string), NULL);
	  }
#line 1865 "gram.c"
    break;

  case 134: /* invoke_stmt: INVOKE identifier STRING_LITERAL USING argument_list_opt RETURNING identifier DOT  */
#line 500 "cbolgrammar.y"
          {
		(yyval.node) = gen_invoke((yyvsp[-6].string), (yyvsp[-5].string), NULL);
	  }
#line 1873 "gram.c"
    break;

  case 135: /* invoke_stmt: INVOKE identifier STRING_LITERAL USING argument_list_opt DOT  */
#line 504 "cbolgrammar.y"
          {
		(yyval.node) = gen_invoke((yyvsp[-4].string), (yyvsp[-3].string), NULL);
	  }
#line 1881 "gram.c"
    break;

  case 140: /* exit_stmt: EXIT DOT  */
#line 521 "cbolgrammar.y"
          {
		(yyval.node) = NULL; /* Generate appropriate exit code */
	  }
#line 1889 "gram.c"
    break;

  case 141: /* exit_stmt: EXIT METHOD DOT  */
#line 525 "cbolgrammar.y"
          {
		(yyval.node) = NULL; /* Generate method return */
	  }
#line 1897 "gram.c"
    break;

  case 142: /* stop_stmt: STOP RUN DOT  */
#line 532 "cbolgrammar.y"
          {
		(yyval.node) = NULL; /* Generate program termination */
	  }
#line 1905 "gram.c"
    break;

  case 143: /* condition: expression EQUAL expression  */
#line 539 "cbolgrammar.y"
          { (yyval.node) = buildtree(EQ, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1911 "gram.c"
    break;

  case 144: /* condition: expression NOT_EQUAL expression  */
#line 541 "cbolgrammar.y"
          { (yyval.node) = buildtree(NE, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1917 "gram.c"
    break;

  case 145: /* condition: expression GREATER expression  */
#line 543 "cbolgrammar.y"
          { (yyval.node) = buildtree(GT, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1923 "gram.c"
    break;

  case 146: /* condition: expression LESS expression  */
#line 545 "cbolgrammar.y"
          { (yyval.node) = buildtree(LT, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1929 "gram.c"
    break;

  case 147: /* condition: expression GREATER_EQUAL expression  */
#line 547 "cbolgrammar.y"
          { (yyval.node) = buildtree(GE, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1935 "gram.c"
    break;

  case 148: /* condition: expression LESS_EQUAL expression  */
#line 549 "cbolgrammar.y"
          { (yyval.node) = buildtree(LE, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1941 "gram.c"
    break;

  case 149: /* condition: condition AND condition  */
#line 551 "cbolgrammar.y"
          { (yyval.node) = buildtree(ANDAND, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1947 "gram.c"
    break;

  case 150: /* condition: condition OR condition  */
#line 553 "cbolgrammar.y"
          { (yyval.node) = buildtree(OROR, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1953 "gram.c"
    break;

  case 151: /* condition: NOT condition  */
#line 555 "cbolgrammar.y"
          { (yyval.node) = buildtree(NOT, (yyvsp[0].node), NULL); }
#line 1959 "gram.c"
    break;

  case 152: /* condition: LPAREN condition RPAREN  */
#line 557 "cbolgrammar.y"
          { (yyval.node) = (yyvsp[-1].node); }
#line 1965 "gram.c"
    break;

  case 153: /* expression: arithmetic_expr  */
#line 562 "cbolgrammar.y"
          { (yyval.node) = (yyvsp[0].node); }
#line 1971 "gram.c"
    break;

  case 155: /* literal_expr: INTEGER  */
#line 568 "cbolgrammar.y"
          { (yyval.node) = make_icon((yyvsp[0].intval)); }
#line 1977 "gram.c"
    break;

  case 156: /* literal_expr: FLOAT  */
#line 570 "cbolgrammar.y"
          { (yyval.node) = make_icon((CONSZ)(yyvsp[0].floatval)); }
#line 1983 "gram.c"
    break;

  case 157: /* literal_expr: STRING_LITERAL  */
#line 572 "cbolgrammar.y"
          { (yyval.node) = NULL; /* Handle strings */ }
#line 1989 "gram.c"
    break;

  case 158: /* literal_expr: ZERO  */
#line 574 "cbolgrammar.y"
          { (yyval.node) = make_icon(0); }
#line 1995 "gram.c"
    break;

  case 159: /* literal_expr: SPACE  */
#line 576 "cbolgrammar.y"
          { (yyval.node) = NULL; /* Handle spaces */ }
#line 2001 "gram.c"
    break;

  case 160: /* arithmetic_expr: identifier  */
#line 581 "cbolgrammar.y"
          { (yyval.node) = make_name((yyvsp[0].string)); }
#line 2007 "gram.c"
    break;

  case 161: /* arithmetic_expr: expression PLUS expression  */
#line 583 "cbolgrammar.y"
          { (yyval.node) = buildtree(PLUS, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 2013 "gram.c"
    break;

  case 162: /* arithmetic_expr: expression MINUS expression  */
#line 585 "cbolgrammar.y"
          { (yyval.node) = buildtree(MINUS, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 2019 "gram.c"
    break;

  case 163: /* arithmetic_expr: expression TIMES expression  */
#line 587 "cbolgrammar.y"
          { (yyval.node) = buildtree(MUL, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 2025 "gram.c"
    break;

  case 164: /* arithmetic_expr: expression DIVIDE_OP expression  */
#line 589 "cbolgrammar.y"
          { (yyval.node) = buildtree(DIV, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 2031 "gram.c"
    break;

  case 165: /* arithmetic_expr: expression POWER expression  */
#line 591 "cbolgrammar.y"
          { (yyval.node) = NULL; /* Handle power */ }
#line 2037 "gram.c"
    break;

  case 166: /* arithmetic_expr: MINUS expression  */
#line 593 "cbolgrammar.y"
          { (yyval.node) = buildtree(UMINUS, (yyvsp[0].node), NULL); }
#line 2043 "gram.c"
    break;

  case 167: /* arithmetic_expr: LPAREN expression RPAREN  */
#line 595 "cbolgrammar.y"
          { (yyval.node) = (yyvsp[-1].node); }
#line 2049 "gram.c"
    break;

  case 168: /* identifier: IDENTIFIER  */
#line 600 "cbolgrammar.y"
          { (yyval.string) = (yyvsp[0].string); }
#line 2055 "gram.c"
    break;


#line 2059 "gram.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 609 "cbolgrammar.y"

