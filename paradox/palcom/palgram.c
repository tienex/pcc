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
#line 8 "palgram.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Forward declarations */
int yylex(void);
void yyerror(const char *s);


#line 83 "palgram.tab.c"

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

#include "palgram.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_AND = 3,                        /* AND  */
  YYSYMBOL_ARRAY = 4,                      /* ARRAY  */
  YYSYMBOL_AS = 5,                         /* AS  */
  YYSYMBOL_BBEGIN = 6,                     /* BBEGIN  */
  YYSYMBOL_BREAK = 7,                      /* BREAK  */
  YYSYMBOL_CASE = 8,                       /* CASE  */
  YYSYMBOL_CONST = 9,                      /* CONST  */
  YYSYMBOL_CONTINUE = 10,                  /* CONTINUE  */
  YYSYMBOL_DEFAULT = 11,                   /* DEFAULT  */
  YYSYMBOL_DO = 12,                        /* DO  */
  YYSYMBOL_DOWNTO = 13,                    /* DOWNTO  */
  YYSYMBOL_ELSE = 14,                      /* ELSE  */
  YYSYMBOL_ELSIF = 15,                     /* ELSIF  */
  YYSYMBOL_END = 16,                       /* END  */
  YYSYMBOL_ENDFOR = 17,                    /* ENDFOR  */
  YYSYMBOL_ENDFOREACH = 18,                /* ENDFOREACH  */
  YYSYMBOL_ENDIF = 19,                     /* ENDIF  */
  YYSYMBOL_ENDMETHOD = 20,                 /* ENDMETHOD  */
  YYSYMBOL_ENDPROC = 21,                   /* ENDPROC  */
  YYSYMBOL_ENDSCAN = 22,                   /* ENDSCAN  */
  YYSYMBOL_ENDSWITCH = 23,                 /* ENDSWITCH  */
  YYSYMBOL_ENDWHILE = 24,                  /* ENDWHILE  */
  YYSYMBOL_EXCEPT = 25,                    /* EXCEPT  */
  YYSYMBOL_FALSE = 26,                     /* FALSE  */
  YYSYMBOL_FOR = 27,                       /* FOR  */
  YYSYMBOL_FOREACH = 28,                   /* FOREACH  */
  YYSYMBOL_FROM = 29,                      /* FROM  */
  YYSYMBOL_FUNCTION = 30,                  /* FUNCTION  */
  YYSYMBOL_IF = 31,                        /* IF  */
  YYSYMBOL_IN = 32,                        /* IN  */
  YYSYMBOL_INTDIV = 33,                    /* INTDIV  */
  YYSYMBOL_LIKE = 34,                      /* LIKE  */
  YYSYMBOL_METHOD = 35,                    /* METHOD  */
  YYSYMBOL_MOD = 36,                       /* MOD  */
  YYSYMBOL_NOT = 37,                       /* NOT  */
  YYSYMBOL_OF = 38,                        /* OF  */
  YYSYMBOL_OR = 39,                        /* OR  */
  YYSYMBOL_PRIVATE = 40,                   /* PRIVATE  */
  YYSYMBOL_PROC = 41,                      /* PROC  */
  YYSYMBOL_PROTECTED = 42,                 /* PROTECTED  */
  YYSYMBOL_PUBLIC = 43,                    /* PUBLIC  */
  YYSYMBOL_QUIT = 44,                      /* QUIT  */
  YYSYMBOL_RECORD = 45,                    /* RECORD  */
  YYSYMBOL_RETURN = 46,                    /* RETURN  */
  YYSYMBOL_SCAN = 47,                      /* SCAN  */
  YYSYMBOL_SELF = 48,                      /* SELF  */
  YYSYMBOL_STEP = 49,                      /* STEP  */
  YYSYMBOL_SWITCH = 50,                    /* SWITCH  */
  YYSYMBOL_THEN = 51,                      /* THEN  */
  YYSYMBOL_TO = 52,                        /* TO  */
  YYSYMBOL_TRUE = 53,                      /* TRUE  */
  YYSYMBOL_TRY = 54,                       /* TRY  */
  YYSYMBOL_TYPE = 55,                      /* TYPE  */
  YYSYMBOL_UNTIL = 56,                     /* UNTIL  */
  YYSYMBOL_USES = 57,                      /* USES  */
  YYSYMBOL_VAR = 58,                       /* VAR  */
  YYSYMBOL_WHILE = 59,                     /* WHILE  */
  YYSYMBOL_WITH = 60,                      /* WITH  */
  YYSYMBOL_TSMALLINT = 61,                 /* TSMALLINT  */
  YYSYMBOL_TSHORTINT = 62,                 /* TSHORTINT  */
  YYSYMBOL_TLONGINT = 63,                  /* TLONGINT  */
  YYSYMBOL_TNUMBER = 64,                   /* TNUMBER  */
  YYSYMBOL_TCURRENCY = 65,                 /* TCURRENCY  */
  YYSYMBOL_TLOGICAL = 66,                  /* TLOGICAL  */
  YYSYMBOL_TSTRING = 67,                   /* TSTRING  */
  YYSYMBOL_TDATE = 68,                     /* TDATE  */
  YYSYMBOL_TTIME = 69,                     /* TTIME  */
  YYSYMBOL_TDATETIME = 70,                 /* TDATETIME  */
  YYSYMBOL_TTIMESTAMP = 71,                /* TTIMESTAMP  */
  YYSYMBOL_TMEMO = 72,                     /* TMEMO  */
  YYSYMBOL_TBLOB = 73,                     /* TBLOB  */
  YYSYMBOL_TGRAPHIC = 74,                  /* TGRAPHIC  */
  YYSYMBOL_TVARIANT = 75,                  /* TVARIANT  */
  YYSYMBOL_ASSIGN = 76,                    /* ASSIGN  */
  YYSYMBOL_DOTDOT = 77,                    /* DOTDOT  */
  YYSYMBOL_NE = 78,                        /* NE  */
  YYSYMBOL_LE = 79,                        /* LE  */
  YYSYMBOL_GE = 80,                        /* GE  */
  YYSYMBOL_EQ = 81,                        /* EQ  */
  YYSYMBOL_LT = 82,                        /* LT  */
  YYSYMBOL_GT = 83,                        /* GT  */
  YYSYMBOL_PLUS = 84,                      /* PLUS  */
  YYSYMBOL_MINUS = 85,                     /* MINUS  */
  YYSYMBOL_STAR = 86,                      /* STAR  */
  YYSYMBOL_SLASH = 87,                     /* SLASH  */
  YYSYMBOL_UPARROW = 88,                   /* UPARROW  */
  YYSYMBOL_AT = 89,                        /* AT  */
  YYSYMBOL_DOT = 90,                       /* DOT  */
  YYSYMBOL_COMMA = 91,                     /* COMMA  */
  YYSYMBOL_COLON = 92,                     /* COLON  */
  YYSYMBOL_SEMICOLON = 93,                 /* SEMICOLON  */
  YYSYMBOL_LPAREN = 94,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 95,                    /* RPAREN  */
  YYSYMBOL_LBRACK = 96,                    /* LBRACK  */
  YYSYMBOL_RBRACK = 97,                    /* RBRACK  */
  YYSYMBOL_LBRACE = 98,                    /* LBRACE  */
  YYSYMBOL_RBRACE = 99,                    /* RBRACE  */
  YYSYMBOL_INTCONST = 100,                 /* INTCONST  */
  YYSYMBOL_LOGICALCONST = 101,             /* LOGICALCONST  */
  YYSYMBOL_NUMBERCONST = 102,              /* NUMBERCONST  */
  YYSYMBOL_STRINGCONST = 103,              /* STRINGCONST  */
  YYSYMBOL_IDENT = 104,                    /* IDENT  */
  YYSYMBOL_YYACCEPT = 105,                 /* $accept  */
  YYSYMBOL_program = 106,                  /* program  */
  YYSYMBOL_toplevel_list = 107,            /* toplevel_list  */
  YYSYMBOL_toplevel_decl = 108,            /* toplevel_decl  */
  YYSYMBOL_uses_declaration = 109,         /* uses_declaration  */
  YYSYMBOL_identifier_list = 110,          /* identifier_list  */
  YYSYMBOL_var_declaration = 111,          /* var_declaration  */
  YYSYMBOL_var_decl_list = 112,            /* var_decl_list  */
  YYSYMBOL_var_decl = 113,                 /* var_decl  */
  YYSYMBOL_const_declaration = 114,        /* const_declaration  */
  YYSYMBOL_const_decl_list = 115,          /* const_decl_list  */
  YYSYMBOL_const_decl = 116,               /* const_decl  */
  YYSYMBOL_type_declaration = 117,         /* type_declaration  */
  YYSYMBOL_type_decl_list = 118,           /* type_decl_list  */
  YYSYMBOL_type_decl = 119,                /* type_decl  */
  YYSYMBOL_type_spec = 120,                /* type_spec  */
  YYSYMBOL_simple_type = 121,              /* simple_type  */
  YYSYMBOL_array_type = 122,               /* array_type  */
  YYSYMBOL_record_type = 123,              /* record_type  */
  YYSYMBOL_field_list = 124,               /* field_list  */
  YYSYMBOL_field_decl = 125,               /* field_decl  */
  YYSYMBOL_proc_declaration = 126,         /* proc_declaration  */
  YYSYMBOL_proc_body = 127,                /* proc_body  */
  YYSYMBOL_local_declarations = 128,       /* local_declarations  */
  YYSYMBOL_method_declaration = 129,       /* method_declaration  */
  YYSYMBOL_visibility = 130,               /* visibility  */
  YYSYMBOL_method_body = 131,              /* method_body  */
  YYSYMBOL_opt_param_list = 132,           /* opt_param_list  */
  YYSYMBOL_param_list = 133,               /* param_list  */
  YYSYMBOL_param_decl = 134,               /* param_decl  */
  YYSYMBOL_statement_list = 135,           /* statement_list  */
  YYSYMBOL_statement = 136,                /* statement  */
  YYSYMBOL_simple_statement = 137,         /* simple_statement  */
  YYSYMBOL_assignment_statement = 138,     /* assignment_statement  */
  YYSYMBOL_lvalue = 139,                   /* lvalue  */
  YYSYMBOL_procedure_call = 140,           /* procedure_call  */
  YYSYMBOL_opt_arguments = 141,            /* opt_arguments  */
  YYSYMBOL_argument_list = 142,            /* argument_list  */
  YYSYMBOL_return_statement = 143,         /* return_statement  */
  YYSYMBOL_break_statement = 144,          /* break_statement  */
  YYSYMBOL_continue_statement = 145,       /* continue_statement  */
  YYSYMBOL_quit_statement = 146,           /* quit_statement  */
  YYSYMBOL_structured_statement = 147,     /* structured_statement  */
  YYSYMBOL_if_statement = 148,             /* if_statement  */
  YYSYMBOL_endif_part = 149,               /* endif_part  */
  YYSYMBOL_elsif_list = 150,               /* elsif_list  */
  YYSYMBOL_while_statement = 151,          /* while_statement  */
  YYSYMBOL_for_statement = 152,            /* for_statement  */
  YYSYMBOL_for_body = 153,                 /* for_body  */
  YYSYMBOL_foreach_statement = 154,        /* foreach_statement  */
  YYSYMBOL_foreach_body = 155,             /* foreach_body  */
  YYSYMBOL_switch_statement = 156,         /* switch_statement  */
  YYSYMBOL_case_list = 157,                /* case_list  */
  YYSYMBOL_case_clause = 158,              /* case_clause  */
  YYSYMBOL_endswitch_part = 159,           /* endswitch_part  */
  YYSYMBOL_try_statement = 160,            /* try_statement  */
  YYSYMBOL_scan_statement = 161,           /* scan_statement  */
  YYSYMBOL_begin_end_block = 162,          /* begin_end_block  */
  YYSYMBOL_expression = 163,               /* expression  */
  YYSYMBOL_simple_expression = 164,        /* simple_expression  */
  YYSYMBOL_term = 165,                     /* term  */
  YYSYMBOL_factor = 166,                   /* factor  */
  YYSYMBOL_primary = 167,                  /* primary  */
  YYSYMBOL_function_call = 168,            /* function_call  */
  YYSYMBOL_constant = 169,                 /* constant  */
  YYSYMBOL_identifier = 170                /* identifier  */
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
#define YYFINAL  34
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   860

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  105
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  66
/* YYNRULES -- Number of rules.  */
#define YYNRULES  166
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  311

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   359


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
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
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    76,    76,    77,    82,    83,    87,    88,    89,    90,
      91,    92,    97,   102,   103,   108,   112,   113,   117,   123,
     127,   128,   132,   138,   142,   143,   147,   153,   155,   157,
     162,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   174,   175,   176,   177,   182,   184,   189,   194,
     196,   201,   207,   212,   215,   217,   218,   219,   224,   229,
     237,   238,   239,   243,   249,   250,   252,   257,   259,   264,
     266,   268,   273,   275,   279,   280,   284,   285,   286,   287,
     288,   289,   293,   295,   300,   301,   303,   308,   310,   314,
     316,   317,   321,   322,   326,   328,   333,   341,   349,   355,
     356,   357,   358,   359,   360,   361,   362,   366,   370,   371,
     372,   373,   377,   378,   382,   390,   392,   394,   399,   403,
     411,   415,   423,   424,   428,   432,   433,   437,   445,   450,
     455,   456,   458,   460,   462,   464,   466,   468,   470,   475,
     476,   478,   480,   482,   484,   489,   490,   492,   494,   496,
     498,   503,   504,   506,   508,   513,   514,   515,   516,   518,
     523,   525,   530,   532,   534,   536,   541
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
  "\"end of file\"", "error", "\"invalid token\"", "AND", "ARRAY", "AS",
  "BBEGIN", "BREAK", "CASE", "CONST", "CONTINUE", "DEFAULT", "DO",
  "DOWNTO", "ELSE", "ELSIF", "END", "ENDFOR", "ENDFOREACH", "ENDIF",
  "ENDMETHOD", "ENDPROC", "ENDSCAN", "ENDSWITCH", "ENDWHILE", "EXCEPT",
  "FALSE", "FOR", "FOREACH", "FROM", "FUNCTION", "IF", "IN", "INTDIV",
  "LIKE", "METHOD", "MOD", "NOT", "OF", "OR", "PRIVATE", "PROC",
  "PROTECTED", "PUBLIC", "QUIT", "RECORD", "RETURN", "SCAN", "SELF",
  "STEP", "SWITCH", "THEN", "TO", "TRUE", "TRY", "TYPE", "UNTIL", "USES",
  "VAR", "WHILE", "WITH", "TSMALLINT", "TSHORTINT", "TLONGINT", "TNUMBER",
  "TCURRENCY", "TLOGICAL", "TSTRING", "TDATE", "TTIME", "TDATETIME",
  "TTIMESTAMP", "TMEMO", "TBLOB", "TGRAPHIC", "TVARIANT", "ASSIGN",
  "DOTDOT", "NE", "LE", "GE", "EQ", "LT", "GT", "PLUS", "MINUS", "STAR",
  "SLASH", "UPARROW", "AT", "DOT", "COMMA", "COLON", "SEMICOLON", "LPAREN",
  "RPAREN", "LBRACK", "RBRACK", "LBRACE", "RBRACE", "INTCONST",
  "LOGICALCONST", "NUMBERCONST", "STRINGCONST", "IDENT", "$accept",
  "program", "toplevel_list", "toplevel_decl", "uses_declaration",
  "identifier_list", "var_declaration", "var_decl_list", "var_decl",
  "const_declaration", "const_decl_list", "const_decl", "type_declaration",
  "type_decl_list", "type_decl", "type_spec", "simple_type", "array_type",
  "record_type", "field_list", "field_decl", "proc_declaration",
  "proc_body", "local_declarations", "method_declaration", "visibility",
  "method_body", "opt_param_list", "param_list", "param_decl",
  "statement_list", "statement", "simple_statement",
  "assignment_statement", "lvalue", "procedure_call", "opt_arguments",
  "argument_list", "return_statement", "break_statement",
  "continue_statement", "quit_statement", "structured_statement",
  "if_statement", "endif_part", "elsif_list", "while_statement",
  "for_statement", "for_body", "foreach_statement", "foreach_body",
  "switch_statement", "case_list", "case_clause", "endswitch_part",
  "try_statement", "scan_statement", "begin_end_block", "expression",
  "simple_expression", "term", "factor", "primary", "function_call",
  "constant", "identifier", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-233)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-86)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     802,   -65,   -65,  -233,   -65,  -233,  -233,   -65,   -65,   -65,
      55,   802,  -233,  -233,  -233,  -233,  -233,  -233,  -233,    21,
    -233,   -65,  -233,   -14,   -22,   -22,   -65,  -233,     2,    26,
    -233,    -2,   -65,  -233,  -233,  -233,   -65,  -233,   610,     0,
    -233,  -233,  -233,   309,   -65,  -233,   309,  -233,   -22,   629,
    -233,   629,   629,   629,   629,   610,  -233,  -233,  -233,  -233,
     -74,   674,    18,    14,  -233,  -233,  -233,  -233,    13,   -65,
     -65,  -233,   -62,  -233,    24,     5,    88,   105,     5,    49,
     -65,  -233,  -233,  -233,  -233,  -233,  -233,  -233,  -233,  -233,
    -233,  -233,  -233,  -233,  -233,  -233,    54,  -233,  -233,  -233,
    -233,  -233,    58,  -233,  -233,    14,    14,  -233,  -233,    32,
     -65,   610,   610,   610,   610,   610,   610,   610,   610,   610,
    -233,   629,   629,   629,   629,   629,   629,   629,   629,   542,
      61,    63,     1,  -233,   309,  -233,  -233,  -233,   575,  -233,
    -233,   575,   610,    43,    -5,  -233,  -233,  -233,   132,  -233,
    -233,   245,    18,    18,    18,    18,    18,    18,    18,    18,
      14,    14,    14,  -233,  -233,  -233,  -233,  -233,  -233,   -61,
     749,   309,   309,  -233,  -233,  -233,  -233,  -233,   -65,   -65,
     610,  -233,   610,   -65,   610,  -233,   610,  -233,  -233,  -233,
     -44,  -233,  -233,  -233,  -233,  -233,  -233,  -233,  -233,  -233,
    -233,  -233,  -233,  -233,  -233,    73,   668,   309,  -233,  -233,
    -233,  -233,   610,  -233,  -233,  -233,   289,    75,   130,   515,
     749,    72,   184,   379,    42,   610,   610,   -65,   587,  -233,
     610,   128,    79,   749,  -233,   610,   610,  -233,  -233,   610,
      62,  -233,  -233,  -233,   749,   749,   148,  -233,    41,   494,
     309,  -233,   191,   749,    34,   390,   690,    76,  -233,  -233,
    -233,   425,   454,  -233,  -233,   135,  -233,   610,   610,   575,
     156,  -233,   610,  -233,  -233,     9,  -233,  -233,  -233,  -233,
    -233,   309,   749,   709,  -233,   496,   727,  -233,   610,  -233,
     575,   510,  -233,   575,   158,   610,   162,  -233,  -233,   561,
     742,  -233,  -233,   749,  -233,   575,  -233,  -233,   166,   575,
    -233
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,    60,     0,    61,    62,     0,     0,     0,
       0,     3,     4,    11,     6,     7,     8,     9,    10,     0,
     166,    19,    20,     0,    64,    64,    23,    24,     0,     0,
      13,     0,    15,    16,     1,     5,     0,    21,     0,     0,
      54,    54,    25,     0,     0,    12,     0,    17,    64,     0,
     159,     0,     0,     0,     0,     0,   162,   165,   163,   164,
     156,     0,   130,   139,   145,   151,   157,   155,    84,     0,
       0,    65,     0,    67,     0,    72,     0,     0,    72,     0,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,    27,    28,    29,
      45,    14,     0,    54,   152,   140,   141,   153,   154,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      22,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    66,     0,    55,    56,    57,    63,    58,
      52,    53,     0,     0,     0,    49,    26,    18,     0,   158,
      85,     0,   137,   138,   132,   134,   136,   131,   133,   135,
     144,   142,   143,   150,   148,   149,   146,   147,   161,     0,
      92,     0,     0,    68,    69,    72,    96,    97,     0,     0,
       0,    98,    94,     0,     0,    72,     0,    73,    74,    76,
       0,    77,    78,    79,    80,    81,    75,    99,   100,   101,
     102,   103,   104,   105,   106,    89,     0,     0,    48,    50,
      59,    86,     0,   160,    71,    70,     0,     0,     0,     0,
      95,     0,     0,     0,     0,     0,     0,     0,     0,    87,
       0,     0,     0,    93,   129,     0,     0,    72,    72,     0,
       0,   122,    72,    72,    82,    83,    89,    90,     0,     0,
       0,    51,     0,    72,     0,     0,     0,     0,   125,   123,
     121,     0,     0,    88,    91,     0,    46,     0,     0,   120,
       0,    72,     0,   108,   107,     0,   128,    72,    72,   127,
     114,     0,    72,    72,   119,     0,     0,    72,     0,   109,
     124,     0,    47,   118,     0,     0,     0,   111,    72,     0,
       0,   126,   116,    72,   115,   112,   110,    72,     0,   113,
     117
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -233,  -233,  -233,   173,  -233,    -3,   -63,  -233,   155,    16,
    -233,   167,    64,  -233,   165,   -42,  -233,  -233,  -233,  -233,
      51,  -233,  -233,   152,  -233,  -233,    94,   -12,  -233,    70,
     -57,  -233,  -233,  -233,    60,  -233,   -41,   -21,  -233,  -233,
    -233,  -233,  -233,  -233,  -233,  -233,  -233,  -233,  -232,  -233,
    -233,  -233,  -233,   -31,  -233,  -233,  -233,  -233,   -36,   374,
     -25,    33,  -233,  -233,  -233,    -1
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    10,    11,    12,    13,    31,    14,    32,    33,    15,
      21,    22,    16,    26,    27,    96,    97,    98,    99,   144,
     145,    17,    77,    75,    18,    19,    76,    40,    72,    73,
     293,   187,   188,   189,    60,   191,   229,   169,   192,   193,
     194,   195,   196,   197,   274,   275,   198,   199,   294,   200,
     270,   201,   240,   241,   260,   202,   203,   204,   170,    62,
      63,    64,    65,    66,    67,    68
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      23,    24,    61,    25,   102,    29,    28,    30,    30,    69,
      69,   208,   135,    41,     1,   135,   110,   124,   138,   109,
      23,   141,   111,   287,   288,    28,   105,   106,   289,   132,
     212,    30,   225,   133,   213,    48,   103,   226,    74,    20,
     175,   176,   100,   101,   177,   100,   227,   125,   271,   272,
     126,   296,   111,   273,   243,    34,    36,   121,    70,    70,
       7,   178,   179,     9,   112,   180,   113,    38,   130,   131,
     239,   308,    39,   257,   112,   151,   113,   143,   181,    30,
     182,   183,   104,    43,   184,   258,   107,   108,   185,    44,
      46,   136,   174,   186,   136,    71,   160,   161,   162,    20,
     127,   128,   122,   123,    20,    20,   206,   129,   139,   150,
     114,   115,   116,   117,   118,   119,   134,    44,   216,    45,
     114,   115,   116,   117,   118,   119,   140,   149,   223,   214,
     215,    74,   212,   100,    44,   207,   264,   205,    20,   137,
     205,   143,   137,    30,   219,   142,   220,   146,   222,   -84,
     224,   147,   210,   171,   -84,   172,   235,   163,   164,   165,
     166,   167,   236,   -84,   238,   232,   250,   228,   278,   -84,
     100,   100,   251,   281,   284,   302,   233,   217,   218,   304,
     254,   255,   221,   310,    35,   261,   262,    47,    37,   244,
     245,    42,   239,    78,   249,   209,   269,   148,   190,   252,
     253,   190,   173,   256,   267,   263,   100,   248,   266,   259,
       0,     0,     0,     0,   285,   205,   112,     0,   113,     0,
     290,   291,   205,   112,   -85,   113,   246,     0,     0,   -85,
     299,   282,   283,     0,     0,     0,   286,     0,   -85,   292,
       0,   305,   228,   268,   -85,     0,     0,     0,     0,   100,
     309,     0,   300,   205,   205,     0,     0,     0,     0,   303,
     205,   205,   114,   115,   116,   117,   118,   119,   205,   114,
     115,   116,   117,   118,   119,     0,   190,   112,     0,   113,
     100,     0,     0,   190,   205,     0,     0,     0,     0,   205,
     205,     0,   205,     0,     0,   175,   176,     0,   205,   177,
       0,     0,     0,     0,   205,   234,     0,     0,   205,     0,
       0,     0,     0,    79,   190,   190,   178,   179,     0,     0,
     180,   190,   190,   114,   115,   116,   117,   118,   119,   190,
       0,     0,     0,   181,     0,   182,   183,     0,     0,   184,
       0,     0,   211,   185,     0,   190,     0,     0,   186,     0,
     190,   190,     0,   190,    80,     0,     0,     0,     0,   190,
       0,     0,     0,     0,     0,   190,     0,     0,     0,   190,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,   175,   176,     0,     0,   177,
       0,     0,     0,    20,     0,     0,   175,   176,     0,     0,
     177,     0,     0,     0,   242,     0,   178,   179,     0,     0,
     180,     0,   276,    20,     0,     0,     0,   178,   179,     0,
       0,   180,     0,   181,     0,   182,   183,     0,     0,   184,
       0,   175,   176,   185,   181,   177,   182,   183,   186,     0,
     184,   279,     0,     0,   185,     0,     0,     0,     0,   186,
       0,     0,   178,   179,     0,     0,   180,     0,     0,     0,
     175,   176,     0,     0,   177,     0,     0,     0,     0,   181,
       0,   182,   183,     0,     0,   184,     0,     0,   280,   185,
       0,   178,   179,    20,   186,   180,   152,   153,   154,   155,
     156,   157,   158,   159,    20,     0,     0,     0,   181,     0,
     182,   183,   175,   176,   184,     0,   177,     0,   185,     0,
       0,     0,     0,   186,     0,   297,   175,   176,     0,     0,
     177,     0,     0,   178,   179,     0,   112,   180,   113,    20,
       0,     0,     0,   301,     0,     0,     0,   178,   179,     0,
     181,   180,   182,   183,     0,     0,   184,   112,     0,   113,
     185,     0,     0,     0,   181,   186,   182,   183,    20,     0,
     184,     0,     0,     0,   185,     0,   237,   175,   176,   186,
       0,   177,   114,   115,   116,   117,   118,   119,     0,    49,
     306,   175,   176,     0,     0,   177,     0,     0,   178,   179,
      50,   265,   180,   114,   115,   116,   117,   118,   119,     0,
      20,     0,   178,   179,     0,   181,   180,   182,   183,     0,
       0,   184,     0,     0,    20,   185,     0,     0,     0,   181,
     186,   182,   183,     0,    49,   184,    51,    52,     0,   185,
      53,    54,     0,     0,   186,    50,    55,   168,     0,     0,
       0,     0,    56,    57,    58,    59,    20,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,     0,
       0,     0,     0,     0,     0,    20,    49,     0,     0,     0,
       0,    51,    52,     0,     0,    53,    54,    50,     0,    20,
       0,    55,   247,     0,     0,     0,     0,    56,    57,    58,
      59,    20,     0,     0,    51,    52,     0,     0,    53,    54,
     112,     0,   113,     0,    55,     0,   112,     0,   113,     0,
      56,    57,    58,    59,    20,     0,     0,    53,    54,     0,
       0,     0,   112,    55,   113,     0,     0,     0,     0,    56,
      57,    58,    59,    20,     0,     0,     0,     0,     0,     0,
       0,   112,     0,   113,     0,   230,   114,   115,   116,   117,
     118,   119,   114,   115,   116,   117,   118,   119,   295,   112,
       0,   113,     0,     0,     0,   231,     0,   120,   114,   115,
     116,   117,   118,   119,   112,     0,   113,     0,   298,     0,
       0,   112,   277,   113,     0,     0,     0,   114,   115,   116,
     117,   118,   119,   307,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   114,   115,   116,   117,   118,
     119,     1,     0,     0,     0,     0,     0,     0,     0,     0,
     114,   115,   116,   117,   118,   119,     0,   114,   115,   116,
     117,   118,   119,     0,     0,     0,     0,     2,     0,     0,
       0,     0,     3,     4,     5,     6,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     7,     0,     8,
       9
};

static const yytype_int16 yycheck[] =
{
       1,     2,    38,     4,    46,     8,     7,     8,     9,     9,
       9,    16,    75,    25,     9,    78,    90,     3,    75,    55,
      21,    78,    96,    14,    15,    26,    51,    52,    19,    91,
      91,    32,    76,    95,    95,    36,    48,    81,    39,   104,
       6,     7,    43,    44,    10,    46,    90,    33,    14,    15,
      36,   283,    96,    19,    12,     0,    35,    39,    58,    58,
      55,    27,    28,    58,    32,    31,    34,    81,    69,    70,
       8,   303,    94,    11,    32,   111,    34,    80,    44,    80,
      46,    47,    49,    81,    50,    23,    53,    54,    54,    91,
      92,    75,   134,    59,    78,    95,   121,   122,   123,   104,
      86,    87,    84,    85,   104,   104,   142,    94,    20,   110,
      78,    79,    80,    81,    82,    83,    92,    91,   175,    93,
      78,    79,    80,    81,    82,    83,    21,    95,   185,   171,
     172,   132,    91,   134,    91,    92,    95,   138,   104,    75,
     141,   144,    78,   144,   180,    96,   182,    93,   184,    76,
     186,    93,    20,    92,    81,    92,    81,   124,   125,   126,
     127,   128,    32,    90,    92,   207,    38,    94,    92,    96,
     171,   172,    93,    38,    18,    17,   212,   178,   179,    17,
     237,   238,   183,    17,    11,   242,   243,    32,    21,   225,
     226,    26,     8,    41,   230,   144,   253,   103,   138,   235,
     236,   141,   132,   239,    13,   246,   207,   228,   250,   240,
      -1,    -1,    -1,    -1,   271,   216,    32,    -1,    34,    -1,
     277,   278,   223,    32,    76,    34,   227,    -1,    -1,    81,
     287,   267,   268,    -1,    -1,    -1,   272,    -1,    90,   281,
      -1,   298,    94,    52,    96,    -1,    -1,    -1,    -1,   250,
     307,    -1,   288,   254,   255,    -1,    -1,    -1,    -1,   295,
     261,   262,    78,    79,    80,    81,    82,    83,   269,    78,
      79,    80,    81,    82,    83,    -1,   216,    32,    -1,    34,
     281,    -1,    -1,   223,   285,    -1,    -1,    -1,    -1,   290,
     291,    -1,   293,    -1,    -1,     6,     7,    -1,   299,    10,
      -1,    -1,    -1,    -1,   305,    16,    -1,    -1,   309,    -1,
      -1,    -1,    -1,     4,   254,   255,    27,    28,    -1,    -1,
      31,   261,   262,    78,    79,    80,    81,    82,    83,   269,
      -1,    -1,    -1,    44,    -1,    46,    47,    -1,    -1,    50,
      -1,    -1,    97,    54,    -1,   285,    -1,    -1,    59,    -1,
     290,   291,    -1,   293,    45,    -1,    -1,    -1,    -1,   299,
      -1,    -1,    -1,    -1,    -1,   305,    -1,    -1,    -1,   309,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,     6,     7,    -1,    -1,    10,
      -1,    -1,    -1,   104,    -1,    -1,     6,     7,    -1,    -1,
      10,    -1,    -1,    -1,    25,    -1,    27,    28,    -1,    -1,
      31,    -1,    22,   104,    -1,    -1,    -1,    27,    28,    -1,
      -1,    31,    -1,    44,    -1,    46,    47,    -1,    -1,    50,
      -1,     6,     7,    54,    44,    10,    46,    47,    59,    -1,
      50,    16,    -1,    -1,    54,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    27,    28,    -1,    -1,    31,    -1,    -1,    -1,
       6,     7,    -1,    -1,    10,    -1,    -1,    -1,    -1,    44,
      -1,    46,    47,    -1,    -1,    50,    -1,    -1,    24,    54,
      -1,    27,    28,   104,    59,    31,   112,   113,   114,   115,
     116,   117,   118,   119,   104,    -1,    -1,    -1,    44,    -1,
      46,    47,     6,     7,    50,    -1,    10,    -1,    54,    -1,
      -1,    -1,    -1,    59,    -1,    19,     6,     7,    -1,    -1,
      10,    -1,    -1,    27,    28,    -1,    32,    31,    34,   104,
      -1,    -1,    -1,    23,    -1,    -1,    -1,    27,    28,    -1,
      44,    31,    46,    47,    -1,    -1,    50,    32,    -1,    34,
      54,    -1,    -1,    -1,    44,    59,    46,    47,   104,    -1,
      50,    -1,    -1,    -1,    54,    -1,    51,     6,     7,    59,
      -1,    10,    78,    79,    80,    81,    82,    83,    -1,    37,
      19,     6,     7,    -1,    -1,    10,    -1,    -1,    27,    28,
      48,    97,    31,    78,    79,    80,    81,    82,    83,    -1,
     104,    -1,    27,    28,    -1,    44,    31,    46,    47,    -1,
      -1,    50,    -1,    -1,   104,    54,    -1,    -1,    -1,    44,
      59,    46,    47,    -1,    37,    50,    84,    85,    -1,    54,
      88,    89,    -1,    -1,    59,    48,    94,    95,    -1,    -1,
      -1,    -1,   100,   101,   102,   103,   104,    37,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    37,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    88,    89,    48,    -1,   104,
      -1,    94,    95,    -1,    -1,    -1,    -1,   100,   101,   102,
     103,   104,    -1,    -1,    84,    85,    -1,    -1,    88,    89,
      32,    -1,    34,    -1,    94,    -1,    32,    -1,    34,    -1,
     100,   101,   102,   103,   104,    -1,    -1,    88,    89,    -1,
      -1,    -1,    32,    94,    34,    -1,    -1,    -1,    -1,   100,
     101,   102,   103,   104,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    32,    -1,    34,    -1,    77,    78,    79,    80,    81,
      82,    83,    78,    79,    80,    81,    82,    83,    49,    32,
      -1,    34,    -1,    -1,    -1,    97,    -1,    93,    78,    79,
      80,    81,    82,    83,    32,    -1,    34,    -1,    51,    -1,
      -1,    32,    92,    34,    -1,    -1,    -1,    78,    79,    80,
      81,    82,    83,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,    79,    80,    81,    82,
      83,     9,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    79,    80,    81,    82,    83,    -1,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    35,    -1,    -1,
      -1,    -1,    40,    41,    42,    43,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,    57,
      58
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     9,    35,    40,    41,    42,    43,    55,    57,    58,
     106,   107,   108,   109,   111,   114,   117,   126,   129,   130,
     104,   115,   116,   170,   170,   170,   118,   119,   170,   110,
     170,   110,   112,   113,     0,   108,    35,   116,    81,    94,
     132,   132,   119,    81,    91,    93,    92,   113,   170,    37,
      48,    84,    85,    88,    89,    94,   100,   101,   102,   103,
     139,   163,   164,   165,   166,   167,   168,   169,   170,     9,
      58,    95,   133,   134,   170,   128,   131,   127,   128,     4,
      45,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,   120,   121,   122,   123,
     170,   170,   120,   132,   166,   165,   165,   166,   166,   163,
      90,    96,    32,    34,    78,    79,    80,    81,    82,    83,
      93,    39,    84,    85,     3,    33,    36,    86,    87,    94,
     170,   170,    91,    95,    92,   111,   114,   117,   135,    20,
      21,   135,    96,   110,   124,   125,    93,    93,   131,    95,
     170,   163,   164,   164,   164,   164,   164,   164,   164,   164,
     165,   165,   165,   166,   166,   166,   166,   166,    95,   142,
     163,    92,    92,   134,   120,     6,     7,    10,    27,    28,
      31,    44,    46,    47,    50,    54,    59,   136,   137,   138,
     139,   140,   143,   144,   145,   146,   147,   148,   151,   152,
     154,   156,   160,   161,   162,   170,   163,    92,    16,   125,
      20,    97,    91,    95,   120,   120,   135,   170,   170,   163,
     163,   170,   163,   135,   163,    76,    81,    90,    94,   141,
      77,    97,   120,   163,    16,    81,    32,    51,    92,     8,
     157,   158,    25,    12,   163,   163,   170,    95,   142,   163,
      38,    93,   163,   163,   135,   135,   163,    11,    23,   158,
     159,   135,   135,   141,    95,    97,   120,    13,    52,   135,
     155,    14,    15,    19,   149,   150,    22,    92,    92,    16,
      24,    38,   163,   163,    18,   135,   163,    14,    15,    19,
     135,   135,   120,   135,   153,    49,   153,    19,    51,   135,
     163,    23,    17,   163,    17,   135,    19,    51,   153,   135,
      17
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,   105,   106,   106,   107,   107,   108,   108,   108,   108,
     108,   108,   109,   110,   110,   111,   112,   112,   113,   114,
     115,   115,   116,   117,   118,   118,   119,   120,   120,   120,
     121,   121,   121,   121,   121,   121,   121,   121,   121,   121,
     121,   121,   121,   121,   121,   121,   122,   122,   123,   124,
     124,   125,   126,   127,   128,   128,   128,   128,   129,   129,
     130,   130,   130,   131,   132,   132,   132,   133,   133,   134,
     134,   134,   135,   135,   136,   136,   137,   137,   137,   137,
     137,   137,   138,   138,   139,   139,   139,   140,   140,   141,
     141,   141,   142,   142,   143,   143,   144,   145,   146,   147,
     147,   147,   147,   147,   147,   147,   147,   148,   149,   149,
     149,   149,   150,   150,   151,   152,   152,   152,   153,   154,
     155,   156,   157,   157,   158,   159,   159,   160,   161,   162,
     163,   163,   163,   163,   163,   163,   163,   163,   163,   164,
     164,   164,   164,   164,   164,   165,   165,   165,   165,   165,
     165,   166,   166,   166,   166,   167,   167,   167,   167,   167,
     168,   168,   169,   169,   169,   169,   170
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     3,     1,     3,     2,     1,     2,     4,     2,
       1,     2,     4,     2,     1,     2,     4,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     6,     8,     3,     1,
       2,     4,     5,     2,     0,     2,     2,     2,     5,     6,
       1,     1,     1,     2,     0,     2,     3,     1,     3,     3,
       4,     4,     0,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     1,     3,     4,     2,     4,     0,
       2,     3,     1,     3,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     5,     1,     2,
       4,     3,     4,     5,     5,     8,     8,    10,     1,     6,
       1,     4,     1,     2,     4,     1,     4,     5,     5,     3,
       1,     3,     3,     3,     3,     3,     3,     3,     3,     1,
       2,     2,     3,     3,     3,     1,     3,     3,     3,     3,
       3,     1,     2,     2,     2,     1,     1,     1,     3,     1,
       4,     3,     1,     1,     1,     1,     1
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
  case 2: /* program: %empty  */
#line 76 "palgram.y"
          { /* Empty */ }
#line 1591 "palgram.tab.c"
    break;

  case 3: /* program: toplevel_list  */
#line 78 "palgram.y"
          { /* Program with declarations/procedures */ }
#line 1597 "palgram.tab.c"
    break;

  case 12: /* uses_declaration: USES identifier_list SEMICOLON  */
#line 98 "palgram.y"
          { /* Import libraries */ }
#line 1603 "palgram.tab.c"
    break;

  case 18: /* var_decl: identifier_list COLON type_spec SEMICOLON  */
#line 118 "palgram.y"
          { /* Declare variables */ }
#line 1609 "palgram.tab.c"
    break;

  case 22: /* const_decl: identifier EQ expression SEMICOLON  */
#line 133 "palgram.y"
          { /* Declare constant */ }
#line 1615 "palgram.tab.c"
    break;

  case 26: /* type_decl: identifier EQ type_spec SEMICOLON  */
#line 148 "palgram.y"
          { /* Define type alias */ }
#line 1621 "palgram.tab.c"
    break;

  case 27: /* type_spec: simple_type  */
#line 154 "palgram.y"
          { (yyval.typeptr) = (yyvsp[0].typeptr); }
#line 1627 "palgram.tab.c"
    break;

  case 28: /* type_spec: array_type  */
#line 156 "palgram.y"
          { (yyval.typeptr) = (yyvsp[0].typeptr); }
#line 1633 "palgram.tab.c"
    break;

  case 29: /* type_spec: record_type  */
#line 158 "palgram.y"
          { (yyval.typeptr) = (yyvsp[0].typeptr); }
#line 1639 "palgram.tab.c"
    break;

  case 30: /* simple_type: TSMALLINT  */
#line 162 "palgram.y"
                        { (yyval.typeptr) = mktype(TSMALLINT); }
#line 1645 "palgram.tab.c"
    break;

  case 31: /* simple_type: TSHORTINT  */
#line 163 "palgram.y"
                        { (yyval.typeptr) = mktype(TSHORTINT); }
#line 1651 "palgram.tab.c"
    break;

  case 32: /* simple_type: TLONGINT  */
#line 164 "palgram.y"
                        { (yyval.typeptr) = mktype(TLONGINT); }
#line 1657 "palgram.tab.c"
    break;

  case 33: /* simple_type: TNUMBER  */
#line 165 "palgram.y"
                        { (yyval.typeptr) = mktype(TNUMBER); }
#line 1663 "palgram.tab.c"
    break;

  case 34: /* simple_type: TCURRENCY  */
#line 166 "palgram.y"
                        { (yyval.typeptr) = mktype(TCURRENCY); }
#line 1669 "palgram.tab.c"
    break;

  case 35: /* simple_type: TLOGICAL  */
#line 167 "palgram.y"
                        { (yyval.typeptr) = mktype(TLOGICAL); }
#line 1675 "palgram.tab.c"
    break;

  case 36: /* simple_type: TSTRING  */
#line 168 "palgram.y"
                        { (yyval.typeptr) = mktype(TALPHANUMERIC); }
#line 1681 "palgram.tab.c"
    break;

  case 37: /* simple_type: TDATE  */
#line 169 "palgram.y"
                        { (yyval.typeptr) = mktype(TDATE); }
#line 1687 "palgram.tab.c"
    break;

  case 38: /* simple_type: TTIME  */
#line 170 "palgram.y"
                        { (yyval.typeptr) = mktype(TTIME); }
#line 1693 "palgram.tab.c"
    break;

  case 39: /* simple_type: TDATETIME  */
#line 171 "palgram.y"
                        { (yyval.typeptr) = mktype(TDATETIME); }
#line 1699 "palgram.tab.c"
    break;

  case 40: /* simple_type: TTIMESTAMP  */
#line 172 "palgram.y"
                        { (yyval.typeptr) = mktype(TTIMESTAMP); }
#line 1705 "palgram.tab.c"
    break;

  case 41: /* simple_type: TMEMO  */
#line 173 "palgram.y"
                        { (yyval.typeptr) = mktype(TMEMO); }
#line 1711 "palgram.tab.c"
    break;

  case 42: /* simple_type: TBLOB  */
#line 174 "palgram.y"
                        { (yyval.typeptr) = mktype(TBLOB); }
#line 1717 "palgram.tab.c"
    break;

  case 43: /* simple_type: TGRAPHIC  */
#line 175 "palgram.y"
                        { (yyval.typeptr) = mktype(TGRAPHIC); }
#line 1723 "palgram.tab.c"
    break;

  case 44: /* simple_type: TVARIANT  */
#line 176 "palgram.y"
                        { (yyval.typeptr) = mktype(TVARIANT); }
#line 1729 "palgram.tab.c"
    break;

  case 45: /* simple_type: identifier  */
#line 178 "palgram.y"
          { /* Type reference */ (yyval.typeptr) = NULL; }
#line 1735 "palgram.tab.c"
    break;

  case 46: /* array_type: ARRAY LBRACK expression RBRACK OF type_spec  */
#line 183 "palgram.y"
          { /* Create array type */ (yyval.typeptr) = NULL; }
#line 1741 "palgram.tab.c"
    break;

  case 47: /* array_type: ARRAY LBRACK expression DOTDOT expression RBRACK OF type_spec  */
#line 185 "palgram.y"
          { /* Array with bounds */ (yyval.typeptr) = NULL; }
#line 1747 "palgram.tab.c"
    break;

  case 48: /* record_type: RECORD field_list END  */
#line 190 "palgram.y"
          { /* Create record type */ (yyval.typeptr) = NULL; }
#line 1753 "palgram.tab.c"
    break;

  case 49: /* field_list: field_decl  */
#line 195 "palgram.y"
          { (yyval.fieldptr) = (yyvsp[0].fieldptr); }
#line 1759 "palgram.tab.c"
    break;

  case 50: /* field_list: field_list field_decl  */
#line 197 "palgram.y"
          { /* Append field to list */ (yyval.fieldptr) = (yyvsp[-1].fieldptr); }
#line 1765 "palgram.tab.c"
    break;

  case 51: /* field_decl: identifier_list COLON type_spec SEMICOLON  */
#line 202 "palgram.y"
          { /* Field declaration */ (yyval.fieldptr) = NULL; }
#line 1771 "palgram.tab.c"
    break;

  case 52: /* proc_declaration: PROC identifier opt_param_list proc_body ENDPROC  */
#line 208 "palgram.y"
          { /* Define procedure */ }
#line 1777 "palgram.tab.c"
    break;

  case 58: /* method_declaration: METHOD identifier opt_param_list method_body ENDMETHOD  */
#line 225 "palgram.y"
          { if (!ALLOW_METHODS()) {
	      error("methods not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
#line 1786 "palgram.tab.c"
    break;

  case 59: /* method_declaration: visibility METHOD identifier opt_param_list method_body ENDMETHOD  */
#line 230 "palgram.y"
          { if (!ALLOW_METHODS()) {
	      error("methods not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
#line 1795 "palgram.tab.c"
    break;

  case 64: /* opt_param_list: %empty  */
#line 249 "palgram.y"
          { (yyval.paramptr) = NULL; }
#line 1801 "palgram.tab.c"
    break;

  case 65: /* opt_param_list: LPAREN RPAREN  */
#line 251 "palgram.y"
          { (yyval.paramptr) = NULL; }
#line 1807 "palgram.tab.c"
    break;

  case 66: /* opt_param_list: LPAREN param_list RPAREN  */
#line 253 "palgram.y"
          { (yyval.paramptr) = (yyvsp[-1].paramptr); }
#line 1813 "palgram.tab.c"
    break;

  case 67: /* param_list: param_decl  */
#line 258 "palgram.y"
          { (yyval.paramptr) = (yyvsp[0].paramptr); }
#line 1819 "palgram.tab.c"
    break;

  case 68: /* param_list: param_list COMMA param_decl  */
#line 260 "palgram.y"
          { /* Append parameter */ (yyval.paramptr) = (yyvsp[-2].paramptr); }
#line 1825 "palgram.tab.c"
    break;

  case 69: /* param_decl: identifier COLON type_spec  */
#line 265 "palgram.y"
          { /* Parameter declaration */ (yyval.paramptr) = NULL; }
#line 1831 "palgram.tab.c"
    break;

  case 70: /* param_decl: VAR identifier COLON type_spec  */
#line 267 "palgram.y"
          { /* var parameter (by reference) */ (yyval.paramptr) = NULL; }
#line 1837 "palgram.tab.c"
    break;

  case 71: /* param_decl: CONST identifier COLON type_spec  */
#line 269 "palgram.y"
          { /* const parameter */ (yyval.paramptr) = NULL; }
#line 1843 "palgram.tab.c"
    break;

  case 82: /* assignment_statement: lvalue ASSIGN expression  */
#line 294 "palgram.y"
          { /* Assignment */ }
#line 1849 "palgram.tab.c"
    break;

  case 83: /* assignment_statement: lvalue EQ expression  */
#line 296 "palgram.y"
          { /* Assignment (PAL style uses =) */ }
#line 1855 "palgram.tab.c"
    break;

  case 85: /* lvalue: lvalue DOT identifier  */
#line 302 "palgram.y"
          { /* Field/method access */ }
#line 1861 "palgram.tab.c"
    break;

  case 86: /* lvalue: lvalue LBRACK expression RBRACK  */
#line 304 "palgram.y"
          { /* Array subscript */ }
#line 1867 "palgram.tab.c"
    break;

  case 87: /* procedure_call: identifier opt_arguments  */
#line 309 "palgram.y"
          { /* Call procedure */ }
#line 1873 "palgram.tab.c"
    break;

  case 88: /* procedure_call: lvalue DOT identifier opt_arguments  */
#line 311 "palgram.y"
          { /* Call method */ }
#line 1879 "palgram.tab.c"
    break;

  case 94: /* return_statement: RETURN  */
#line 327 "palgram.y"
          { /* Return from procedure */ }
#line 1885 "palgram.tab.c"
    break;

  case 95: /* return_statement: RETURN expression  */
#line 329 "palgram.y"
          { /* Return value from function */ }
#line 1891 "palgram.tab.c"
    break;

  case 96: /* break_statement: BREAK  */
#line 334 "palgram.y"
          { if (!dialect_features->allow_break_continue) {
	      error("break not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
#line 1900 "palgram.tab.c"
    break;

  case 97: /* continue_statement: CONTINUE  */
#line 342 "palgram.y"
          { if (!dialect_features->allow_break_continue) {
	      error("continue not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
#line 1909 "palgram.tab.c"
    break;

  case 98: /* quit_statement: QUIT  */
#line 350 "palgram.y"
          { /* Quit program */ }
#line 1915 "palgram.tab.c"
    break;

  case 114: /* while_statement: WHILE expression DO statement_list ENDWHILE  */
#line 383 "palgram.y"
          { if (!dialect_features->allow_while_loop) {
	      error("while loop not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
#line 1924 "palgram.tab.c"
    break;

  case 115: /* for_statement: FOR identifier EQ expression TO expression for_body ENDFOR  */
#line 391 "palgram.y"
          { /* for loop ascending */ }
#line 1930 "palgram.tab.c"
    break;

  case 116: /* for_statement: FOR identifier EQ expression DOWNTO expression for_body ENDFOR  */
#line 393 "palgram.y"
          { /* for loop descending */ }
#line 1936 "palgram.tab.c"
    break;

  case 117: /* for_statement: FOR identifier EQ expression TO expression STEP expression for_body ENDFOR  */
#line 395 "palgram.y"
          { /* for loop with step */ }
#line 1942 "palgram.tab.c"
    break;

  case 119: /* foreach_statement: FOREACH identifier IN expression foreach_body ENDFOREACH  */
#line 404 "palgram.y"
          { if (!ALLOW_FOREACH()) {
	      error("foreach loop not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
#line 1951 "palgram.tab.c"
    break;

  case 121: /* switch_statement: SWITCH expression case_list endswitch_part  */
#line 416 "palgram.y"
          { if (!dialect_features->allow_switch_statement) {
	      error("switch statement not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
#line 1960 "palgram.tab.c"
    break;

  case 127: /* try_statement: TRY statement_list EXCEPT statement_list END  */
#line 438 "palgram.y"
          { if (!ALLOW_TRY_EXCEPT()) {
	      error("try-except not allowed in %s", get_dialect_name(current_dialect));
	    }
	  }
#line 1969 "palgram.tab.c"
    break;

  case 128: /* scan_statement: SCAN identifier COLON statement_list ENDSCAN  */
#line 446 "palgram.y"
          { /* Scan through table */ }
#line 1975 "palgram.tab.c"
    break;

  case 131: /* expression: expression EQ simple_expression  */
#line 457 "palgram.y"
          { /* Equality */ }
#line 1981 "palgram.tab.c"
    break;

  case 132: /* expression: expression NE simple_expression  */
#line 459 "palgram.y"
          { /* Inequality */ }
#line 1987 "palgram.tab.c"
    break;

  case 133: /* expression: expression LT simple_expression  */
#line 461 "palgram.y"
          { /* Less than */ }
#line 1993 "palgram.tab.c"
    break;

  case 134: /* expression: expression LE simple_expression  */
#line 463 "palgram.y"
          { /* Less or equal */ }
#line 1999 "palgram.tab.c"
    break;

  case 135: /* expression: expression GT simple_expression  */
#line 465 "palgram.y"
          { /* Greater than */ }
#line 2005 "palgram.tab.c"
    break;

  case 136: /* expression: expression GE simple_expression  */
#line 467 "palgram.y"
          { /* Greater or equal */ }
#line 2011 "palgram.tab.c"
    break;

  case 137: /* expression: expression IN simple_expression  */
#line 469 "palgram.y"
          { /* Set membership */ }
#line 2017 "palgram.tab.c"
    break;

  case 138: /* expression: expression LIKE simple_expression  */
#line 471 "palgram.y"
          { /* String pattern match */ }
#line 2023 "palgram.tab.c"
    break;

  case 140: /* simple_expression: PLUS term  */
#line 477 "palgram.y"
          { /* Unary plus */ }
#line 2029 "palgram.tab.c"
    break;

  case 141: /* simple_expression: MINUS term  */
#line 479 "palgram.y"
          { /* Unary minus */ }
#line 2035 "palgram.tab.c"
    break;

  case 142: /* simple_expression: simple_expression PLUS term  */
#line 481 "palgram.y"
          { /* Addition */ }
#line 2041 "palgram.tab.c"
    break;

  case 143: /* simple_expression: simple_expression MINUS term  */
#line 483 "palgram.y"
          { /* Subtraction */ }
#line 2047 "palgram.tab.c"
    break;

  case 144: /* simple_expression: simple_expression OR term  */
#line 485 "palgram.y"
          { /* Logical OR */ }
#line 2053 "palgram.tab.c"
    break;

  case 146: /* term: term STAR factor  */
#line 491 "palgram.y"
          { /* Multiplication */ }
#line 2059 "palgram.tab.c"
    break;

  case 147: /* term: term SLASH factor  */
#line 493 "palgram.y"
          { /* Division */ }
#line 2065 "palgram.tab.c"
    break;

  case 148: /* term: term INTDIV factor  */
#line 495 "palgram.y"
          { /* Integer division */ }
#line 2071 "palgram.tab.c"
    break;

  case 149: /* term: term MOD factor  */
#line 497 "palgram.y"
          { /* Modulo */ }
#line 2077 "palgram.tab.c"
    break;

  case 150: /* term: term AND factor  */
#line 499 "palgram.y"
          { /* Logical AND */ }
#line 2083 "palgram.tab.c"
    break;

  case 152: /* factor: NOT factor  */
#line 505 "palgram.y"
          { /* Logical NOT */ }
#line 2089 "palgram.tab.c"
    break;

  case 153: /* factor: UPARROW factor  */
#line 507 "palgram.y"
          { /* Pointer dereference */ }
#line 2095 "palgram.tab.c"
    break;

  case 154: /* factor: AT factor  */
#line 509 "palgram.y"
          { /* Address-of */ }
#line 2101 "palgram.tab.c"
    break;

  case 158: /* primary: LPAREN expression RPAREN  */
#line 517 "palgram.y"
          { /* Parenthesized expression */ }
#line 2107 "palgram.tab.c"
    break;

  case 159: /* primary: SELF  */
#line 519 "palgram.y"
          { /* Self reference (ObjectPAL) */ }
#line 2113 "palgram.tab.c"
    break;

  case 160: /* function_call: identifier LPAREN argument_list RPAREN  */
#line 524 "palgram.y"
          { /* Function call */ }
#line 2119 "palgram.tab.c"
    break;

  case 161: /* function_call: identifier LPAREN RPAREN  */
#line 526 "palgram.y"
          { /* Function call with no args */ }
#line 2125 "palgram.tab.c"
    break;

  case 162: /* constant: INTCONST  */
#line 531 "palgram.y"
          { /* Integer constant */ }
#line 2131 "palgram.tab.c"
    break;

  case 163: /* constant: NUMBERCONST  */
#line 533 "palgram.y"
          { /* Floating point constant */ }
#line 2137 "palgram.tab.c"
    break;

  case 164: /* constant: STRINGCONST  */
#line 535 "palgram.y"
          { /* String constant */ }
#line 2143 "palgram.tab.c"
    break;

  case 165: /* constant: LOGICALCONST  */
#line 537 "palgram.y"
          { /* Boolean constant */ }
#line 2149 "palgram.tab.c"
    break;

  case 166: /* identifier: IDENT  */
#line 542 "palgram.y"
          { /* Identifier */ (yyval.symptr) = NULL; }
#line 2155 "palgram.tab.c"
    break;


#line 2159 "palgram.tab.c"

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

#line 545 "palgram.y"


void yyerror(const char *s)
{
	error("%s", s);
}
