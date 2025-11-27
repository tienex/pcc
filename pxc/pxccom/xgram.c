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
#line 8 "./xgram.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Forward declarations */
int yylex(void);
void yyerror(const char *s);

/* Global state */
int current_scope = 0;


#line 86 "y.tab.c"

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

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    FUNCTION = 258,                /* FUNCTION  */
    PROCEDURE = 259,               /* PROCEDURE  */
    RETURN = 260,                  /* RETURN  */
    PARAMETERS = 261,              /* PARAMETERS  */
    LOCAL = 262,                   /* LOCAL  */
    STATIC = 263,                  /* STATIC  */
    PUBLIC = 264,                  /* PUBLIC  */
    PRIVATE = 265,                 /* PRIVATE  */
    MEMVAR = 266,                  /* MEMVAR  */
    FIELD = 267,                   /* FIELD  */
    IF = 268,                      /* IF  */
    ELSE = 269,                    /* ELSE  */
    ELSEIF = 270,                  /* ELSEIF  */
    ENDIF = 271,                   /* ENDIF  */
    IIF = 272,                     /* IIF  */
    DO = 273,                      /* DO  */
    WHILE = 274,                   /* WHILE  */
    ENDDO = 275,                   /* ENDDO  */
    EXIT = 276,                    /* EXIT  */
    LOOP = 277,                    /* LOOP  */
    FOR = 278,                     /* FOR  */
    TO = 279,                      /* TO  */
    STEP = 280,                    /* STEP  */
    NEXT = 281,                    /* NEXT  */
    CASE = 282,                    /* CASE  */
    OTHERWISE = 283,               /* OTHERWISE  */
    ENDCASE = 284,                 /* ENDCASE  */
    BBEGIN = 285,                  /* BBEGIN  */
    SEQUENCE = 286,                /* SEQUENCE  */
    END = 287,                     /* END  */
    RECOVER = 288,                 /* RECOVER  */
    BREAK = 289,                   /* BREAK  */
    CLASS = 290,                   /* CLASS  */
    ENDCLASS = 291,                /* ENDCLASS  */
    METHOD = 292,                  /* METHOD  */
    ENDMETHOD = 293,               /* ENDMETHOD  */
    DATA = 294,                    /* DATA  */
    VAR = 295,                     /* VAR  */
    INLINE = 296,                  /* INLINE  */
    VIRTUAL = 297,                 /* VIRTUAL  */
    CONSTRUCTOR = 298,             /* CONSTRUCTOR  */
    DESTRUCTOR = 299,              /* DESTRUCTOR  */
    INHERIT = 300,                 /* INHERIT  */
    FROM = 301,                    /* FROM  */
    EXPORT = 302,                  /* EXPORT  */
    PROTECTED = 303,               /* PROTECTED  */
    HIDDEN = 304,                  /* HIDDEN  */
    READONLY = 305,                /* READONLY  */
    SHARED = 306,                  /* SHARED  */
    SYNC = 307,                    /* SYNC  */
    USE = 308,                     /* USE  */
    SELECT = 309,                  /* SELECT  */
    GO = 310,                      /* GO  */
    GOTO = 311,                    /* GOTO  */
    SKIP = 312,                    /* SKIP  */
    SEEK = 313,                    /* SEEK  */
    LOCATE = 314,                  /* LOCATE  */
    CONTINUE = 315,                /* CONTINUE  */
    REPLACE = 316,                 /* REPLACE  */
    DELETE = 317,                  /* DELETE  */
    RECALL = 318,                  /* RECALL  */
    PACK = 319,                    /* PACK  */
    ZAP = 320,                     /* ZAP  */
    APPEND = 321,                  /* APPEND  */
    BLANK = 322,                   /* BLANK  */
    INDEX = 323,                   /* INDEX  */
    REINDEX = 324,                 /* REINDEX  */
    SET = 325,                     /* SET  */
    CLOSE = 326,                   /* CLOSE  */
    COMMIT = 327,                  /* COMMIT  */
    UNLOCK = 328,                  /* UNLOCK  */
    ALIAS = 329,                   /* ALIAS  */
    IN = 330,                      /* IN  */
    EXCLUSIVE = 331,               /* EXCLUSIVE  */
    NEW = 332,                     /* NEW  */
    ADDITIVE = 333,                /* ADDITIVE  */
    WITH = 334,                    /* WITH  */
    REQUEST = 335,                 /* REQUEST  */
    EXTERNAL = 336,                /* EXTERNAL  */
    INIT = 337,                    /* INIT  */
    ANNOUNCE = 338,                /* ANNOUNCE  */
    AS = 339,                      /* AS  */
    IS = 340,                      /* IS  */
    ASSIGN = 341,                  /* ASSIGN  */
    PLUSASSIGN = 342,              /* PLUSASSIGN  */
    MINUSASSIGN = 343,             /* MINUSASSIGN  */
    MULASSIGN = 344,               /* MULASSIGN  */
    DIVASSIGN = 345,               /* DIVASSIGN  */
    MODASSIGN = 346,               /* MODASSIGN  */
    EXPASSIGN = 347,               /* EXPASSIGN  */
    EQ = 348,                      /* EQ  */
    NE = 349,                      /* NE  */
    LT = 350,                      /* LT  */
    LE = 351,                      /* LE  */
    GT = 352,                      /* GT  */
    GE = 353,                      /* GE  */
    SUBSTR = 354,                  /* SUBSTR  */
    INCR = 355,                    /* INCR  */
    DECR = 356,                    /* DECR  */
    POWER = 357,                   /* POWER  */
    AND = 358,                     /* AND  */
    OR = 359,                      /* OR  */
    NOT = 360,                     /* NOT  */
    ARROW = 361,                   /* ARROW  */
    DCOLON = 362,                  /* DCOLON  */
    INTEGER = 363,                 /* INTEGER  */
    FLOAT = 364,                   /* FLOAT  */
    STRING = 365,                  /* STRING  */
    DATE = 366,                    /* DATE  */
    IDENT = 367,                   /* IDENT  */
    MACROVAR = 368,                /* MACROVAR  */
    LTRUE = 369,                   /* LTRUE  */
    LFALSE = 370,                  /* LFALSE  */
    NIL = 371,                     /* NIL  */
    EOL = 372,                     /* EOL  */
    UMINUS = 373                   /* UMINUS  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define FUNCTION 258
#define PROCEDURE 259
#define RETURN 260
#define PARAMETERS 261
#define LOCAL 262
#define STATIC 263
#define PUBLIC 264
#define PRIVATE 265
#define MEMVAR 266
#define FIELD 267
#define IF 268
#define ELSE 269
#define ELSEIF 270
#define ENDIF 271
#define IIF 272
#define DO 273
#define WHILE 274
#define ENDDO 275
#define EXIT 276
#define LOOP 277
#define FOR 278
#define TO 279
#define STEP 280
#define NEXT 281
#define CASE 282
#define OTHERWISE 283
#define ENDCASE 284
#define BBEGIN 285
#define SEQUENCE 286
#define END 287
#define RECOVER 288
#define BREAK 289
#define CLASS 290
#define ENDCLASS 291
#define METHOD 292
#define ENDMETHOD 293
#define DATA 294
#define VAR 295
#define INLINE 296
#define VIRTUAL 297
#define CONSTRUCTOR 298
#define DESTRUCTOR 299
#define INHERIT 300
#define FROM 301
#define EXPORT 302
#define PROTECTED 303
#define HIDDEN 304
#define READONLY 305
#define SHARED 306
#define SYNC 307
#define USE 308
#define SELECT 309
#define GO 310
#define GOTO 311
#define SKIP 312
#define SEEK 313
#define LOCATE 314
#define CONTINUE 315
#define REPLACE 316
#define DELETE 317
#define RECALL 318
#define PACK 319
#define ZAP 320
#define APPEND 321
#define BLANK 322
#define INDEX 323
#define REINDEX 324
#define SET 325
#define CLOSE 326
#define COMMIT 327
#define UNLOCK 328
#define ALIAS 329
#define IN 330
#define EXCLUSIVE 331
#define NEW 332
#define ADDITIVE 333
#define WITH 334
#define REQUEST 335
#define EXTERNAL 336
#define INIT 337
#define ANNOUNCE 338
#define AS 339
#define IS 340
#define ASSIGN 341
#define PLUSASSIGN 342
#define MINUSASSIGN 343
#define MULASSIGN 344
#define DIVASSIGN 345
#define MODASSIGN 346
#define EXPASSIGN 347
#define EQ 348
#define NE 349
#define LT 350
#define LE 351
#define GT 352
#define GE 353
#define SUBSTR 354
#define INCR 355
#define DECR 356
#define POWER 357
#define AND 358
#define OR 359
#define NOT 360
#define ARROW 361
#define DCOLON 362
#define INTEGER 363
#define FLOAT 364
#define STRING 365
#define DATE 366
#define IDENT 367
#define MACROVAR 368
#define LTRUE 369
#define LFALSE 370
#define NIL 371
#define EOL 372
#define UMINUS 373

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 23 "./xgram.y"

	long long ival;
	double fval;
	char *sval;
	struct symtab *symptr;
	struct tnode *typeptr;
	struct node *nodeptr;

#line 384 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_FUNCTION = 3,                   /* FUNCTION  */
  YYSYMBOL_PROCEDURE = 4,                  /* PROCEDURE  */
  YYSYMBOL_RETURN = 5,                     /* RETURN  */
  YYSYMBOL_PARAMETERS = 6,                 /* PARAMETERS  */
  YYSYMBOL_LOCAL = 7,                      /* LOCAL  */
  YYSYMBOL_STATIC = 8,                     /* STATIC  */
  YYSYMBOL_PUBLIC = 9,                     /* PUBLIC  */
  YYSYMBOL_PRIVATE = 10,                   /* PRIVATE  */
  YYSYMBOL_MEMVAR = 11,                    /* MEMVAR  */
  YYSYMBOL_FIELD = 12,                     /* FIELD  */
  YYSYMBOL_IF = 13,                        /* IF  */
  YYSYMBOL_ELSE = 14,                      /* ELSE  */
  YYSYMBOL_ELSEIF = 15,                    /* ELSEIF  */
  YYSYMBOL_ENDIF = 16,                     /* ENDIF  */
  YYSYMBOL_IIF = 17,                       /* IIF  */
  YYSYMBOL_DO = 18,                        /* DO  */
  YYSYMBOL_WHILE = 19,                     /* WHILE  */
  YYSYMBOL_ENDDO = 20,                     /* ENDDO  */
  YYSYMBOL_EXIT = 21,                      /* EXIT  */
  YYSYMBOL_LOOP = 22,                      /* LOOP  */
  YYSYMBOL_FOR = 23,                       /* FOR  */
  YYSYMBOL_TO = 24,                        /* TO  */
  YYSYMBOL_STEP = 25,                      /* STEP  */
  YYSYMBOL_NEXT = 26,                      /* NEXT  */
  YYSYMBOL_CASE = 27,                      /* CASE  */
  YYSYMBOL_OTHERWISE = 28,                 /* OTHERWISE  */
  YYSYMBOL_ENDCASE = 29,                   /* ENDCASE  */
  YYSYMBOL_BBEGIN = 30,                    /* BBEGIN  */
  YYSYMBOL_SEQUENCE = 31,                  /* SEQUENCE  */
  YYSYMBOL_END = 32,                       /* END  */
  YYSYMBOL_RECOVER = 33,                   /* RECOVER  */
  YYSYMBOL_BREAK = 34,                     /* BREAK  */
  YYSYMBOL_CLASS = 35,                     /* CLASS  */
  YYSYMBOL_ENDCLASS = 36,                  /* ENDCLASS  */
  YYSYMBOL_METHOD = 37,                    /* METHOD  */
  YYSYMBOL_ENDMETHOD = 38,                 /* ENDMETHOD  */
  YYSYMBOL_DATA = 39,                      /* DATA  */
  YYSYMBOL_VAR = 40,                       /* VAR  */
  YYSYMBOL_INLINE = 41,                    /* INLINE  */
  YYSYMBOL_VIRTUAL = 42,                   /* VIRTUAL  */
  YYSYMBOL_CONSTRUCTOR = 43,               /* CONSTRUCTOR  */
  YYSYMBOL_DESTRUCTOR = 44,                /* DESTRUCTOR  */
  YYSYMBOL_INHERIT = 45,                   /* INHERIT  */
  YYSYMBOL_FROM = 46,                      /* FROM  */
  YYSYMBOL_EXPORT = 47,                    /* EXPORT  */
  YYSYMBOL_PROTECTED = 48,                 /* PROTECTED  */
  YYSYMBOL_HIDDEN = 49,                    /* HIDDEN  */
  YYSYMBOL_READONLY = 50,                  /* READONLY  */
  YYSYMBOL_SHARED = 51,                    /* SHARED  */
  YYSYMBOL_SYNC = 52,                      /* SYNC  */
  YYSYMBOL_USE = 53,                       /* USE  */
  YYSYMBOL_SELECT = 54,                    /* SELECT  */
  YYSYMBOL_GO = 55,                        /* GO  */
  YYSYMBOL_GOTO = 56,                      /* GOTO  */
  YYSYMBOL_SKIP = 57,                      /* SKIP  */
  YYSYMBOL_SEEK = 58,                      /* SEEK  */
  YYSYMBOL_LOCATE = 59,                    /* LOCATE  */
  YYSYMBOL_CONTINUE = 60,                  /* CONTINUE  */
  YYSYMBOL_REPLACE = 61,                   /* REPLACE  */
  YYSYMBOL_DELETE = 62,                    /* DELETE  */
  YYSYMBOL_RECALL = 63,                    /* RECALL  */
  YYSYMBOL_PACK = 64,                      /* PACK  */
  YYSYMBOL_ZAP = 65,                       /* ZAP  */
  YYSYMBOL_APPEND = 66,                    /* APPEND  */
  YYSYMBOL_BLANK = 67,                     /* BLANK  */
  YYSYMBOL_INDEX = 68,                     /* INDEX  */
  YYSYMBOL_REINDEX = 69,                   /* REINDEX  */
  YYSYMBOL_SET = 70,                       /* SET  */
  YYSYMBOL_CLOSE = 71,                     /* CLOSE  */
  YYSYMBOL_COMMIT = 72,                    /* COMMIT  */
  YYSYMBOL_UNLOCK = 73,                    /* UNLOCK  */
  YYSYMBOL_ALIAS = 74,                     /* ALIAS  */
  YYSYMBOL_IN = 75,                        /* IN  */
  YYSYMBOL_EXCLUSIVE = 76,                 /* EXCLUSIVE  */
  YYSYMBOL_NEW = 77,                       /* NEW  */
  YYSYMBOL_ADDITIVE = 78,                  /* ADDITIVE  */
  YYSYMBOL_WITH = 79,                      /* WITH  */
  YYSYMBOL_REQUEST = 80,                   /* REQUEST  */
  YYSYMBOL_EXTERNAL = 81,                  /* EXTERNAL  */
  YYSYMBOL_INIT = 82,                      /* INIT  */
  YYSYMBOL_ANNOUNCE = 83,                  /* ANNOUNCE  */
  YYSYMBOL_AS = 84,                        /* AS  */
  YYSYMBOL_IS = 85,                        /* IS  */
  YYSYMBOL_ASSIGN = 86,                    /* ASSIGN  */
  YYSYMBOL_PLUSASSIGN = 87,                /* PLUSASSIGN  */
  YYSYMBOL_MINUSASSIGN = 88,               /* MINUSASSIGN  */
  YYSYMBOL_MULASSIGN = 89,                 /* MULASSIGN  */
  YYSYMBOL_DIVASSIGN = 90,                 /* DIVASSIGN  */
  YYSYMBOL_MODASSIGN = 91,                 /* MODASSIGN  */
  YYSYMBOL_EXPASSIGN = 92,                 /* EXPASSIGN  */
  YYSYMBOL_EQ = 93,                        /* EQ  */
  YYSYMBOL_NE = 94,                        /* NE  */
  YYSYMBOL_LT = 95,                        /* LT  */
  YYSYMBOL_LE = 96,                        /* LE  */
  YYSYMBOL_GT = 97,                        /* GT  */
  YYSYMBOL_GE = 98,                        /* GE  */
  YYSYMBOL_SUBSTR = 99,                    /* SUBSTR  */
  YYSYMBOL_INCR = 100,                     /* INCR  */
  YYSYMBOL_DECR = 101,                     /* DECR  */
  YYSYMBOL_POWER = 102,                    /* POWER  */
  YYSYMBOL_AND = 103,                      /* AND  */
  YYSYMBOL_OR = 104,                       /* OR  */
  YYSYMBOL_NOT = 105,                      /* NOT  */
  YYSYMBOL_ARROW = 106,                    /* ARROW  */
  YYSYMBOL_DCOLON = 107,                   /* DCOLON  */
  YYSYMBOL_INTEGER = 108,                  /* INTEGER  */
  YYSYMBOL_FLOAT = 109,                    /* FLOAT  */
  YYSYMBOL_STRING = 110,                   /* STRING  */
  YYSYMBOL_DATE = 111,                     /* DATE  */
  YYSYMBOL_IDENT = 112,                    /* IDENT  */
  YYSYMBOL_MACROVAR = 113,                 /* MACROVAR  */
  YYSYMBOL_LTRUE = 114,                    /* LTRUE  */
  YYSYMBOL_LFALSE = 115,                   /* LFALSE  */
  YYSYMBOL_NIL = 116,                      /* NIL  */
  YYSYMBOL_EOL = 117,                      /* EOL  */
  YYSYMBOL_118_ = 118,                     /* '+'  */
  YYSYMBOL_119_ = 119,                     /* '-'  */
  YYSYMBOL_120_ = 120,                     /* '*'  */
  YYSYMBOL_121_ = 121,                     /* '/'  */
  YYSYMBOL_122_ = 122,                     /* '%'  */
  YYSYMBOL_UMINUS = 123,                   /* UMINUS  */
  YYSYMBOL_124_ = 124,                     /* '.'  */
  YYSYMBOL_125_ = 125,                     /* '['  */
  YYSYMBOL_126_ = 126,                     /* '('  */
  YYSYMBOL_127_ = 127,                     /* ')'  */
  YYSYMBOL_128_ = 128,                     /* ','  */
  YYSYMBOL_129_ = 129,                     /* ';'  */
  YYSYMBOL_130_ = 130,                     /* ']'  */
  YYSYMBOL_131_ = 131,                     /* '{'  */
  YYSYMBOL_132_ = 132,                     /* '}'  */
  YYSYMBOL_133_ = 133,                     /* '|'  */
  YYSYMBOL_134_ = 134,                     /* '@'  */
  YYSYMBOL_135_ = 135,                     /* '&'  */
  YYSYMBOL_YYACCEPT = 136,                 /* $accept  */
  YYSYMBOL_program = 137,                  /* program  */
  YYSYMBOL_definition_list = 138,          /* definition_list  */
  YYSYMBOL_definition = 139,               /* definition  */
  YYSYMBOL_function_def = 140,             /* function_def  */
  YYSYMBOL_141_1 = 141,                    /* @1  */
  YYSYMBOL_procedure_def = 142,            /* procedure_def  */
  YYSYMBOL_143_2 = 143,                    /* @2  */
  YYSYMBOL_class_def = 144,                /* class_def  */
  YYSYMBOL_145_3 = 145,                    /* $@3  */
  YYSYMBOL_opt_from_clause = 146,          /* opt_from_clause  */
  YYSYMBOL_opt_class_members = 147,        /* opt_class_members  */
  YYSYMBOL_class_member_list = 148,        /* class_member_list  */
  YYSYMBOL_class_member = 149,             /* class_member  */
  YYSYMBOL_data_member = 150,              /* data_member  */
  YYSYMBOL_method_def = 151,               /* method_def  */
  YYSYMBOL_152_4 = 152,                    /* $@4  */
  YYSYMBOL_opt_param_list = 153,           /* opt_param_list  */
  YYSYMBOL_param_list = 154,               /* param_list  */
  YYSYMBOL_variable_decl = 155,            /* variable_decl  */
  YYSYMBOL_var_list = 156,                 /* var_list  */
  YYSYMBOL_var_item = 157,                 /* var_item  */
  YYSYMBOL_field_list = 158,               /* field_list  */
  YYSYMBOL_opt_statement_list = 159,       /* opt_statement_list  */
  YYSYMBOL_statement_list = 160,           /* statement_list  */
  YYSYMBOL_statement = 161,                /* statement  */
  YYSYMBOL_expr_stmt = 162,                /* expr_stmt  */
  YYSYMBOL_if_stmt = 163,                  /* if_stmt  */
  YYSYMBOL_opt_elseif_list = 164,          /* opt_elseif_list  */
  YYSYMBOL_elseif_list = 165,              /* elseif_list  */
  YYSYMBOL_elseif_clause = 166,            /* elseif_clause  */
  YYSYMBOL_opt_else_clause = 167,          /* opt_else_clause  */
  YYSYMBOL_while_stmt = 168,               /* while_stmt  */
  YYSYMBOL_for_stmt = 169,                 /* for_stmt  */
  YYSYMBOL_opt_step = 170,                 /* opt_step  */
  YYSYMBOL_case_stmt = 171,                /* case_stmt  */
  YYSYMBOL_case_list = 172,                /* case_list  */
  YYSYMBOL_case_clause = 173,              /* case_clause  */
  YYSYMBOL_opt_otherwise = 174,            /* opt_otherwise  */
  YYSYMBOL_return_stmt = 175,              /* return_stmt  */
  YYSYMBOL_opt_return_stmt = 176,          /* opt_return_stmt  */
  YYSYMBOL_break_stmt = 177,               /* break_stmt  */
  YYSYMBOL_exit_stmt = 178,                /* exit_stmt  */
  YYSYMBOL_loop_stmt = 179,                /* loop_stmt  */
  YYSYMBOL_db_command = 180,               /* db_command  */
  YYSYMBOL_field_assign_list = 181,        /* field_assign_list  */
  YYSYMBOL_field_assign = 182,             /* field_assign  */
  YYSYMBOL_opt_expression = 183,           /* opt_expression  */
  YYSYMBOL_expression = 184,               /* expression  */
  YYSYMBOL_binary_expr = 185,              /* binary_expr  */
  YYSYMBOL_unary_expr = 186,               /* unary_expr  */
  YYSYMBOL_postfix_expr = 187,             /* postfix_expr  */
  YYSYMBOL_primary_expr = 188,             /* primary_expr  */
  YYSYMBOL_opt_expr_list = 189,            /* opt_expr_list  */
  YYSYMBOL_expr_list = 190,                /* expr_list  */
  YYSYMBOL_identifier = 191,               /* identifier  */
  YYSYMBOL_opt_eol = 192                   /* opt_eol  */
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
#define YYFINAL  31
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1310

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  136
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  57
/* YYNRULES -- Number of rules.  */
#define YYNRULES  165
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  327

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   373


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
       2,     2,     2,     2,     2,     2,     2,   122,   135,     2,
     126,   127,   120,   118,   128,   119,   124,   121,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,   129,
       2,     2,     2,     2,   134,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   125,     2,   130,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   131,   133,   132,     2,     2,     2,     2,
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
     115,   116,   117,   123
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   104,   104,   106,   110,   111,   115,   116,   117,   118,
     119,   125,   124,   157,   156,   188,   187,   198,   200,   201,
     204,   206,   210,   211,   215,   216,   217,   221,   222,   223,
     224,   225,   230,   229,   238,   242,   244,   245,   249,   250,
     255,   256,   257,   258,   259,   260,   264,   265,   269,   270,
     274,   275,   281,   282,   286,   287,   292,   294,   296,   298,
     300,   302,   304,   306,   308,   310,   312,   314,   319,   320,
     325,   333,   335,   339,   340,   344,   348,   350,   356,   364,
     370,   372,   377,   385,   386,   390,   394,   396,   402,   408,
     409,   414,   418,   422,   427,   428,   429,   430,   431,   432,
     433,   434,   435,   436,   437,   441,   442,   446,   452,   453,
     457,   458,   459,   460,   464,   466,   468,   470,   472,   474,
     476,   478,   480,   482,   484,   486,   488,   490,   492,   494,
     496,   498,   503,   505,   507,   509,   511,   516,   518,   520,
     522,   524,   526,   528,   533,   535,   537,   539,   541,   543,
     545,   547,   549,   551,   553,   555,   557,   559,   565,   566,
     570,   571,   577,   588,   590,   591
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
  "\"end of file\"", "error", "\"invalid token\"", "FUNCTION",
  "PROCEDURE", "RETURN", "PARAMETERS", "LOCAL", "STATIC", "PUBLIC",
  "PRIVATE", "MEMVAR", "FIELD", "IF", "ELSE", "ELSEIF", "ENDIF", "IIF",
  "DO", "WHILE", "ENDDO", "EXIT", "LOOP", "FOR", "TO", "STEP", "NEXT",
  "CASE", "OTHERWISE", "ENDCASE", "BBEGIN", "SEQUENCE", "END", "RECOVER",
  "BREAK", "CLASS", "ENDCLASS", "METHOD", "ENDMETHOD", "DATA", "VAR",
  "INLINE", "VIRTUAL", "CONSTRUCTOR", "DESTRUCTOR", "INHERIT", "FROM",
  "EXPORT", "PROTECTED", "HIDDEN", "READONLY", "SHARED", "SYNC", "USE",
  "SELECT", "GO", "GOTO", "SKIP", "SEEK", "LOCATE", "CONTINUE", "REPLACE",
  "DELETE", "RECALL", "PACK", "ZAP", "APPEND", "BLANK", "INDEX", "REINDEX",
  "SET", "CLOSE", "COMMIT", "UNLOCK", "ALIAS", "IN", "EXCLUSIVE", "NEW",
  "ADDITIVE", "WITH", "REQUEST", "EXTERNAL", "INIT", "ANNOUNCE", "AS",
  "IS", "ASSIGN", "PLUSASSIGN", "MINUSASSIGN", "MULASSIGN", "DIVASSIGN",
  "MODASSIGN", "EXPASSIGN", "EQ", "NE", "LT", "LE", "GT", "GE", "SUBSTR",
  "INCR", "DECR", "POWER", "AND", "OR", "NOT", "ARROW", "DCOLON",
  "INTEGER", "FLOAT", "STRING", "DATE", "IDENT", "MACROVAR", "LTRUE",
  "LFALSE", "NIL", "EOL", "'+'", "'-'", "'*'", "'/'", "'%'", "UMINUS",
  "'.'", "'['", "'('", "')'", "','", "';'", "']'", "'{'", "'}'", "'|'",
  "'@'", "'&'", "$accept", "program", "definition_list", "definition",
  "function_def", "@1", "procedure_def", "@2", "class_def", "$@3",
  "opt_from_clause", "opt_class_members", "class_member_list",
  "class_member", "data_member", "method_def", "$@4", "opt_param_list",
  "param_list", "variable_decl", "var_list", "var_item", "field_list",
  "opt_statement_list", "statement_list", "statement", "expr_stmt",
  "if_stmt", "opt_elseif_list", "elseif_list", "elseif_clause",
  "opt_else_clause", "while_stmt", "for_stmt", "opt_step", "case_stmt",
  "case_list", "case_clause", "opt_otherwise", "return_stmt",
  "opt_return_stmt", "break_stmt", "exit_stmt", "loop_stmt", "db_command",
  "field_assign_list", "field_assign", "opt_expression", "expression",
  "binary_expr", "unary_expr", "postfix_expr", "primary_expr",
  "opt_expr_list", "expr_list", "identifier", "opt_eol", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-154)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-54)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
       1,   -98,   -98,   -98,   -98,   -98,   -98,   -98,   -98,   -98,
    -154,    16,     1,  -154,  -154,  -154,  -154,   -99,  -154,  -106,
    -106,   -87,  -154,   -29,   -87,   -87,   -87,   -87,   -56,  -154,
      29,  -154,  -154,  -154,   -95,   -48,   -48,   -98,   448,   -98,
     -98,   -98,   -48,  -154,   -51,  -154,  -154,   -35,   -35,  -154,
     -43,   448,   448,   448,  -154,  -154,  -154,  -154,  -154,  -154,
    -154,   448,   448,   448,   267,   448,   -28,  1084,  -154,  -154,
    -154,  -154,  -154,  -154,  -154,  -154,   -35,  -154,   -98,  -154,
     480,   480,   448,   -22,   -22,     8,     8,     8,   959,  -106,
    1084,   -47,   -42,  1084,  -154,   448,   448,   448,   448,   448,
     448,   448,   448,   448,   448,  -154,  -154,   448,   448,   448,
     -98,   -98,   448,   448,   448,   448,   448,   -98,   448,   448,
       3,  -154,   448,   448,    34,   -48,   -48,   -98,   -48,   448,
     448,   448,   448,   448,    64,   -98,   -48,   -48,    21,   -48,
    -154,  -154,    86,    19,  -154,  -154,  -154,  -154,  -154,  -154,
    -154,  -154,  -154,  -154,  -154,   829,    86,   873,  -154,   -40,
    -154,   448,  1084,  1084,  1084,  1184,  1184,   197,   197,   197,
     197,   197,     8,  1152,  1118,  -154,  -154,   326,   326,     8,
       8,     8,  -154,   788,   -33,   -98,   -98,   -98,    59,    56,
      60,    63,  -154,    62,     3,  -154,  -154,  -154,   -48,  1084,
    1043,   448,   -48,   -35,   -35,    18,   -35,   -48,  1043,  1043,
     -48,  1043,   448,   -79,  -154,    26,   -35,   -35,   -48,   -35,
    -154,  -154,   611,  -154,   -35,  -154,   448,   448,  1084,  -154,
    -154,  -106,   -48,   -48,   -98,   -98,   -98,   -98,   -48,  -154,
     -35,   611,  1043,   -20,   448,   -35,   -35,   -35,   -35,   -35,
    1043,   -98,   -35,   448,   -35,  -154,  -154,   916,   747,   -48,
     -35,   -35,  -106,   -48,   -48,   -48,   -35,    91,   611,   448,
      53,  -154,   661,   -35,  -154,  1084,   448,  -154,   -35,   -48,
     -35,   -35,   -35,   448,    97,    91,  -154,    92,  1043,   -48,
    -154,    84,   448,  1001,   480,   303,  1043,   -48,   100,  -154,
     -48,   611,   611,   -48,   706,  -154,    86,  1043,   611,   611,
     -48,   -35,  -154,  -154,   -35,   448,   -48,  -154,   -35,  -154,
    -154,   -35,  1084,   611,   104,   -48,   -35
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      10,     0,     3,     4,     6,     7,     8,     0,   162,    35,
      35,    40,    46,    48,    41,    42,    43,    44,    45,    50,
      17,     1,     5,     9,     0,   163,   163,     0,     0,     0,
       0,     0,   163,    36,     0,    38,   164,    11,    13,    47,
       0,     0,     0,     0,   144,   145,   146,   147,   148,   149,
     150,     0,     0,     0,   158,     0,     0,    49,   110,   111,
     112,   113,   151,    51,    19,    18,    15,    37,     0,   165,
      52,    52,     0,   135,   136,   132,   134,   133,     0,    35,
     160,     0,   159,   156,   157,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   142,   143,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   158,
      20,    39,   108,     0,     0,   163,   163,     0,   163,   108,
       0,     0,   108,     0,     0,     0,   163,   163,     0,   163,
      67,    65,    89,   163,    54,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    66,   163,    89,     0,   152,     0,
     153,     0,   129,   130,   131,   120,   121,   122,   123,   124,
     125,   128,   119,   126,   127,   139,   140,   114,   115,   116,
     117,   118,   138,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    26,     0,    21,    22,    24,    25,   163,   109,
     163,     0,   163,    92,    93,     0,    91,   163,   163,   163,
     163,   163,     0,   163,   105,     0,   101,   102,   163,   104,
      90,    12,     0,    69,    68,    14,     0,     0,   161,   137,
     141,    35,   163,   163,     0,     0,     0,     0,   163,    23,
      88,    52,   163,     0,     0,    94,    95,    96,    97,    98,
     163,     0,   100,     0,   103,    67,    55,     0,     0,   163,
      27,    28,    35,   163,   163,   163,    16,    71,    52,     0,
      86,    83,     0,    99,   106,   107,     0,   154,    32,   163,
      29,    30,    31,     0,    76,    72,    73,     0,   163,   163,
      84,     0,     0,     0,    52,     0,   163,   163,     0,    74,
     163,    52,    52,   163,    80,   155,    89,   163,    52,    52,
     163,    78,    85,    87,    82,     0,   163,    33,    34,    75,
      77,    70,    81,    52,     0,   163,    79
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -154,  -154,  -154,   105,  -154,  -154,  -154,  -154,  -154,  -154,
    -154,  -154,  -154,   -63,  -154,  -154,  -154,   -18,  -154,    25,
      58,    98,  -154,   -60,  -154,   -85,  -154,  -154,  -154,  -154,
    -147,  -154,  -154,  -154,  -154,  -154,  -154,  -131,  -154,  -141,
    -153,  -154,  -154,  -154,  -154,  -154,  -111,   -59,   230,  -154,
    -154,  -154,  -154,    22,  -154,   120,   -36
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    11,    12,    13,    14,    80,    15,    81,    16,   120,
      42,   193,   194,   195,   196,   197,   294,    35,    44,   141,
      21,    22,    28,   142,   143,   144,   145,   146,   284,   285,
     286,   298,   147,   148,   316,   149,   270,   271,   291,   150,
     221,   151,   152,   153,   154,   213,   214,   198,   155,    68,
      69,    70,    71,    91,    92,    72,    47
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      48,   220,    36,   225,     1,     2,    76,   269,     3,     4,
       5,     6,     7,     8,    18,   220,    31,    18,    33,   -53,
      34,   156,   -53,   -53,   -53,    17,   -53,   -53,   -53,   -53,
     -53,   -53,    43,   -53,   -53,   -53,     9,    17,    46,   -53,
     185,    37,   186,   187,   188,   -53,   -53,   -53,   -53,   251,
     189,   190,   191,   201,   -53,   -53,   -53,    38,   -53,   -53,
     -53,   202,    24,    25,    26,    27,   -53,   -53,   -53,    46,
     207,   159,    39,   210,    40,    41,    77,    78,   105,   106,
     269,   289,    79,    82,    94,   160,   161,   212,   218,   203,
     204,   122,   206,   227,   230,   235,   234,    79,   238,   236,
     216,   217,   237,   219,   244,   253,   283,   222,   105,   106,
     107,   297,   300,   303,   110,   111,   310,    32,    10,   224,
     192,    19,    20,    23,    23,    23,    23,    23,    29,    30,
     325,   239,   117,   118,   119,    49,    46,   256,   299,   290,
     274,   184,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   317,    45,     0,     0,    23,     0,    73,
      74,    75,   240,     0,   241,   220,   243,     0,     0,     0,
       0,   245,   246,   247,   248,   249,     0,   252,     0,     0,
       0,   267,   254,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   260,   261,   121,     0,
       0,     0,   266,     0,     0,     0,   268,     0,   287,     0,
       0,     0,     0,   259,   273,     0,     0,     0,     0,     0,
       0,     0,     0,   278,     0,     0,     0,   280,   281,   282,
     175,   176,     0,     0,   306,     0,     0,   182,     0,     0,
       0,   312,   313,   295,   279,     0,     0,   205,   319,   320,
       0,     0,   301,   302,     0,   215,     0,     0,     0,     0,
     308,   309,     0,   324,   311,     0,     0,   314,    67,     0,
       0,   318,     0,     0,   321,     0,     0,     0,     0,     0,
     323,    83,    84,    85,    50,     0,     0,     0,     0,   326,
       0,    86,    87,    88,    90,    93,     0,   105,   106,   107,
       0,     0,     0,   110,   111,   231,   232,   233,     0,     0,
       0,     0,   157,     0,     0,   112,   113,   114,   115,   116,
      50,   117,   118,   119,     0,   162,   163,   164,   165,   166,
     167,   168,   169,   170,   171,     0,     0,   172,   173,   174,
       0,     0,   177,   178,   179,   180,   181,     0,   183,    90,
       0,     0,   199,   200,   262,   263,   264,   265,     0,   199,
     208,   209,   199,   211,     0,     0,     0,    51,    52,     0,
       0,   215,    53,     0,     0,    54,    55,    56,    57,    18,
       0,    58,    59,    60,     0,    61,    62,     0,     0,     0,
       0,   228,     0,    63,     0,     0,     0,     0,    64,     0,
      89,    65,    66,    51,    52,     0,     0,     0,    53,     0,
       0,    54,    55,    56,    57,    18,     0,    58,    59,    60,
      79,    61,    62,     0,     0,     0,   105,   106,   107,    63,
       0,   242,   110,   111,    64,     0,     0,    65,    66,     0,
       0,     0,   250,     0,     0,     0,   114,   115,   116,     0,
     117,   118,   119,     0,     0,     0,   257,   258,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,     0,     0,
       0,     0,     0,     0,   272,     0,     0,     0,     0,     0,
       0,     0,     0,   275,     0,   122,     0,     3,     4,     5,
       6,     7,     8,   123,     0,     0,     0,    50,   124,   288,
       0,   125,   126,   127,     0,     0,   293,     0,     0,     0,
       0,     0,     0,   296,   128,     0,     0,     0,     0,     0,
       0,     0,   304,     0,     0,   307,     0,     0,     0,     0,
       0,     0,     0,   129,   130,   131,     0,   132,   133,   134,
       0,   135,   136,   137,     0,   322,   138,     0,    51,    52,
       0,   139,     0,    53,     0,     0,    54,    55,    56,    57,
      18,     0,    58,    59,    60,     0,    61,    62,     0,     0,
       0,     0,     0,     0,    63,     0,     0,     0,     0,    64,
      51,    52,    65,    66,     0,    53,     0,     0,    54,    55,
      56,    57,    18,     0,    58,    59,    60,   140,    61,    62,
       0,     0,     0,     0,     0,     0,    63,     0,     0,     0,
       0,    64,     0,     0,    65,    66,   122,     0,     3,     4,
       5,     6,     7,     8,   123,     0,     0,     0,    50,   124,
       0,     0,   125,   126,   127,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   128,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   129,   130,   131,     0,   132,   133,
     134,     0,   135,   136,   137,     0,     0,   138,     0,     0,
       0,     0,   139,     0,     0,   292,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    51,    52,     0,     0,     0,    53,     0,     0,    54,
      55,    56,    57,    18,     0,    58,    59,    60,   255,    61,
      62,   315,     0,     0,     0,     0,     0,    63,     0,     0,
       0,     0,    64,     0,     0,    65,    66,    95,    96,    97,
       0,     0,     0,     0,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,     0,   110,   111,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   112,
     113,   114,   115,   116,     0,   117,   118,   119,     0,     0,
       0,     0,    95,    96,    97,     0,     0,     0,     0,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,     0,   110,   111,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   112,   113,   114,   115,   116,     0,
     117,   118,   119,    95,    96,    97,     0,     0,     0,     0,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,     0,   110,   111,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   112,   113,   114,   115,   116,
       0,   117,   118,   119,    95,    96,    97,     0,     0,   277,
       0,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,     0,   110,   111,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,   113,   114,   115,
     116,     0,   117,   118,   119,    95,    96,    97,   229,     0,
       0,     0,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,     0,   110,   111,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    46,   112,   113,   114,
     115,   116,     0,   117,   118,   119,     0,     0,   223,    95,
      96,    97,     0,     0,     0,     0,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,     0,   110,
     111,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,   113,   114,   115,   116,     0,   117,   118,   119,
       0,   226,    95,    96,    97,     0,     0,     0,     0,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,     0,   110,   111,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   112,   113,   114,   115,   116,     0,
     117,   118,   119,     0,   276,    95,    96,    97,     0,     0,
       0,     0,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,     0,   110,   111,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,   113,   114,
     115,   116,     0,   117,   118,   119,   158,    95,    96,    97,
       0,     0,     0,     0,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,     0,   110,   111,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   112,
     113,   114,   115,   116,     0,   117,   118,   119,   305,    95,
      96,    97,     0,     0,     0,     0,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,     0,   110,
     111,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      46,   112,   113,   114,   115,   116,     0,   117,   118,   119,
      95,    96,    97,     0,     0,     0,     0,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,     0,
     110,   111,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   112,   113,   114,   115,   116,     0,   117,   118,
     119,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,     0,     0,   110,   111,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,   113,   114,   115,
     116,     0,   117,   118,   119,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,     0,     0,     0,   110,   111,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,   113,   114,   115,   116,     0,   117,   118,   119,   100,
     101,   102,   103,   104,   105,   106,   107,     0,     0,     0,
     110,   111,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   112,   113,   114,   115,   116,     0,   117,   118,
     119
};

static const yytype_int16 yycheck[] =
{
      36,   142,    20,   156,     3,     4,    42,    27,     7,     8,
       9,    10,    11,    12,   112,   156,     0,   112,   117,     0,
     126,    81,     3,     4,     5,     0,     7,     8,     9,    10,
      11,    12,   127,    14,    15,    16,    35,    12,   117,    20,
      37,   128,    39,    40,    41,    26,    27,    28,    29,   128,
      47,    48,    49,    19,    35,    36,    37,    86,    39,    40,
      41,    27,     4,     5,     6,     7,    47,    48,    49,   117,
     129,    89,   128,   132,    45,    46,   127,   128,   100,   101,
      27,    28,   117,   126,   112,   132,   128,    23,    67,   125,
     126,     5,   128,   133,   127,    39,    37,   117,    36,    39,
     136,   137,    39,   139,    86,    79,    15,   143,   100,   101,
     102,    14,    20,    29,   106,   107,    16,    12,   117,   155,
     117,     1,     2,     3,     4,     5,     6,     7,     8,     9,
      26,   194,   124,   125,   126,    37,   117,   222,   285,   270,
     251,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   306,    34,    -1,    -1,    37,    -1,    39,
      40,    41,   198,    -1,   200,   306,   202,    -1,    -1,    -1,
      -1,   207,   208,   209,   210,   211,    -1,   213,    -1,    -1,
      -1,   241,   218,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   232,   233,    78,    -1,
      -1,    -1,   238,    -1,    -1,    -1,   242,    -1,   268,    -1,
      -1,    -1,    -1,   231,   250,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   259,    -1,    -1,    -1,   263,   264,   265,
     110,   111,    -1,    -1,   294,    -1,    -1,   117,    -1,    -1,
      -1,   301,   302,   279,   262,    -1,    -1,   127,   308,   309,
      -1,    -1,   288,   289,    -1,   135,    -1,    -1,    -1,    -1,
     296,   297,    -1,   323,   300,    -1,    -1,   303,    38,    -1,
      -1,   307,    -1,    -1,   310,    -1,    -1,    -1,    -1,    -1,
     316,    51,    52,    53,    17,    -1,    -1,    -1,    -1,   325,
      -1,    61,    62,    63,    64,    65,    -1,   100,   101,   102,
      -1,    -1,    -1,   106,   107,   185,   186,   187,    -1,    -1,
      -1,    -1,    82,    -1,    -1,   118,   119,   120,   121,   122,
      17,   124,   125,   126,    -1,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,    -1,    -1,   107,   108,   109,
      -1,    -1,   112,   113,   114,   115,   116,    -1,   118,   119,
      -1,    -1,   122,   123,   234,   235,   236,   237,    -1,   129,
     130,   131,   132,   133,    -1,    -1,    -1,   100,   101,    -1,
      -1,   251,   105,    -1,    -1,   108,   109,   110,   111,   112,
      -1,   114,   115,   116,    -1,   118,   119,    -1,    -1,    -1,
      -1,   161,    -1,   126,    -1,    -1,    -1,    -1,   131,    -1,
     133,   134,   135,   100,   101,    -1,    -1,    -1,   105,    -1,
      -1,   108,   109,   110,   111,   112,    -1,   114,   115,   116,
     117,   118,   119,    -1,    -1,    -1,   100,   101,   102,   126,
      -1,   201,   106,   107,   131,    -1,    -1,   134,   135,    -1,
      -1,    -1,   212,    -1,    -1,    -1,   120,   121,   122,    -1,
     124,   125,   126,    -1,    -1,    -1,   226,   227,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   244,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,     5,    -1,     7,     8,     9,
      10,    11,    12,    13,    -1,    -1,    -1,    17,    18,   269,
      -1,    21,    22,    23,    -1,    -1,   276,    -1,    -1,    -1,
      -1,    -1,    -1,   283,    34,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   292,    -1,    -1,   295,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    53,    54,    55,    -1,    57,    58,    59,
      -1,    61,    62,    63,    -1,   315,    66,    -1,   100,   101,
      -1,    71,    -1,   105,    -1,    -1,   108,   109,   110,   111,
     112,    -1,   114,   115,   116,    -1,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,   131,
     100,   101,   134,   135,    -1,   105,    -1,    -1,   108,   109,
     110,   111,   112,    -1,   114,   115,   116,   117,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
      -1,   131,    -1,    -1,   134,   135,     5,    -1,     7,     8,
       9,    10,    11,    12,    13,    -1,    -1,    -1,    17,    18,
      -1,    -1,    21,    22,    23,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    53,    54,    55,    -1,    57,    58,
      59,    -1,    61,    62,    63,    -1,    -1,    66,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,    -1,    -1,   105,    -1,    -1,   108,
     109,   110,   111,   112,    -1,   114,   115,   116,   117,   118,
     119,    25,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,
      -1,    -1,   131,    -1,    -1,   134,   135,    86,    87,    88,
      -1,    -1,    -1,    -1,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,
     119,   120,   121,   122,    -1,   124,   125,   126,    -1,    -1,
      -1,    -1,    86,    87,    88,    -1,    -1,    -1,    -1,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   118,   119,   120,   121,   122,    -1,
     124,   125,   126,    86,    87,    88,    -1,    -1,    -1,    -1,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   118,   119,   120,   121,   122,
      -1,   124,   125,   126,    86,    87,    88,    -1,    -1,   132,
      -1,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118,   119,   120,   121,
     122,    -1,   124,   125,   126,    86,    87,    88,   130,    -1,
      -1,    -1,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,   119,   120,
     121,   122,    -1,   124,   125,   126,    -1,    -1,   129,    86,
      87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   118,   119,   120,   121,   122,    -1,   124,   125,   126,
      -1,   128,    86,    87,    88,    -1,    -1,    -1,    -1,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   118,   119,   120,   121,   122,    -1,
     124,   125,   126,    -1,   128,    86,    87,    88,    -1,    -1,
      -1,    -1,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,   120,
     121,   122,    -1,   124,   125,   126,   127,    86,    87,    88,
      -1,    -1,    -1,    -1,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,
     119,   120,   121,   122,    -1,   124,   125,   126,   127,    86,
      87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,   119,   120,   121,   122,    -1,   124,   125,   126,
      86,    87,    88,    -1,    -1,    -1,    -1,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,   119,   120,   121,   122,    -1,   124,   125,
     126,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118,   119,   120,   121,
     122,    -1,   124,   125,   126,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,   119,   120,   121,   122,    -1,   124,   125,   126,    95,
      96,    97,    98,    99,   100,   101,   102,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,   119,   120,   121,   122,    -1,   124,   125,
     126
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     7,     8,     9,    10,    11,    12,    35,
     117,   137,   138,   139,   140,   142,   144,   155,   112,   191,
     191,   156,   157,   191,   156,   156,   156,   156,   158,   191,
     191,     0,   139,   117,   126,   153,   153,   128,    86,   128,
      45,    46,   146,   127,   154,   191,   117,   192,   192,   157,
      17,   100,   101,   105,   108,   109,   110,   111,   114,   115,
     116,   118,   119,   126,   131,   134,   135,   184,   185,   186,
     187,   188,   191,   191,   191,   191,   192,   127,   128,   117,
     141,   143,   126,   184,   184,   184,   184,   184,   184,   133,
     184,   189,   190,   184,   112,    86,    87,    88,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     106,   107,   118,   119,   120,   121,   122,   124,   125,   126,
     145,   191,     5,    13,    18,    21,    22,    23,    34,    53,
      54,    55,    57,    58,    59,    61,    62,    63,    66,    71,
     117,   155,   159,   160,   161,   162,   163,   168,   169,   171,
     175,   177,   178,   179,   180,   184,   159,   184,   127,   153,
     132,   128,   184,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   184,   191,   191,   184,   184,   184,
     184,   184,   191,   184,   189,    37,    39,    40,    41,    47,
      48,    49,   117,   147,   148,   149,   150,   151,   183,   184,
     184,    19,    27,   192,   192,   191,   192,   183,   184,   184,
     183,   184,    23,   181,   182,   191,   192,   192,    67,   192,
     175,   176,   192,   129,   192,   176,   128,   133,   184,   130,
     127,   191,   191,   191,    37,    39,    39,    39,    36,   149,
     192,   192,   184,   192,    86,   192,   192,   192,   192,   192,
     184,   128,   192,    79,   192,   117,   161,   184,   184,   153,
     192,   192,   191,   191,   191,   191,   192,   159,   192,    27,
     172,   173,   184,   192,   182,   184,   128,   132,   192,   153,
     192,   192,   192,    15,   164,   165,   166,   159,   184,    28,
     173,   174,    24,   184,   152,   192,   184,    14,   167,   166,
      20,   192,   192,    29,   184,   127,   159,   184,   192,   192,
      16,   192,   159,   159,   192,    25,   170,   176,   192,   159,
     159,   192,   184,   192,   159,    26,   192
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,   136,   137,   137,   138,   138,   139,   139,   139,   139,
     139,   141,   140,   143,   142,   145,   144,   146,   146,   146,
     147,   147,   148,   148,   149,   149,   149,   150,   150,   150,
     150,   150,   152,   151,   151,   153,   153,   153,   154,   154,
     155,   155,   155,   155,   155,   155,   156,   156,   157,   157,
     158,   158,   159,   159,   160,   160,   161,   161,   161,   161,
     161,   161,   161,   161,   161,   161,   161,   161,   162,   162,
     163,   164,   164,   165,   165,   166,   167,   167,   168,   169,
     170,   170,   171,   172,   172,   173,   174,   174,   175,   176,
     176,   177,   178,   179,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   180,   180,   181,   181,   182,   183,   183,
     184,   184,   184,   184,   185,   185,   185,   185,   185,   185,
     185,   185,   185,   185,   185,   185,   185,   185,   185,   185,
     185,   185,   186,   186,   186,   186,   186,   187,   187,   187,
     187,   187,   187,   187,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   189,   189,
     190,   190,   191,   192,   192,   192
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     1,     1,     2,     1,     1,     1,     2,
       1,     0,     7,     0,     7,     0,     8,     0,     2,     2,
       0,     1,     1,     2,     1,     1,     1,     3,     3,     4,
       4,     4,     0,     7,     7,     0,     2,     3,     1,     3,
       2,     2,     2,     2,     2,     2,     1,     3,     1,     3,
       1,     3,     0,     1,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       8,     0,     1,     1,     2,     4,     0,     3,     7,    11,
       0,     2,     7,     1,     2,     4,     0,     3,     3,     0,
       1,     2,     2,     2,     3,     3,     3,     3,     3,     4,
       3,     2,     2,     3,     2,     1,     3,     3,     0,     1,
       1,     1,     1,     1,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     2,     2,     2,     4,     3,     3,
       3,     4,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     6,     8,     2,     2,     0,     1,
       1,     3,     1,     0,     1,     2
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
  case 11: /* @1: %empty  */
#line 125 "./xgram.y"
                {
			(yyvsp[-2].symptr)->sclass = S_FUNC;
			enter_scope();
			(yyval.symptr) = (yyvsp[-2].symptr);
		}
#line 2002 "y.tab.c"
    break;

  case 12: /* function_def: FUNCTION identifier opt_param_list opt_eol @1 opt_statement_list opt_return_stmt  */
#line 132 "./xgram.y"
                {
			SYMTAB *func = (yyvsp[-2].symptr);
			struct node *body = (yyvsp[-1].nodeptr);

			/* Add return statement to body if present */
			if ((yyvsp[0].nodeptr) && body) {
				/* Append return to body */
				body = make_node(N_BLOCK, body, (yyvsp[0].nodeptr));
			} else if ((yyvsp[0].nodeptr)) {
				body = (yyvsp[0].nodeptr);
			}

			/* Run semantic analysis */
			semantic_analyze_function(func, body);

			/* Generate code */
			codegen_function(func, body);

			exit_scope();
		}
#line 2027 "y.tab.c"
    break;

  case 13: /* @2: %empty  */
#line 157 "./xgram.y"
                {
			(yyvsp[-2].symptr)->sclass = S_PROC;
			enter_scope();
			(yyval.symptr) = (yyvsp[-2].symptr);
		}
#line 2037 "y.tab.c"
    break;

  case 14: /* procedure_def: PROCEDURE identifier opt_param_list opt_eol @2 opt_statement_list opt_return_stmt  */
#line 164 "./xgram.y"
                {
			SYMTAB *proc = (yyvsp[-2].symptr);
			struct node *body = (yyvsp[-1].nodeptr);

			/* Add return statement to body if present */
			if ((yyvsp[0].nodeptr) && body) {
				body = make_node(N_BLOCK, body, (yyvsp[0].nodeptr));
			} else if ((yyvsp[0].nodeptr)) {
				body = (yyvsp[0].nodeptr);
			}

			/* Run semantic analysis */
			semantic_analyze_function(proc, body);

			/* Generate code */
			codegen_function(proc, body);

			exit_scope();
		}
#line 2061 "y.tab.c"
    break;

  case 15: /* $@3: %empty  */
#line 188 "./xgram.y"
                {
			enter_scope();
		}
#line 2069 "y.tab.c"
    break;

  case 16: /* class_def: CLASS identifier opt_from_clause opt_eol $@3 opt_class_members ENDCLASS opt_eol  */
#line 193 "./xgram.y"
                {
			exit_scope();
		}
#line 2077 "y.tab.c"
    break;

  case 32: /* $@4: %empty  */
#line 230 "./xgram.y"
                {
			enter_scope();
		}
#line 2085 "y.tab.c"
    break;

  case 33: /* method_def: METHOD identifier opt_param_list opt_eol $@4 opt_statement_list opt_return_stmt  */
#line 235 "./xgram.y"
                {
			exit_scope();
		}
#line 2093 "y.tab.c"
    break;

  case 52: /* opt_statement_list: %empty  */
#line 281 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2099 "y.tab.c"
    break;

  case 55: /* statement_list: statement_list opt_eol statement  */
#line 288 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_BLOCK, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2105 "y.tab.c"
    break;

  case 56: /* statement: expr_stmt  */
#line 293 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[0].nodeptr); }
#line 2111 "y.tab.c"
    break;

  case 57: /* statement: if_stmt  */
#line 295 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[0].nodeptr); }
#line 2117 "y.tab.c"
    break;

  case 58: /* statement: while_stmt  */
#line 297 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[0].nodeptr); }
#line 2123 "y.tab.c"
    break;

  case 59: /* statement: for_stmt  */
#line 299 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[0].nodeptr); }
#line 2129 "y.tab.c"
    break;

  case 60: /* statement: case_stmt  */
#line 301 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[0].nodeptr); }
#line 2135 "y.tab.c"
    break;

  case 61: /* statement: return_stmt  */
#line 303 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[0].nodeptr); }
#line 2141 "y.tab.c"
    break;

  case 62: /* statement: break_stmt  */
#line 305 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2147 "y.tab.c"
    break;

  case 63: /* statement: exit_stmt  */
#line 307 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2153 "y.tab.c"
    break;

  case 64: /* statement: loop_stmt  */
#line 309 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2159 "y.tab.c"
    break;

  case 65: /* statement: variable_decl  */
#line 311 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2165 "y.tab.c"
    break;

  case 66: /* statement: db_command  */
#line 313 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2171 "y.tab.c"
    break;

  case 67: /* statement: EOL  */
#line 315 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2177 "y.tab.c"
    break;

  case 70: /* if_stmt: IF expression opt_eol opt_statement_list opt_elseif_list opt_else_clause ENDIF opt_eol  */
#line 330 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_IF, (yyvsp[-6].nodeptr), (yyvsp[-4].nodeptr)); }
#line 2183 "y.tab.c"
    break;

  case 78: /* while_stmt: DO WHILE expression opt_eol opt_statement_list ENDDO opt_eol  */
#line 359 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_WHILE, (yyvsp[-4].nodeptr), (yyvsp[-2].nodeptr)); }
#line 2189 "y.tab.c"
    break;

  case 79: /* for_stmt: FOR identifier ASSIGN expression TO expression opt_step opt_eol opt_statement_list NEXT opt_eol  */
#line 367 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_FOR, NULL, NULL); }
#line 2195 "y.tab.c"
    break;

  case 82: /* case_stmt: DO CASE opt_eol case_list opt_otherwise ENDCASE opt_eol  */
#line 381 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2201 "y.tab.c"
    break;

  case 88: /* return_stmt: RETURN opt_expression opt_eol  */
#line 403 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_RETURN, (yyvsp[-1].nodeptr), NULL); }
#line 2207 "y.tab.c"
    break;

  case 89: /* opt_return_stmt: %empty  */
#line 408 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2213 "y.tab.c"
    break;

  case 90: /* opt_return_stmt: return_stmt  */
#line 410 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[0].nodeptr); }
#line 2219 "y.tab.c"
    break;

  case 108: /* opt_expression: %empty  */
#line 452 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2225 "y.tab.c"
    break;

  case 114: /* binary_expr: expression '+' expression  */
#line 465 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_PLUS, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2231 "y.tab.c"
    break;

  case 115: /* binary_expr: expression '-' expression  */
#line 467 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_MINUS, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2237 "y.tab.c"
    break;

  case 116: /* binary_expr: expression '*' expression  */
#line 469 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_MUL, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2243 "y.tab.c"
    break;

  case 117: /* binary_expr: expression '/' expression  */
#line 471 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_DIV, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2249 "y.tab.c"
    break;

  case 118: /* binary_expr: expression '%' expression  */
#line 473 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_MOD, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2255 "y.tab.c"
    break;

  case 119: /* binary_expr: expression POWER expression  */
#line 475 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_PLUS, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); /* TODO: POWER op */ }
#line 2261 "y.tab.c"
    break;

  case 120: /* binary_expr: expression EQ expression  */
#line 477 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_EQ, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2267 "y.tab.c"
    break;

  case 121: /* binary_expr: expression NE expression  */
#line 479 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_NE, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2273 "y.tab.c"
    break;

  case 122: /* binary_expr: expression LT expression  */
#line 481 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_LT, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2279 "y.tab.c"
    break;

  case 123: /* binary_expr: expression LE expression  */
#line 483 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_LE, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2285 "y.tab.c"
    break;

  case 124: /* binary_expr: expression GT expression  */
#line 485 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_GT, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2291 "y.tab.c"
    break;

  case 125: /* binary_expr: expression GE expression  */
#line 487 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_GE, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2297 "y.tab.c"
    break;

  case 126: /* binary_expr: expression AND expression  */
#line 489 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_AND, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2303 "y.tab.c"
    break;

  case 127: /* binary_expr: expression OR expression  */
#line 491 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_OR, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2309 "y.tab.c"
    break;

  case 128: /* binary_expr: expression SUBSTR expression  */
#line 493 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_PLUS, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); /* TODO: SUBSTR op */ }
#line 2315 "y.tab.c"
    break;

  case 129: /* binary_expr: expression ASSIGN expression  */
#line 495 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_ASSIGN, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); }
#line 2321 "y.tab.c"
    break;

  case 130: /* binary_expr: expression PLUSASSIGN expression  */
#line 497 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_ASSIGN, (yyvsp[-2].nodeptr), make_node(N_PLUS, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr))); }
#line 2327 "y.tab.c"
    break;

  case 131: /* binary_expr: expression MINUSASSIGN expression  */
#line 499 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_ASSIGN, (yyvsp[-2].nodeptr), make_node(N_MINUS, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr))); }
#line 2333 "y.tab.c"
    break;

  case 132: /* unary_expr: NOT expression  */
#line 504 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_NOT, (yyvsp[0].nodeptr), NULL); }
#line 2339 "y.tab.c"
    break;

  case 133: /* unary_expr: '-' expression  */
#line 506 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_UMINUS, (yyvsp[0].nodeptr), NULL); }
#line 2345 "y.tab.c"
    break;

  case 134: /* unary_expr: '+' expression  */
#line 508 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[0].nodeptr); }
#line 2351 "y.tab.c"
    break;

  case 135: /* unary_expr: INCR expression  */
#line 510 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_ASSIGN, (yyvsp[0].nodeptr), make_node(N_PLUS, (yyvsp[0].nodeptr), make_icon(1))); }
#line 2357 "y.tab.c"
    break;

  case 136: /* unary_expr: DECR expression  */
#line 512 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_ASSIGN, (yyvsp[0].nodeptr), make_node(N_MINUS, (yyvsp[0].nodeptr), make_icon(1))); }
#line 2363 "y.tab.c"
    break;

  case 137: /* postfix_expr: expression '[' expression ']'  */
#line 517 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_SUBSCR, (yyvsp[-3].nodeptr), (yyvsp[-1].nodeptr)); }
#line 2369 "y.tab.c"
    break;

  case 138: /* postfix_expr: expression '.' identifier  */
#line 519 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_FIELD, (yyvsp[-2].nodeptr), make_name((yyvsp[0].symptr))); }
#line 2375 "y.tab.c"
    break;

  case 139: /* postfix_expr: expression ARROW identifier  */
#line 521 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_FIELD, (yyvsp[-2].nodeptr), make_name((yyvsp[0].symptr))); }
#line 2381 "y.tab.c"
    break;

  case 140: /* postfix_expr: expression DCOLON identifier  */
#line 523 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_FIELD, (yyvsp[-2].nodeptr), make_name((yyvsp[0].symptr))); }
#line 2387 "y.tab.c"
    break;

  case 141: /* postfix_expr: expression '(' opt_expr_list ')'  */
#line 525 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_CALL, (yyvsp[-3].nodeptr), (yyvsp[-1].nodeptr)); }
#line 2393 "y.tab.c"
    break;

  case 142: /* postfix_expr: expression INCR  */
#line 527 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_ASSIGN, (yyvsp[-1].nodeptr), make_node(N_PLUS, (yyvsp[-1].nodeptr), make_icon(1))); }
#line 2399 "y.tab.c"
    break;

  case 143: /* postfix_expr: expression DECR  */
#line 529 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_ASSIGN, (yyvsp[-1].nodeptr), make_node(N_MINUS, (yyvsp[-1].nodeptr), make_icon(1))); }
#line 2405 "y.tab.c"
    break;

  case 144: /* primary_expr: INTEGER  */
#line 534 "./xgram.y"
                { (yyval.nodeptr) = make_icon((yyvsp[0].ival)); }
#line 2411 "y.tab.c"
    break;

  case 145: /* primary_expr: FLOAT  */
#line 536 "./xgram.y"
                { (yyval.nodeptr) = make_fcon((yyvsp[0].fval)); }
#line 2417 "y.tab.c"
    break;

  case 146: /* primary_expr: STRING  */
#line 538 "./xgram.y"
                { (yyval.nodeptr) = make_scon((yyvsp[0].sval)); }
#line 2423 "y.tab.c"
    break;

  case 147: /* primary_expr: DATE  */
#line 540 "./xgram.y"
                { (yyval.nodeptr) = make_scon((yyvsp[0].sval)); /* TODO: date type */ }
#line 2429 "y.tab.c"
    break;

  case 148: /* primary_expr: LTRUE  */
#line 542 "./xgram.y"
                { (yyval.nodeptr) = make_icon(1); }
#line 2435 "y.tab.c"
    break;

  case 149: /* primary_expr: LFALSE  */
#line 544 "./xgram.y"
                { (yyval.nodeptr) = make_icon(0); }
#line 2441 "y.tab.c"
    break;

  case 150: /* primary_expr: NIL  */
#line 546 "./xgram.y"
                { (yyval.nodeptr) = make_icon(0); /* TODO: NIL */ }
#line 2447 "y.tab.c"
    break;

  case 151: /* primary_expr: identifier  */
#line 548 "./xgram.y"
                { (yyval.nodeptr) = make_name((yyvsp[0].symptr)); }
#line 2453 "y.tab.c"
    break;

  case 152: /* primary_expr: '(' expression ')'  */
#line 550 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[-1].nodeptr); }
#line 2459 "y.tab.c"
    break;

  case 153: /* primary_expr: '{' opt_expr_list '}'  */
#line 552 "./xgram.y"
                { (yyval.nodeptr) = NULL; /* TODO: array literal */ }
#line 2465 "y.tab.c"
    break;

  case 154: /* primary_expr: '{' '|' opt_param_list '|' expression '}'  */
#line 554 "./xgram.y"
                { (yyval.nodeptr) = NULL; /* TODO: code block */ }
#line 2471 "y.tab.c"
    break;

  case 155: /* primary_expr: IIF '(' expression ',' expression ',' expression ')'  */
#line 556 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_IF, (yyvsp[-5].nodeptr), make_node(N_BLOCK, (yyvsp[-3].nodeptr), (yyvsp[-1].nodeptr))); }
#line 2477 "y.tab.c"
    break;

  case 156: /* primary_expr: '@' expression  */
#line 558 "./xgram.y"
                { (yyval.nodeptr) = (yyvsp[0].nodeptr); /* TODO: macro expansion */ }
#line 2483 "y.tab.c"
    break;

  case 157: /* primary_expr: '&' IDENT  */
#line 560 "./xgram.y"
                { (yyval.nodeptr) = NULL; /* TODO: macro variable */ }
#line 2489 "y.tab.c"
    break;

  case 158: /* opt_expr_list: %empty  */
#line 565 "./xgram.y"
                { (yyval.nodeptr) = NULL; }
#line 2495 "y.tab.c"
    break;

  case 161: /* expr_list: expr_list ',' expression  */
#line 572 "./xgram.y"
                { (yyval.nodeptr) = make_node(N_BLOCK, (yyvsp[-2].nodeptr), (yyvsp[0].nodeptr)); /* TODO: expr list */ }
#line 2501 "y.tab.c"
    break;

  case 162: /* identifier: IDENT  */
#line 578 "./xgram.y"
                {
			SYMTAB *sp = lookup((yyvsp[0].sval), current_scope);
			if (sp == NULL) {
				sp = install((yyvsp[0].sval), S_NULL, NULL, current_scope);
			}
			(yyval.symptr) = sp;
		}
#line 2513 "y.tab.c"
    break;


#line 2517 "y.tab.c"

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

#line 594 "./xgram.y"


/* Error handling */
void yyerror(const char *s) {
	error("%s at line %d", s, lineno);
}
