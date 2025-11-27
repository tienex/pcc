/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_GRAM_H_INCLUDED
# define YY_YY_GRAM_H_INCLUDED
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
    IDENTIFIER = 258,              /* IDENTIFIER  */
    STRING_LITERAL = 259,          /* STRING_LITERAL  */
    INTEGER = 260,                 /* INTEGER  */
    LEVEL_NUMBER = 261,            /* LEVEL_NUMBER  */
    FLOAT = 262,                   /* FLOAT  */
    IDENTIFICATION_DIVISION = 263, /* IDENTIFICATION_DIVISION  */
    ENVIRONMENT_DIVISION = 264,    /* ENVIRONMENT_DIVISION  */
    DATA_DIVISION = 265,           /* DATA_DIVISION  */
    PROCEDURE_DIVISION = 266,      /* PROCEDURE_DIVISION  */
    CONFIGURATION_SECTION = 267,   /* CONFIGURATION_SECTION  */
    INPUT_OUTPUT_SECTION = 268,    /* INPUT_OUTPUT_SECTION  */
    FILE_SECTION = 269,            /* FILE_SECTION  */
    WORKING_STORAGE_SECTION = 270, /* WORKING_STORAGE_SECTION  */
    LOCAL_STORAGE_SECTION = 271,   /* LOCAL_STORAGE_SECTION  */
    LINKAGE_SECTION = 272,         /* LINKAGE_SECTION  */
    CLASS = 273,                   /* CLASS  */
    CLASS_ID = 274,                /* CLASS_ID  */
    METHOD = 275,                  /* METHOD  */
    METHOD_ID = 276,               /* METHOD_ID  */
    OBJECT = 277,                  /* OBJECT  */
    INHERITS = 278,                /* INHERITS  */
    INTERFACE = 279,               /* INTERFACE  */
    INTERFACE_ID = 280,            /* INTERFACE_ID  */
    FACTORY = 281,                 /* FACTORY  */
    INVOKE = 282,                  /* INVOKE  */
    NEW = 283,                     /* NEW  */
    SELF = 284,                    /* SELF  */
    SUPER = 285,                   /* SUPER  */
    PROPERTY = 286,                /* PROPERTY  */
    STATIC = 287,                  /* STATIC  */
    INSTANCE = 288,                /* INSTANCE  */
    FINAL = 289,                   /* FINAL  */
    ABSTRACT = 290,                /* ABSTRACT  */
    OVERRIDE = 291,                /* OVERRIDE  */
    PIC = 292,                     /* PIC  */
    VALUE = 293,                   /* VALUE  */
    REDEFINES = 294,               /* REDEFINES  */
    OCCURS = 295,                  /* OCCURS  */
    TIMES = 296,                   /* TIMES  */
    DEPENDING_ON = 297,            /* DEPENDING_ON  */
    INDEXED_BY = 298,              /* INDEXED_BY  */
    USAGE = 299,                   /* USAGE  */
    COMP = 300,                    /* COMP  */
    COMP_1 = 301,                  /* COMP_1  */
    COMP_2 = 302,                  /* COMP_2  */
    COMP_3 = 303,                  /* COMP_3  */
    COMP_4 = 304,                  /* COMP_4  */
    COMP_5 = 305,                  /* COMP_5  */
    BINARY = 306,                  /* BINARY  */
    PACKED_DECIMAL = 307,          /* PACKED_DECIMAL  */
    DISPLAY_USAGE = 308,           /* DISPLAY_USAGE  */
    POINTER = 309,                 /* POINTER  */
    OBJECT_REFERENCE = 310,        /* OBJECT_REFERENCE  */
    ACCEPT = 311,                  /* ACCEPT  */
    ADD = 312,                     /* ADD  */
    CALL = 313,                    /* CALL  */
    COMPUTE = 314,                 /* COMPUTE  */
    DELETE = 315,                  /* DELETE  */
    DISPLAY = 316,                 /* DISPLAY  */
    DIVIDE_OP = 317,               /* DIVIDE_OP  */
    EVALUATE = 318,                /* EVALUATE  */
    EXIT = 319,                    /* EXIT  */
    GOTO = 320,                    /* GOTO  */
    IF = 321,                      /* IF  */
    ELSE = 322,                    /* ELSE  */
    END_IF = 323,                  /* END_IF  */
    MOVE = 324,                    /* MOVE  */
    MULTIPLY = 325,                /* MULTIPLY  */
    PERFORM = 326,                 /* PERFORM  */
    END_PERFORM = 327,             /* END_PERFORM  */
    READ = 328,                    /* READ  */
    RETURN = 329,                  /* RETURN  */
    RETURNING = 330,               /* RETURNING  */
    REWRITE = 331,                 /* REWRITE  */
    SEARCH = 332,                  /* SEARCH  */
    SET = 333,                     /* SET  */
    STOP = 334,                    /* STOP  */
    STRING_STMT = 335,             /* STRING_STMT  */
    SUBTRACT = 336,                /* SUBTRACT  */
    UNSTRING = 337,                /* UNSTRING  */
    WRITE = 338,                   /* WRITE  */
    UNTIL = 339,                   /* UNTIL  */
    VARYING = 340,                 /* VARYING  */
    WHEN = 341,                    /* WHEN  */
    OTHER = 342,                   /* OTHER  */
    THROUGH = 343,                 /* THROUGH  */
    THRU = 344,                    /* THRU  */
    BY = 345,                      /* BY  */
    GIVING = 346,                  /* GIVING  */
    REMAINDER = 347,               /* REMAINDER  */
    INTO = 348,                    /* INTO  */
    FROM = 349,                    /* FROM  */
    OPEN = 350,                    /* OPEN  */
    CLOSE = 351,                   /* CLOSE  */
    INPUT = 352,                   /* INPUT  */
    OUTPUT = 353,                  /* OUTPUT  */
    IO = 354,                      /* IO  */
    EXTEND = 355,                  /* EXTEND  */
    FILE = 356,                    /* FILE  */
    FD = 357,                      /* FD  */
    SD = 358,                      /* SD  */
    SELECT = 359,                  /* SELECT  */
    ASSIGN = 360,                  /* ASSIGN  */
    ORGANIZATION = 361,            /* ORGANIZATION  */
    ACCESS_MODE = 362,             /* ACCESS_MODE  */
    SEQUENTIAL = 363,              /* SEQUENTIAL  */
    INDEXED = 364,                 /* INDEXED  */
    RELATIVE = 365,                /* RELATIVE  */
    RANDOM = 366,                  /* RANDOM  */
    DYNAMIC = 367,                 /* DYNAMIC  */
    RECORD_KEY = 368,              /* RECORD_KEY  */
    AND = 369,                     /* AND  */
    OR = 370,                      /* OR  */
    NOT = 371,                     /* NOT  */
    EQUAL = 372,                   /* EQUAL  */
    GREATER = 373,                 /* GREATER  */
    LESS = 374,                    /* LESS  */
    GREATER_EQUAL = 375,           /* GREATER_EQUAL  */
    LESS_EQUAL = 376,              /* LESS_EQUAL  */
    NOT_EQUAL = 377,               /* NOT_EQUAL  */
    PLUS = 378,                    /* PLUS  */
    MINUS = 379,                   /* MINUS  */
    POWER = 380,                   /* POWER  */
    PROGRAM_ID = 381,              /* PROGRAM_ID  */
    AUTHOR = 382,                  /* AUTHOR  */
    DATE_WRITTEN = 383,            /* DATE_WRITTEN  */
    IS = 384,                      /* IS  */
    ARE = 385,                     /* ARE  */
    TO = 386,                      /* TO  */
    OF = 387,                      /* OF  */
    IN = 388,                      /* IN  */
    WITH = 389,                    /* WITH  */
    ALL = 390,                     /* ALL  */
    USING = 391,                   /* USING  */
    TRUE = 392,                    /* TRUE  */
    FALSE = 393,                   /* FALSE  */
    ZERO = 394,                    /* ZERO  */
    SPACE = 395,                   /* SPACE  */
    NULL_TOK = 396,                /* NULL_TOK  */
    INITIAL = 397,                 /* INITIAL  */
    CONTROL = 398,                 /* CONTROL  */
    FILLER = 399,                  /* FILLER  */
    END = 400,                     /* END  */
    PROGRAM = 401,                 /* PROGRAM  */
    RUN = 402,                     /* RUN  */
    DOT = 403,                     /* DOT  */
    COMMA = 404,                   /* COMMA  */
    SEMICOLON = 405,               /* SEMICOLON  */
    COLON = 406,                   /* COLON  */
    LPAREN = 407,                  /* LPAREN  */
    RPAREN = 408,                  /* RPAREN  */
    DIVIDE = 409,                  /* DIVIDE  */
    UMINUS = 410                   /* UMINUS  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 20 "cbolgrammar.y"

	int intval;
	double floatval;
	char *string;
	struct cobsym *symbol;
	struct picture *pic;
	NODE *node;

#line 228 "gram.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_GRAM_H_INCLUDED  */
