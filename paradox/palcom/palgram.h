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

#ifndef YY_YY_PALGRAM_TAB_H_INCLUDED
# define YY_YY_PALGRAM_TAB_H_INCLUDED
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
    AND = 258,                     /* AND  */
    ARRAY = 259,                   /* ARRAY  */
    AS = 260,                      /* AS  */
    BBEGIN = 261,                  /* BBEGIN  */
    BREAK = 262,                   /* BREAK  */
    CASE = 263,                    /* CASE  */
    CONST = 264,                   /* CONST  */
    CONTINUE = 265,                /* CONTINUE  */
    DEFAULT = 266,                 /* DEFAULT  */
    DO = 267,                      /* DO  */
    DOWNTO = 268,                  /* DOWNTO  */
    ELSE = 269,                    /* ELSE  */
    ELSIF = 270,                   /* ELSIF  */
    END = 271,                     /* END  */
    ENDFOR = 272,                  /* ENDFOR  */
    ENDFOREACH = 273,              /* ENDFOREACH  */
    ENDIF = 274,                   /* ENDIF  */
    ENDMETHOD = 275,               /* ENDMETHOD  */
    ENDPROC = 276,                 /* ENDPROC  */
    ENDSCAN = 277,                 /* ENDSCAN  */
    ENDSWITCH = 278,               /* ENDSWITCH  */
    ENDWHILE = 279,                /* ENDWHILE  */
    EXCEPT = 280,                  /* EXCEPT  */
    FALSE = 281,                   /* FALSE  */
    FOR = 282,                     /* FOR  */
    FOREACH = 283,                 /* FOREACH  */
    FROM = 284,                    /* FROM  */
    FUNCTION = 285,                /* FUNCTION  */
    IF = 286,                      /* IF  */
    IN = 287,                      /* IN  */
    INTDIV = 288,                  /* INTDIV  */
    LIKE = 289,                    /* LIKE  */
    METHOD = 290,                  /* METHOD  */
    MOD = 291,                     /* MOD  */
    NOT = 292,                     /* NOT  */
    OF = 293,                      /* OF  */
    OR = 294,                      /* OR  */
    PRIVATE = 295,                 /* PRIVATE  */
    PROC = 296,                    /* PROC  */
    PROTECTED = 297,               /* PROTECTED  */
    PUBLIC = 298,                  /* PUBLIC  */
    QUIT = 299,                    /* QUIT  */
    RECORD = 300,                  /* RECORD  */
    RETURN = 301,                  /* RETURN  */
    SCAN = 302,                    /* SCAN  */
    SELF = 303,                    /* SELF  */
    STEP = 304,                    /* STEP  */
    SWITCH = 305,                  /* SWITCH  */
    THEN = 306,                    /* THEN  */
    TO = 307,                      /* TO  */
    TRUE = 308,                    /* TRUE  */
    TRY = 309,                     /* TRY  */
    TYPE = 310,                    /* TYPE  */
    UNTIL = 311,                   /* UNTIL  */
    USES = 312,                    /* USES  */
    VAR = 313,                     /* VAR  */
    WHILE = 314,                   /* WHILE  */
    WITH = 315,                    /* WITH  */
    TSMALLINT = 316,               /* TSMALLINT  */
    TSHORTINT = 317,               /* TSHORTINT  */
    TLONGINT = 318,                /* TLONGINT  */
    TNUMBER = 319,                 /* TNUMBER  */
    TCURRENCY = 320,               /* TCURRENCY  */
    TLOGICAL = 321,                /* TLOGICAL  */
    TSTRING = 322,                 /* TSTRING  */
    TDATE = 323,                   /* TDATE  */
    TTIME = 324,                   /* TTIME  */
    TDATETIME = 325,               /* TDATETIME  */
    TTIMESTAMP = 326,              /* TTIMESTAMP  */
    TMEMO = 327,                   /* TMEMO  */
    TBLOB = 328,                   /* TBLOB  */
    TGRAPHIC = 329,                /* TGRAPHIC  */
    TVARIANT = 330,                /* TVARIANT  */
    ASSIGN = 331,                  /* ASSIGN  */
    DOTDOT = 332,                  /* DOTDOT  */
    NE = 333,                      /* NE  */
    LE = 334,                      /* LE  */
    GE = 335,                      /* GE  */
    EQ = 336,                      /* EQ  */
    LT = 337,                      /* LT  */
    GT = 338,                      /* GT  */
    PLUS = 339,                    /* PLUS  */
    MINUS = 340,                   /* MINUS  */
    STAR = 341,                    /* STAR  */
    SLASH = 342,                   /* SLASH  */
    UPARROW = 343,                 /* UPARROW  */
    AT = 344,                      /* AT  */
    DOT = 345,                     /* DOT  */
    COMMA = 346,                   /* COMMA  */
    COLON = 347,                   /* COLON  */
    SEMICOLON = 348,               /* SEMICOLON  */
    LPAREN = 349,                  /* LPAREN  */
    RPAREN = 350,                  /* RPAREN  */
    LBRACK = 351,                  /* LBRACK  */
    RBRACK = 352,                  /* RBRACK  */
    LBRACE = 353,                  /* LBRACE  */
    RBRACE = 354,                  /* RBRACE  */
    INTCONST = 355,                /* INTCONST  */
    LOGICALCONST = 356,            /* LOGICALCONST  */
    NUMBERCONST = 357,             /* NUMBERCONST  */
    STRINGCONST = 358,             /* STRINGCONST  */
    IDENT = 359                    /* IDENT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 20 "palgram.y"

	int ival;
	double dval;
	char *sval;
	struct symtab *symptr;
	struct tnode *typeptr;
	struct param_list *paramptr;
	struct field_list *fieldptr;

#line 178 "palgram.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_PALGRAM_TAB_H_INCLUDED  */
