/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    OP_PLUS = 258,
    OP_MINUS = 259,
    OP_DIV = 260,
    OP_MULT = 261,
    OP_OP = 262,
    OP_CP = 263,
    OP_DBLMULT = 264,
    OP_OC = 265,
    OP_CC = 266,
    OP_COMMA = 267,
    KW_AND = 268,
    KW_OR = 269,
    KW_NOT = 270,
    KW_EQUAL = 271,
    KW_LESS = 272,
    KW_NIL = 273,
    KW_LIST = 274,
    KW_APPEND = 275,
    KW_CONCAT = 276,
    KW_SET = 277,
    KW_DEFFUN = 278,
    KW_FOR = 279,
    KW_IF = 280,
    KW_EXIT = 281,
    KW_LOAD = 282,
    KW_DISP = 283,
    KW_TRUE = 284,
    KW_FALSE = 285,
    COMMENT = 286,
    VALUE = 287,
    IDENTIFIER = 288
  };
#endif
/* Tokens.  */
#define OP_PLUS 258
#define OP_MINUS 259
#define OP_DIV 260
#define OP_MULT 261
#define OP_OP 262
#define OP_CP 263
#define OP_DBLMULT 264
#define OP_OC 265
#define OP_CC 266
#define OP_COMMA 267
#define KW_AND 268
#define KW_OR 269
#define KW_NOT 270
#define KW_EQUAL 271
#define KW_LESS 272
#define KW_NIL 273
#define KW_LIST 274
#define KW_APPEND 275
#define KW_CONCAT 276
#define KW_SET 277
#define KW_DEFFUN 278
#define KW_FOR 279
#define KW_IF 280
#define KW_EXIT 281
#define KW_LOAD 282
#define KW_DISP 283
#define KW_TRUE 284
#define KW_FALSE 285
#define COMMENT 286
#define VALUE 287
#define IDENTIFIER 288

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 172 "gpp_interpreter.y" /* yacc.c:1909  */

int operand;
int *operands;
char expr[30];

#line 126 "y.tab.h" /* yacc.c:1909  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
