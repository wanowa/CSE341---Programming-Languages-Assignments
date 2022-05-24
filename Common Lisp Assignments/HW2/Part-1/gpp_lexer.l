%{
#include "y.tab.h"
%}

%%

\+ { return OP_PLUS; }
\- { return OP_MINUS; }
\* { return OP_MULT; }
\/ { return OP_DIV; }
, { return OP_COMMA; }
\( { return OP_OP; }
\) { return OP_CP; }
\*\* { return OP_DBLMULT; }

true    {    return KW_TRUE;      }
false   {   return KW_FALSE;      }
append  {  return KW_APPEND;      }
concat  {  return KW_CONCAT;      }
not     {     return KW_NOT;      }
equal   {   return KW_EQUAL;      }
less    {    return KW_LESS;      }
nil     {     return KW_NIL;      }
list    {    return KW_LIST;      }
set     {     return KW_SET;      }
for     {     return KW_FOR;      }
if      {     return KW_IF;       }
and     {     return KW_AND;      }
or      {      return KW_OR;      }
exit    {    return KW_EXIT;      }
load    {    return KW_LOAD;      }
disp    {    return KW_DISP;      }

;;(.*) { return COMMENT; }

(0)|([1-9][0-9]*) {yylval.operand = atoi(yytext); return VALUE;}

[a-zA-Z][0-9a-zA-Z]* {strcpy(yylval.expr, yytext); return IDENTIFIER;}
[\n\t ] {} /*ignore*/

. {return 0;}
([a-zA-Z]*[,+-/*])|([0-9][0-9a-zA-Z]+) { return 0;}
[+][0-9A-zA-Z]* {return 0;}
[-][0-9A-zA-Z]*	{return 0;}
[/][0-9A-zA-Z]* {return 0;}
[*][0-9A-zA-Z]* { return 0;}
[,][0-9A-zA-Z]* { return 0;}

%%

int yywrap(){
  return 1;
}