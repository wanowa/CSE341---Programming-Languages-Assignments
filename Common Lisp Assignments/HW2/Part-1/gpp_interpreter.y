%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

%}

%type <operand> EXPI
%type <operand> EXPB
%type <operands> VALUES
%type <operands> EXPLIST
%type <operands> ELEM

%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_OP
%token OP_CP
%token OP_DBLMULT
%token OP_OC
%token OP_CC
%token OP_COMMA
%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_LESS
%token KW_NIL
%token KW_LIST
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_DEFFUN
%token KW_FOR
%token KW_IF
%token KW_EXIT
%token KW_LOAD
%token KW_DISP
%token KW_TRUE
%token KW_FALSE
%token COMMENT
%token <operand> VALUE
%token <expr> IDENTIFIER

%{

FILE *stream;
extern FILE *yyin;
void printList(int *list);

%}

%{

int yylex();
int yyerror(char *error);

typedef struct myStruct
{
    char str[100];
    int variable;
    struct myStruct *next;

}structVar;

structVar *root= NULL;
structVar *var = NULL;

int yyerror(char *error) {
    printf("SYNTAX ERROR\n");
}


int* combineLists(int *ls1, int *ls2){
    return ls2;
}


int* addElem(int *ls, int element){
    structVar *iter;
    iter = var;
    if (var==NULL) {
        var=(structVar *)malloc(sizeof(structVar));
        var->variable=element;
        var->next=NULL;
        ls = (int *)malloc(sizeof(int)*2);
        ls[0] = element;
        ls[1] = -1;
    }
    else{
        int size = 0;
        while(iter->next != NULL){
            size++;
            iter = iter->next;
        }
        size++;
        structVar *var1 = (structVar*)malloc(sizeof(structVar));
        var1->variable = element;
        var1->next = iter->next;
        iter->next=var1;
        ls = (int*)(malloc(sizeof(int)*(size+2)));
        int i = 0;
        iter = var;
        while(iter != NULL){
            ls[i] = iter->variable;
            i++;
            iter = iter->next;
        }
        ls[i] = -1;
    }
    return ls;
}

int pow_func(int base, int power){
    int i,temp;
    temp = base;

    if(power == 0){
        return 1;
    }
    else if(power == 1){
        return base;
    }
    else{
        for(i = 1 ; i < power ; i++){
            base = base * temp;
        }
        return base;    
    }
}

int getVar(char str[100]){
    structVar *iter;
    iter=root;
    while(iter != NULL){
        if(strcmp(iter->str, str) == 0){
            return iter->variable;
        }
        iter=iter->next;
    }
    return 404;
}


void addVar(char str[100], int variable){
    structVar *iter;
    iter=root;

    if(root==NULL) {
        root=(structVar*)malloc(sizeof(structVar));
        root->variable=variable;
        strcpy(root->str, str);
        root->next=NULL;
    }

    else{
        while(iter->next!=NULL){
            iter=iter->next;
        }
        structVar *var1=(structVar *)malloc(sizeof(structVar));
        var1->variable=variable;
        strcpy(var1->str, str);
        var1->next=iter->next;
        iter->next =var1;
    }
}


%}
%union{
int operand;
int *operands;
char expr[30];
}
%start START
%%
START:
    EXPI {printf("SYNTAX OK. \nResult: %d\n\n", $1);}    |    EXPLIST {printf("SYNTAX OK. \nResult: ");printList($1);}
    /*|error { yyerrok; yyclearin;}*/    ;

START:
    START OP_OP KW_EXIT OP_CP { printf("Terminated....\n"); exit(-1); };
    |
    START EXPLIST {printf("SYNTAX OK. \nResult:"); printList($2); }    |    START EXPB {printf("SYNTAX OK. \nResult: %s\n\n", $2 == 1 ? "T" : "NIL");}
    |
    EXPB {printf("SYNTAX OK. \nResult: %s\n\n", $1 == 1 ? "T" : "NIL");}    |    START EXPI {printf("SYNTAX OK. \nResult: %d\n\n", $2);}
    |
    COMMENT {}    |    START COMMENT {}    |    OP_OP KW_EXIT OP_CP { printf("Terminated...\n"); exit(-1);}
    

EXPI:
    /* (+ EXPI EXPI) G++ Syntax*/    OP_OP OP_PLUS EXPI EXPI OP_CP  {$$=$3+$4;}
    |
    /* (- EXPI EXPI) */    OP_OP OP_MINUS EXPI EXPI OP_CP {$$=$3-$4;}
    |
    /* (* EXPI EXPI) */    OP_OP OP_MULT EXPI EXPI OP_CP  {$$=$3*$4;}
    |
    /* (/ EXPI EXPI) */    OP_OP OP_DIV EXPI EXPI OP_CP   {$$=$3/$4;}
    |
    OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = pow_func($3, $4);}
    /*
    |
    OP_OP KW_FOR EXPB EXPI OP_CP { $$ = (1 == $3) ? $4 : 0; }    |    OP_OP KW_IF EXPB EXPI EXPI OP_CP {$$ = (1 == $3) ? $4: $5;}
    |
    OP_OP KW_DISP EXPI OP_CP { $$ = $3; printf("Display : %d\n", $3);};
    |
    OP_OP KW_SET IDENTIFIER EXPI OP_CP {$$ = $4; addVar($3, $4);}    |    OP_OP KW_IF EXPB EXPI OP_CP {$$ = (1 == $3) ? $4: 0;}
    |
    IDENTIFIER {$$ = getVar($1);}*/    |    VALUE {$$ = $1;}


EXPB:
    KW_TRUE  { $$ = 1; }    |    KW_FALSE   { $$ = 0; }
    |
    OP_OP KW_AND EXPB EXPB OP_CP {$$ = $3 && $4;}    |    OP_OP KW_OR EXPB EXPB OP_CP  {$$ = $3 || $4;}
    |
    OP_OP KW_NOT EXPB OP_CP  {$$ = ! ($3);}    |    OP_OP KW_EQUAL EXPB EXPB OP_CP {$$ = ($3 == $4);}
    |
    OP_OP KW_EQUAL EXPI EXPI OP_CP {$$ = ($3 == $4);}    |    OP_OP KW_LESS EXPI EXPI OP_CP { $$ = $3 < $4; }
    |
    OP_OP KW_DISP EXPB OP_CP { $$ = $3; printf("Display : %s\n", ($3 ? "T":"NIL"));};

EXPLIST:
    OP_OP KW_CONCAT EXPLIST EXPLIST OP_CP {$$ = combineLists($3, $4);}    |    OP_OP KW_APPEND EXPI EXPLIST OP_CP {$$ = addElem($4, $3);}
    |
    OP_OP KW_LIST VALUES OP_CP {$$ = $3;}    |    ELEM  {$$ = $1;}
    |
    OP_OP KW_DISP ELEM OP_CP { $$ = $3; printf("Display : "); printList($3);};

ELEM:
    OP_OP VALUES OP_CP {$$ = $2;}    |    OP_OP OP_CP { $$= NULL; }    |    KW_NIL { $$ = NULL;};

VALUES:
    VALUES VALUE  {$$ = addElem($1, $2);}    |    VALUE {$$ = NULL; $$ = addElem($$, $1);};

%%

void printList(int *ls){
    printf("( ");

    for(int i=0;ls[i]!=-1; ++i){
        printf("%d ", ls[i]);
    }

    printf(")\n");
}


int main(){
   	printf("\nTo terminate program, enter an empty string\n");
   	printf("Program will print outputs to terminal when program is terminated.n");
    char* newLine = NULL;
    char* line = (char*)malloc(100*sizeof(char));
    int isEmpty;
    size_t size = 0;

    while(isEmpty != 1){
        printf("\nEnter an Input-->");
        isEmpty = getline(&newLine, &size, stdin);
        if(isEmpty != 1){
            line = (char *) realloc(line, (strlen(line)+size+5)*sizeof(char));
            strcat(line,newLine);
        }    
    }

    stream = fmemopen (line, strlen (line) - 1, "r");
    yyin = stream;
    yyparse();
    exit(-1);
}
