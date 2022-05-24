%{ 

	int quoteMark = 0;

%}  

%% 

[Ee][Qq][Uu][Aa][Ll] {return 0;}
[Oo][Rr] {return 1;}
[Nn][Oo][Tt] {return 2;}
[Aa][Nn][Dd] {return 3;}
[Ff][Oo][Rr] {return 4;}
[Ii][Ff] {return 5;}
[Ll][Ee][Ss][Ss] {return 6;}
[Cc][Oo][Nn][Cc][Aa][Tt] {return 7;}
[Aa][Pp][Pp][Ee][Nn][Dd] {return 8;}
[Ss][Ee][Tt] {return 9;}
[Dd][Ee][Ff][Ff][Uu][Nn] {return 10;}
[Ll][Ii][Ss][Tt] {return 11;}
[Nn][Ii][Ll] {return 12;}
[Ee][Xx][Ii][Tt] {return 13;}
[Ll][Oo][Aa][Dd] {return 14;}
[Dd][Ii][Ss][Pp] {return 15;}
[Ff][Aa][Ll][Ss][Ee] {return 16;}
[Tt][Rr][Uu][Ee] {return 17;}

[*] {return 18;}
[/] {return 19;}
[-] {return 20;}
[+] {return 21;}
[(] { return 22;} 
[)] { return 23;}
[*][*] {return 24;}
["] {	if(quoteMark % 2 == 0){
			quoteMark++;
			return 25;	
		}
		else{
			quoteMark++;
			return 26;
		}  
	}
[,] {return 27;}
[;][;][.]* {return 28;}
[0]|[1-9][0-9]* {return 29;}
[A-Za-z_][a-zA-Z0-9_]* {return 30;}
[ \t]+   {return 31;} 
\n 	   {return 32;}
<<EOF>>		{return 33;}

[0-9]+[A-Za-z_]+ {return 34;}
[0][0-9]+ {return 34;}

. {return 34;}
%% 

void my_Lexer(FILE *fp2){
	char* nameOfTokens[35] = {
		"KW_EQUAL","KW_OR","KW_NOT","KW_AND","KW_FOR","KW_IF","KW_LESS","KW_CONCAT","KW_APPEND",
		"KW_SET","KW_DEFFUN","KW_LIST","KW_NIL","KW_EXIT","KW_LOAD","KW_DISP","KW_FALSE","KW_TRUE",
		"OP_MULT","OP_DIV","OP_MINUS","OP_PLUS","OP_OP","OP_CP","OP_DBLMULT","OP_OC","OP_CC","OP_COMMA",
		"COMMENT","VALUE","IDENTIFIER","Space","Newline","EOF","ERROR"
	}; 
	int number;
	int lastToken = 0;
	
	do{
		number = yylex();
		if(number < 31){
			fprintf(fp2, "%s\n",nameOfTokens[number]);	
			lastToken = number;
			if(number == 28){
				while(number != 32 && number != 33){
					number = yylex();
				}
			}
		}
		else if(number == 34){
			fprintf(fp2, "SYNTAX_ERROR %s cannot be tokenized\n",yytext);	
			
		}
	}
	while(number>=0 && number <33 || number == 34);
}

int yywrap(){} 

int main(int argc, char *argv[]){ 
  	FILE *fp2 = fopen("flex_Parser.txt","w");
	
	if(argc > 2){
		printf("Input is Wrong");
		return 0;
	}
	if(argc == 2){ // file mode
		FILE *fp;
		fp = fopen(argv[1],"r");
		
		if(fp == NULL){
			printf("There is no such a file");
			exit(0);
		}
		yyin = fp;
		my_Lexer(fp2);
		fclose(fp);
	}
	else{
		char convert[1024];
		while(1){
			printf("Enter an Input : ");
			int k = scanf("%[^\n]", convert);
			getchar();
			if(k == 0){
				fclose(fp2);
				return 0;	
			} 
			yy_scan_string(convert);
			my_Lexer(fp2);
		}
	}
	return 0; 
} 