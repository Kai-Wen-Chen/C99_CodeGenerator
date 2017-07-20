%{

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "y.tab.h"
#include "SymbolTable.h"

	int lineCount = 0;
	bool pragToken = true;
	bool pragSource = true;
	
	char token[1000];
	
	char* copys();
	extern FILE *f_asm;
%}

%x COMMENT
%x DIGITAL
%x DELAY

FUNCTION ("remove"|"rename"|"tmpfile"|"tmpnam"|"fclose"|"fflush"|"fopen"|"freopen"|"setbuf"|"setvbuf"|"fprintf"|"fscanf"|"printf"|"scanf"|"sprintf"|"sscanf"|"vfprintf"|"vprintf"|"vsprintf"|"fgetc"|"fgets"|"fputc"|"fputs"|"getc"|"getchar"|"gets"|"putc"|"putchar"|"puts"|"ungetc"|"fread"|"fwrite"|"fgetpos"|"fseek"|"fsetpos"|"ftell"|"rewind"|"clearerr"|"feof"|"ferror"|"perror")

INT int
VOID void
DOUBLE double
FLOAT float
BOOLEAN bool
CHAR char
STRING string
DATATYPE ({INT}|{VOID}|{DOUBLE}|{FLOAT}|{BOOLEAN}|{CHAR}|{STRING})

NULL null
FOR for
WHILE while
DO do
IF if
ELSE else
SWITCH switch
RETURN return
BREAK break
CONTINUE continue
CONST const
TRUE true
FALSE false
STRUCT struct
CASE case
DEFAULT default
KEY ({NULL}|{FOR}|{WHILE}|{DO}|{IF}|{ELSE}|{SWITCH}|{RETURN}|{BREAK}|{CONTINUE}|{CONST}|{TRUE}|{FALSE}|{STRUCT}|{CASE}|{DEFAULT})
KEYWORD ({DATATYPE}|{KEY})

ID ({chars}(({chars}|{number})*))

OP [\+\-\*\/\=\%]
EQ "=="
GTE ">="
LTE "<="
GT ">"
LT "<"
AND "&&"
OR "||"
PO "++"
SO "--"
PE "+="
SE "-="
NOT "!"
NOTEQ "!="
REF "&"
OPERATOR ({OP}|{GTE}|{LTE}|{EQ}|{GT}|{LT}|{AND}|{OR}|{PO}|{SO}|{PE}|{SE}|{NOT}|{NOTEQ}|{REF})

PUNC [:;,.\(\)\[\]\{\}?]

number [0-9]
numbers ({number}+)
F ("F"|"f")
L ("l"|"L"|"ll"|"LL")
U ("u"|"U")
ITAIL ({L}|{U}|{L}{U}|{U}{L})
FTAIL ({F}|"L"|"l")
decimal (({numbers}?("."){numbers})|({numbers}(".")))
integer (([0]|([1-9][0-9]*)){ITAIL}?)
doubledecimal ({decimal}({FTAIL}?))

sci ("E"|"e")
SCI (({decimal}|{integer})({sci})(((\-)?){numbers})({FTAIL}?))

chars [A-Za-z\_]
character (({L}?)(\')([^\'\\\n]|(\n|\t))(\'))
str (({L}?)(\")(((\\n)|(\\t))|[^"\\\n])*(\"))

space [ \t\r]
SPACE {space}+

PRAGMA "pragma"
TOKEN "token"
SOURCE "source"
ON "on"
OFF "off"

PRAGSOURCEON (("#")({SPACE}?)({PRAGMA})({SPACE})({SOURCE})({SPACE})({ON})({SPACE}?))
PRAGSOURCEOFF (("#")({SPACE}?)({PRAGMA})({SPACE})({SOURCE})({SPACE})({OFF})({SPACE}?))
PRAGTOKENON (("#")({SPACE}?)({PRAGMA})({SPACE})({TOKEN})({SPACE})({ON})({SPACE}?))
PRAGTOKENOFF (("#")({SPACE}?)({PRAGMA})({SPACE})({TOKEN})({SPACE})({OFF})({SPACE}?))

%%

{PRAGSOURCEON} {
	pragSource = true;
	
	strcat(token, yytext);
}

{PRAGSOURCEOFF} {
	pragSource = false;
	
	strcat(token, yytext);
}

{PRAGTOKENON} {
	pragToken = true;
	
	strcat(token, yytext);
} 

{PRAGTOKENOFF} {
	pragToken = false;
	
	strcat(token, yytext);
}

"/*" {
	BEGIN COMMENT;
	
	strcat(token, yytext);
} 

"//".* { strcat(token, yytext); }

<COMMENT>"*/" {
	BEGIN INITIAL;
	
	strcat(token, yytext);
}

<COMMENT>"\n" {
	++lineCount;

	if (pragSource == true) printf("%d:%s\n", lineCount, token);
	
	token[0] = '\0';
}

<COMMENT>. { strcat(token, yytext); }

"digitalWrite(" {
	strcat(token, yytext);
	BEGIN DIGITAL;
}

<DIGITAL>"LOW" {
	strcat(token, yytext);
	fprintf(f_asm, " movi $r1, 0\n");
}

<DIGITAL>"HIGH" {
	strcat(token, yytext);
	fprintf(f_asm, " movi $r1, 1\n");
}

<DIGITAL>{SPACE} { strcat(token, yytext); }

<DIGITAL>"," { strcat(token, yytext); }

<DIGITAL>")" { strcat(token, yytext); }

<DIGITAL>{ID} {
	int idx = look_up_symbol(yytext);
	
	switch(table[idx].mode){
		case ARGUMENT_MODE:
			fprintf(f_asm, " lwi $r0, [$sp+%d]\n", table[idx].offset*4);
			break;
		case LOCAL_MODE:
			fprintf(f_asm, " lwi $r0, [$sp+%d]\n", table[idx].offset*4);
			break;
	}
	
	strcat(token, yytext);
}

<DIGITAL>{integer} {
    int num = atoi(yytext);
    fprintf(f_asm, " movi $r0, %d\n",num);
    strcat(token, yytext);
}

<DIGITAL>";" {
	fprintf(f_asm, " bal digitalWrite\n");
	strcat(token, yytext);
	BEGIN INITIAL;
}

"delay(" {
	strcat(token, yytext);
	BEGIN DELAY;
} 

<DELAY>")" { strcat(token, yytext); }

<DELAY>{ID} {
	int idx = look_up_symbol(yytext);
	
	switch(table[idx].mode){
		case ARGUMENT_MODE:
			fprintf(f_asm, " lwi $r0, [$sp+%d]\n", table[idx].offset*4);
			break;
		case LOCAL_MODE:
			fprintf(f_asm, " lwi $r0, [$sp+%d]\n", table[idx].offset*4);
			break;
	}
	
	strcat(token, yytext);
}

<DELAY>{integer} {
    int num = atoi(yytext);
    fprintf(f_asm, " movi $r0, %d\n",num);
    strcat(token, yytext);
}

<DELAY>";" {
	fprintf(f_asm, " bal delay\n");
	strcat(token, yytext);
	BEGIN INITIAL;
}

{FUNCTION} {
	if (pragToken == true) printf("#key:%s\n", yytext);
	
	strcat(token, yytext);
	
	yylval.ident = copys(yytext);
	
	return ID;
}

{DATATYPE} {
	if (pragToken == true) printf("#key:%s\n", yytext);
	
	strcat(token, yytext);
	
	if (strcmp(yytext, "int") == 0) return INT;
	if (strcmp(yytext, "void") == 0) return VOID;
	if (strcmp(yytext, "double") == 0) return DOUBLE;
	if (strcmp(yytext, "float") == 0) return FLOAT;
	if (strcmp(yytext, "bool") == 0) return BOOL;
	if (strcmp(yytext, "char") == 0) return CHAR;
}

{KEY} {
	if (pragToken == true) printf("#key:%s\n", yytext);
	
	strcat(token, yytext);
	
	if (strcmp(yytext, "null") == 0) return NUL;
	if (strcmp(yytext, "for") == 0) return FOR;
	if (strcmp(yytext, "while") == 0) return WHILE;
	if (strcmp(yytext, "do") == 0) return DO;
	if (strcmp(yytext, "if") == 0) return IF;
	if (strcmp(yytext, "else") == 0) return ELSE;
	if (strcmp(yytext, "switch") == 0) return SWITCH;
	if (strcmp(yytext, "return") == 0) return RETURN;
	if (strcmp(yytext, "break") == 0) return BREAK;
	if (strcmp(yytext, "continue") == 0) return CONTINUE;
	if (strcmp(yytext, "const") == 0) return CONST;
	if (strcmp(yytext, "true") == 0) return TRUE;
	if (strcmp(yytext, "false") == 0) return FALSE;
	if (strcmp(yytext, "case") == 0) return CASE;
	if (strcmp(yytext, "default") == 0) return DEFAULT;
}

{ID} {
	if (pragToken == true) printf("#id:%s\n", yytext);
	
	strcat(token, yytext);
	
	yylval.ident = copys(yytext);
	
	return ID;
}

{OPERATOR} {
	if (pragToken == true) printf("#op:%s\n", yytext);
	
	strcat(token, yytext);
	
	if (strcmp(yytext, "+") == 0) return PLUS;
	if (strcmp(yytext, "-") == 0) return SUB;
	if (strcmp(yytext, "*") == 0) return MUL;
	if (strcmp(yytext, "/") == 0) return DIV;
	if (strcmp(yytext, "%") == 0) return MOD;
	if (strcmp(yytext, "=") == 0) return '=';
	if (strcmp(yytext, "==") == 0) return EQ;
	if (strcmp(yytext, ">=") == 0) return GTE;
	if (strcmp(yytext, "<=") == 0) return LTE;
	if (strcmp(yytext, ">") == 0) return GT;
	if (strcmp(yytext, "<") == 0) return LT;
	if (strcmp(yytext, "&&") == 0) return AND;
	if (strcmp(yytext, "||") == 0) return OR;
	if (strcmp(yytext, "++") == 0) return PO;
	if (strcmp(yytext, "--") == 0) return SO;
	if (strcmp(yytext, "!") == 0) return NOT;
	if (strcmp(yytext, "!=") == 0) return NOTEQ;
	if (strcmp(yytext, "&") == 0) return '&';
}

{PUNC} {
	if (pragToken == true) printf("#punc:%s\n", yytext);
	
	strcat(token, yytext);
	
	if (strcmp(yytext, ":") == 0) return ':';
	if (strcmp(yytext, ";") == 0) return ';';
	if (strcmp(yytext, ",") == 0) return ',';
	if (strcmp(yytext, ".") == 0) return '.';
	if (strcmp(yytext, "(") == 0) return '(';
	if (strcmp(yytext, ")") == 0) return ')';
	if (strcmp(yytext, "[") == 0) return '[';
	if (strcmp(yytext, "]") == 0) return ']';
	if (strcmp(yytext, "{") == 0) return '{';
	if (strcmp(yytext, "}") == 0) return '}';
	if (strcmp(yytext, "?") == 0) return '?';
}

{integer} {
	if (pragToken == true) printf("#integer:%s\n", yytext);

	strcat(token, yytext);
	
	yylval.intVal = atoi(yytext);
	
	return INTEGER;
}

{doubledecimal} {
	if (pragToken == true) printf("#double:%s\n", yytext);
	
	strcat(token, yytext);
	
	sscanf(yytext, "%lf", &yylval.doubleVal);
	
	return DBLE;
}

{character} {
	char tempStr[500];
	
	if (pragToken == true) printf("#char:%s\n", yytext);
	
	strcat(token, yytext);
	
	strcpy(tempStr, yytext+1);
	tempStr[yyleng-2] = '\0';
	
	yylval.ident = copys(tempStr);
	
	return CHR;
}

{SCI} {
	if (pragToken == true) printf("#sci:%s\n", yytext);
	
	strcat(token, yytext);
	
	sscanf(yytext, "%lf", &yylval.doubleVal);
	
	return DBLE;
}

{str} {
	char tmpStr[500];
	if (yytext[0] == 'L'){
		strcpy(tmpStr, yytext+2);
		int len = strlen(tmpStr);
		tmpStr[len-1] = '\0';
	}
	else{
		strcpy(tmpStr, yytext+1);
		int len = strlen(tmpStr);
		tmpStr[len-1] = '\0';
	}
	
	if (pragToken == true) printf("#string:%s\n", tmpStr);
	
	strcat(token, yytext);
	
	yylval.ident = copys(tmpStr);
	return STR;
}

{SPACE} { strcat(token, yytext); }

\n {
	++lineCount;
	
	if (pragSource == true) printf("%d:%s\n", lineCount, token);
	token[0] = '\0';
} 

[0]{integer} {
	fprintf(stderr, "Error at line %d: %s\n", ++lineCount, yytext);
	exit(1);
}

. {
	fprintf(stderr, "Error at line %d: %s\n", ++lineCount, yytext);
	exit(1);
	
	strcat(token, yytext);
}

%%

/*int yywrap(){
	return 1;
}*/
