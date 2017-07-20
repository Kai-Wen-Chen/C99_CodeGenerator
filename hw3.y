%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "SymbolTable.h"

    #define Int 1000
    #define Double 1001
    #define Char 1010
    #define String 1011
    #define Bool 1100
    #define Void 1101
    
    extern int lineCount;
    extern char token[1000];
    extern char *yytext;
    int Funct_Count = 0;
    int locOffset = 0;
    int label_Count = 2;
    FILE *f_asm;
%}

%start StartProduction

%token <ident> ID

%token <token> INT VOID DOUBLE FLOAT BOOL CHAR
%token <intVal> INTEGER 
%token DBLE CHR STR
%token <token> NUL FOR WHILE DO IF ELSE SWITCH 
%token <token> RETURN BREAK CONTINUE CONST
%token <token> TRUE FALSE CASE DEFAULT
%token <charVal> ';' ':' '?' '=' '(' ')' '[' ']' '{' '}' '&'

%left <token> PLUS SUB MUL DIV MOD
%left <token> EQ GTE LTE GT LT NOT NOTEQ
%left <token> AND OR 
%left <token> PO SO 

%type <ident> Array Funct_Declaration Funct_Type Declaration ID_or_Array
%type <ident> Final_Expr Init_Final_Expr
%type <ident> Initial_Stmts Initial_Stmt
%type <token> Datatype
%type <token> Expression PO_or_SO Init_PO_or_SO Init_Expr Logic Init_Logic
%type <token> Unary_Expr MUL_DIV_MOD Init_Unary_Expr Init_MUL_DIV_MOD
%type <token> PLUS_SUB Init_PLUS_SUB Relation_Expr Init_Relation_Expr
%type <token> Logic Init_Logic Logic_Expr Init_Logic_Expr Relation Init_Relation

%union {
         int token;
         int intVal;
         double doubleVal;
         char charVal;
         char *ident;
}

%%

StartProduction: Code
               | StartProduction Code
               ;

Code: Declaration { }
    | Funct_Definition { Funct_Count++; }
    ;

Declarations: Declarations Declaration
            | 
            ; 
          
Declaration: Const_Declaration { $$ = NULL; } 
           | Simple_Declaration { $$ = NULL; }
           | Funct_Declaration { $$ = $1; }
           ;

Const_Declaration: CONST Datatype Const_Stmts ';' ;

Const_Stmts: Const_Stmt
           | Const_Stmts ',' Const_Stmt
           ;

Const_Stmt: ID '=' INTEGER
          {
		install_symbol($1);
		int idx = look_up_symbol($1);

		table[idx].offset = locOffset++;
		table[idx].mode = LOCAL_MODE;

		PopReg(0);

		fprintf(f_asm, " swi $r0, [$sp+%d]\n", table[idx].offset*4);
          }
          | ID '=' DBLE { install_symbol($1); }
          | ID '=' CHR { install_symbol($1); }
          | ID '=' STR { install_symbol($1); }
          | ID '=' TRUE { install_symbol($1); }
          | ID '=' FALSE { install_symbol($1); }
          ;

Simple_Declaration: Datatype Initial_Stmts ';' 
 		    {
 			int idx = look_up_symbol($2);
			
			if ($1 == Int) table[idx].type = Int;
			else if ($1 == Double) table[idx].type = Double;
 			else if ($1 == Char) table[idx].type = Char;
			else if ($1 == Bool) table[idx].type = Bool;
		    } 
                    ;

Initial_Stmts: Initial_Stmt { $$ = $1; }
             | Initial_Stmts ',' Initial_Stmt { $$ = $1; }
             ;

Initial_Stmt: ID { install_symbol($1); $$ = $1; }
            | Array { install_symbol($1); $$ = $1; }
            | ID '=' Init_Expr
	    {
		install_symbol($1);
		$$ = $1;
		
		int idx = look_up_symbol($1);
		table[idx].offset = locOffset++;
		table[idx].mode = LOCAL_MODE;

		PopReg(0);

		fprintf(f_asm, " swi $r0, [$sp+%d]\n", table[idx].offset*4);
	    }
            | Array '=' Array_Content { install_symbol($1); }
            ;

Funct_Definition: Funct_Type '{'
		  {
			cur_scope++;
                        set_scope_and_offset_of_param($1);
			locOffset = 0;
		  }
                  Declarations { set_local_vars($1); }
            	  Stmts '}' { cur_scope--; } ;

Funct_Declaration: Funct_Type ';' ;

Funct_Type: Datatype ID { install_symbol($2); } 
            '(' Params ')'
            {
		int idx = look_up_symbol($2);

		if ($1 == Int) table[idx].type = Int;
		if ($1 == Double) table[idx].type = Double;
   		if ($1 == Char) table[idx].type = Char;
		if ($1 == Bool) table[idx].type = Bool;

		$$ = $2;
	    }
            | VOID ID { install_symbol($2); }
             '(' Params ')'
              {
 			int idx = look_up_symbol($2);
			table[idx].type = Void;
    			$$ = $2;
	      }
	    ;

Datatype: INT { $$ = Int; }
        | DOUBLE { $$ = Double; }
        | BOOL { $$ = Bool; }
        | CHAR { $$ = Char; }
        ;

Params: Param
      | Params ',' Param 
      ;

Param: Datatype ID { install_symbol($2); }
     | Datatype Array { install_symbol($2); }
     | 
     ;

Array: ID Array_Declaration { $$ = $1; } 
     ;

Array_Declaration: '[' INTEGER ']'
                 | Array_Declaration '[' INTEGER ']'
                 ;

Array_Content: '{' '}'
             | '{' Init_Exprs '}'
             ;

Array_Expr: '[' Expression ']'
          |  Array_Expr '[' Expression ']'
          ;

Stmts: 
     | Stmts Stmt
     ;

Stmt: Simple_Stmt
    | IF_ELSE
    | Switch_Stmt
    | Loop_Stmt
    | Terminal
    ;

Simple_Stmt: ID_or_Array '=' Expression ';'
	   { 
	   	int idx = look_up_symbol($1);
		PopReg(0);
		fprintf(f_asm, " swi $r0, [$sp+%d]\n", table[idx].offset*4);
	   }
           ;

IF_ELSE: IF '(' Expression ')'
       {
		PopReg(0);

		fprintf(f_asm, " beqz $r0, .L%d\n", label_Count);
       } Compound_Stmt Else_Tail
       ;

Else_Tail: ELSE 
         {
		fprintf(f_asm, " j .L%d\n", label_Count+1);
		fprintf(f_asm, ".L%d: \n", label_Count);
	 }
	 Compound_Stmt 
	 {
		fprintf(f_asm, ".L%d: \n", label_Count+1);
		label_Count += 2;
	 }
	 |  
	 { 
		fprintf(f_asm, ".L%d: \n", label_Count);
	    	label_Count++;
  	 };


Compound_Stmt: '{'
		{ cur_scope++; }
	 	Declarations { }
 		Stmts 
		'}' { cur_scope--; } ;

Switch_Stmt: SWITCH '(' ID ')' '{' Switch_Compound '}'
           ;

Switch_Compound: Cases
               | Cases Default
               ;

Cases: Case_Content
     | Cases Case_Content
     ;

Case_Content: CASE INTEGER ':' Stmts
            | CASE CHR ':' Stmts
            ;

Default: DEFAULT ':' Stmts
       ;

Loop_Stmt: WHILE { fprintf(f_asm, ".L%d: \n", label_Count); } 
         '(' Expression ')' 
         {
		PopReg(0);
		fprintf(f_asm, " beqz $r0, .L%d\n", label_Count+1);
         }
         Compound_Stmt
         {
		fprintf(f_asm, " j .L%d\n", label_Count);
	
		fprintf(f_asm, ".L%d: \n", label_Count+1);
		label_Count += 2;
         }
         | DO { fprintf(f_asm, ".L%d: \n", label_Count); }
         Compound_Stmt WHILE '(' Expression ')' ';'
         {
		PopReg(0);
		fprintf(f_asm, " bnez $r0, .L%d\n", label_Count);
		label_Count++;
         }
         | FOR For_Stmt Compound_Stmt
         ;

For_Stmt: '(' Expr_Stmt Expr_Stmt For_last ')' ;

For_last: Expression
        |
        ;

Expr_Stmt: Expression ';'
         | ID '=' Expression ';'
         | ';'   
         ;

Terminal: BREAK ';'
        | CONTINUE ';'
        | RETURN ';'
        | RETURN Expression ';'
        ;

Expression: Logic
          ;

Logic: Logic_Expr
     | Logic OR Logic_Expr
     {
	PopReg(0);
	PopReg(1);
	fprintf(f_asm, " or $r0, $r0, $r1\n");
	PushReg(0);
     }
     ;

Logic_Expr: Relation
          | Logic_Expr AND Relation
          {
		PopReg(0);
		PopReg(1);
		fprintf(f_asm, " and $r0, $r0, $r1\n");
		PushReg(0);
          }
          ;

Relation: Relation_Expr
        | NOT Relation_Expr
        {
		PopReg(0);
		fprintf(f_asm, " addi $r0, $r0, 0\n");
		fprintf(f_asm, " slti $r0, $r0, 1\n");
		fprintf(f_asm, " zeb $r0, $r0\n");
		PushReg(0);
        }
        ;

Relation_Expr: PLUS_SUB
             | Relation_Expr EQ PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " xor $r0, $r0, $r1\n");
			fprintf(f_asm, " slti $r0, $r0, 1\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Relation_Expr GTE PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " slts $r0, $r1, $r0\n");
			fprintf(f_asm, " xori $r0, $r0, 1\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Relation_Expr LTE PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " slts $r0, $r0, $r1\n");
			fprintf(f_asm, " xori $r0, $r0, 1\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Relation_Expr GT PLUS_SUB
	     {  
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " slts $r0, $r0, $r1\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Relation_Expr LT PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " slts $r0, $r1, $r0\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Relation_Expr NOTEQ PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " xor $r0, $r0, $r1\n");
			fprintf(f_asm, " movi $r1, 0\n");
			fprintf(f_asm, " slt $r0, $r1, $r0\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             ;

PLUS_SUB: MUL_DIV_MOD { $$ = $1; }
        | PLUS_SUB PLUS MUL_DIV_MOD
        {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;
	
		PopReg(0);
		PopReg(1);
		fprintf(f_asm, " add $r0, $r1, $r0\n");
		PushReg(0);
        }
        | PLUS_SUB SUB MUL_DIV_MOD
        {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;

		PopReg(0);
		PopReg(1);
		fprintf(f_asm, " sub $r0, $r1, $r0\n");
		PushReg(0);
        }
        ;

MUL_DIV_MOD: Unary_Expr { $$ = $1; }
           | MUL_DIV_MOD MUL Unary_Expr
           {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;

		PopReg(0);
		PopReg(1);
		fprintf(f_asm, " mul $r0, $r1, $r0\n");
		PushReg(0);
           }
           | MUL_DIV_MOD DIV Unary_Expr
           {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;

		PopReg(2);
		PopReg(3);
		fprintf(f_asm, " divsr $r0, $r1, $r3, $r2\n");
		PushReg(0);
           }
           | MUL_DIV_MOD MOD Unary_Expr
           {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;

		PopReg(2);
		PopReg(3);
		fprintf(f_asm, " divsr $r0, $r1, $r3, $r2\n");
		PushReg(1);
           }
           ;
 
Unary_Expr: PO_or_SO { $$ = $1; }
          | SUB PO_or_SO
          {
		PopReg(0);
		fprintf(f_asm, " subri $r0, $r0, 0\n");
		PushReg(0);

		$$ = $2;
          }
          ;

PO_or_SO: Final_Expr { $$ = $1; }
        | Final_Expr PO { $$ = $1; }
        | Final_Expr SO { $$ = $1; }
        ;

Final_Expr: INTEGER
          {
	  	fprintf(f_asm, " movi $r0, %d\n",$1);
	 	PushReg(0);

	  	$$ = Int;
          }
          | ID_or_Array
          {
	  	int idx = look_up_symbol($1);
	
	 	switch(table[idx].mode){
			case ARGUMENT_MODE:
				fprintf(f_asm, " lwi $r0, [$sp+%d]\n",table[idx].offset*4);
				break;
			case LOCAL_MODE:
				fprintf(f_asm, " lwi $r0, [$sp+%d]\n",table[idx].offset*4);
				break;
          	}
          	
 	 	PushReg(0);
	
	  	$$ = table[idx].type;
          }
	  | DBLE { $$ = Double; }
	  | CHR { $$ = Char; }
 	  | STR { $$ = String; }
          | TRUE { $$ = Bool; }
          | FALSE { $$ = Bool; }
          | '(' Expression ')' { $$ = $2; }
          | ID '(' Expressions ')'
          {
		int idx = look_up_symbol($1);

		$$ = table[idx].type;
          }
          | ID '(' ')'
          {
		int idx = look_up_symbol($1);
	
		$$ = table[idx].type;
          }
          ;

ID_or_Array: ID { $$ = $1; }
           | ID Array_Expr { $$ = $1; }
           ;

Expressions: Expression
           | Expressions ',' Expression
           ;

Init_Exprs: Init_Exprs ',' Init_Expr
               | Init_Expr
               ;

Init_Expr: Init_Logic ;

Init_Logic: Init_Logic_Expr
     | Init_Logic OR Init_Logic_Expr
     {
	PopReg(0);
	PopReg(1);
	fprintf(f_asm, " or $r0, $r0, $r1\n");
	PushReg(0);
     }
     ;

Init_Logic_Expr: Init_Relation
          | Init_Logic_Expr AND Init_Relation
          {
		PopReg(0);
		PopReg(1);
		fprintf(f_asm, " and $r0, $r0, $r1\n");
		PushReg(0);
          }
          ;

Init_Relation: Init_Relation_Expr
        | NOT Init_Relation_Expr
        {
		PopReg(0);
		fprintf(f_asm, " addi $r0, $r0, 0\n");
		fprintf(f_asm, " slti $r0, $r0, 1\n");
		fprintf(f_asm, " zeb $r0, $r0\n");
		PushReg(0);
        }
        ;

Init_Relation_Expr: Init_PLUS_SUB
             | Init_Relation_Expr EQ Init_PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " xor $r0, $r0, $r1\n");
			fprintf(f_asm, " slti $r0, $r0, 1\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Init_Relation_Expr GTE Init_PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " slts $r0, $r1, $r0\n");
			fprintf(f_asm, " xori $r0, $r0, 1\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Init_Relation_Expr LTE Init_PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " slts $r0, $r0, $r1\n");
			fprintf(f_asm, " xori $r0, $r0, 1\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Init_Relation_Expr GT Init_PLUS_SUB
	     {  
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " slts $r0, $r0, $r1\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Init_Relation_Expr LT Init_PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " slts $r0, $r1, $r0\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             | Init_Relation_Expr NOTEQ Init_PLUS_SUB
	     {
			PopReg(0);
			PopReg(1);
			fprintf(f_asm, " xor $r0, $r0, $r1\n");
			fprintf(f_asm, " movi $r1, 0\n");
			fprintf(f_asm, " slt $r0, $r1, $r0\n");
			fprintf(f_asm, " zeb $r0, $r0\n");
			PushReg(0);
	     }
             ;

Init_PLUS_SUB: Init_MUL_DIV_MOD { $$ = $1; }
        | Init_PLUS_SUB PLUS Init_MUL_DIV_MOD
        {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;
	
		PopReg(0);
		PopReg(1);
		fprintf(f_asm, " add $r0, $r1, $r0\n");
		PushReg(0);
        }
        | Init_PLUS_SUB SUB Init_MUL_DIV_MOD
        {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;

		PopReg(0);
		PopReg(1);
		fprintf(f_asm, " sub $r0, $r1, $r0\n");
		PushReg(0);
        }
        ;

Init_MUL_DIV_MOD: Init_Unary_Expr { $$ = $1; }
           | Init_MUL_DIV_MOD MUL Init_Unary_Expr
           {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;

		PopReg(0);
		PopReg(1);
		fprintf(f_asm, " mul $r0, $r1, $r0\n");
		PushReg(0);
           }
           | Init_MUL_DIV_MOD DIV Init_Unary_Expr
           {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;

		PopReg(2);
		PopReg(3);
		fprintf(f_asm, " divsr $r0, $r1, $r3, $r2\n");
		PushReg(0);
           }
           | Init_MUL_DIV_MOD MOD Init_Unary_Expr
           {
		if ($1 != $3 || ($1 != Int && $1 != Double)) yyerror("Type error");
		$$ = $1;

		PopReg(2);
		PopReg(3);
		fprintf(f_asm, " divsr $r0, $r1, $r3, $r2\n");
		PushReg(1);
           }
           ;
 
Init_Unary_Expr: Init_PO_or_SO { $$ = $1; }
          | SUB Init_PO_or_SO
          {
		PopReg(0);
		fprintf(f_asm, " subri $r0, $r0, 0\n");
		PushReg(0);

		$$ = $2;
          }
          ;

Init_PO_or_SO: Init_Final_Expr { $$ = $1; }
        | Init_Final_Expr PO { $$ = $1; }
        | Init_Final_Expr SO { $$ = $1; }
        ;

Init_Final_Expr: INTEGER
          {
	  	fprintf(f_asm, " movi $r0, %d\n",$1);
	 	PushReg(0);

	  	$$ = Int;
          }
          | DBLE { $$ = Double; }
          | CHR { $$ = Char; }
          | STR { $$ = String; }
          | ID_or_Array
          {
	  	int idx = look_up_symbol($1);
	
	 	switch(table[idx].mode){
			case ARGUMENT_MODE:
				fprintf(f_asm, " lwi $r0, [$sp+%d]\n",table[idx].offset*4);
				break;
			case LOCAL_MODE:
				fprintf(f_asm, " lwi $r0, [$sp+%d]\n",table[idx].offset*4);
				break;
          	}
          	
 	 	PushReg(0);
	
	  	$$ = table[idx].type;
          }
          | TRUE { $$ = Bool; }
          | FALSE { $$ = Bool; }
          | '(' Init_Expr ')' { $$ = $2; }
          ;

%%

extern int lineCount;
extern char* yytext;
extern char token[1000];

int yyerror( const char* str) {
	fprintf( stderr, "*** Error at line %d: %s\n", lineCount, token );
	fprintf( stderr, "\n" );
	fprintf( stderr, "Unmatched token: %s\n", yytext );
	fprintf( stderr, "*** syntax error\n");
	exit(-1);
}

int main(void) {
 
  init_symbol_table();

  if ((f_asm = fopen("assembly", "w")) == NULL)
	fprintf(stderr, "Failed to opening the file %s", "assembly");

  yyparse();
  
  if (Funct_Count == 0) yyerror("There should be at least one function");
  
  printf("No syntax error!\n");

  fclose(f_asm);
  
  return 0;
}
