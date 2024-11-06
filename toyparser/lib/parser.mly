%{
open Ast
%}

%token <string> CONST
%token PLUS
%token MINUS
%token MULTIPLICATION
%token DIVISION
%token LPAREN
%token RPAREN
%token EOF

%left PLUS MINUS
%left MULTIPLICATION DIVISION


%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Minus(e1,e2)}
  | MINUS; e1 = expr {Uminus(e1)}
  | e1 = expr; MULTIPLICATION; e2 = expr {Multiplication(e1,e2)}
  | e1 = expr; DIVISION; e2 = expr {Division(e1,e2)}
  | LPAREN; e=expr; RPAREN {e}
;
