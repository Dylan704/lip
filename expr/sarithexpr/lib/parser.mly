%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token NOT
%token AND
%token OR
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token EOF

%start <expr> prog

%left AND OR
%right SUCC PRED ISZERO 
%right NOT

%%

prog:
  | e = expr EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | ZERO { Zero }
  | ISZERO e = expr { IsZero e }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { If (e1, e2, e3) }
  | LPAREN e = expr RPAREN { e }
  
  | NOT e = expr { Not e }
  | e1 = expr AND e2 = expr { And (e1, e2) }
  | e1 = expr OR e2 = expr { Or (e1, e2) }
  | SUCC e = expr { Succ e }
  | PRED e = expr { Pred e }
;
