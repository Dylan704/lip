%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ

%token LPAREN
%token RPAREN

%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token WHILE

%start <expr> prog

%left AND OR ADD SUB MUL EQ LEQ SEQ
%right ASSIGN WHILE DO
%right NOT

%%

prog:
  | e = expr EOF { e }
  | c = cmd EOF { c } 
;

expr:
  | TRUE { True }
  | FALSE { False }
  | LPAREN e = expr RPAREN { e }
  | NOT e = expr { Not e }
  | e1 = expr AND e2 = expr { And (e1, e2) }
  | e1 = expr OR e2 = expr { Or (e1, e2) }
  | e1 = expr ADD e2 = expr { Add(e1, e2) }
  | e1 = expr SUB e2 = expr { Sub(e1, e2) }
  | e1 = expr MUL e2 = expr { Mul(e1, e2) }
  | e1 = expr EQ e2 = expr { Eq(e1, e2) }
  | e1 = expr LEQ e2 = expr { Leq(e1, e2) }
;


cmd:
  | SKIP { Skip}
  | e1 = Var ASSIGN e2 = Const { Assign(e1, e2) }
  | e1 = cmd SEQ e2 = cmd { Seq(e1, e2) }
  | IF e1 = expr THEN e2 = cmd ELSE e3 = cmd { If (e1, e2, e3) }
  | WHILE e1 = expr DO e2 = cmd { While(e1, e2) }
;