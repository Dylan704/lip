%{
open Ast
%}

%token TRUE FALSE
%token <string> VAR
%token <int> CONST
%token NOT AND OR
%token ADD SUB MUL
%token EQ LEQ
%token ASSIGN
%token SEQ
%token IF THEN ELSE
%token WHILE DO
%token LPAREN RPAREN
%token EOF

%right NOT
%left AND OR
%left ADD SUB 
%left MUL
%left EQ LEQ

%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | VAR { Var($1) }
  | CONST { Const($1) }
  | NOT; e = expr; { Not(e) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | e1 = expr; ADD; e2 = expr; { Add(e1, e2) }
  | e1 = expr; SUB; e2 = expr; { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr; { Mul(e1, e2) }
  | e1 = expr; EQ; e2 = expr; { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr; { Leq(e1, e2) }
  | LPAREN; e = expr; RPAREN { e }

cmd:
  | VAR; ASSIGN; e = expr; { Assign($1, e) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1, c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e, c1, c2) }
  | WHILE; e = expr; DO; c = cmd; {While(e, c)}