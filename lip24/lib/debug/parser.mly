%{
  open Ast

  let remove_quotes s =
    let len = String.length s in
    if len <= 2 then "" else String.sub s 1 (len - 2)
%}

(* Tipi di valore *)
%token BOOLT I32 STRINGT

(* Costanti *)
%token <string> BOOL
%token <string> INT
%token <string> STRING

(* Identificatori (variabili, procedure e funzioni) *)
%token <string> IDE

(* Operazioni su booleani *)
%token NOT AND OR

(* Operazioni su interi *)
%token ADD SUB MUL DIV MOD
%token LEQ

(* Operazioni su stringhe *)
%token STRFROM

(* Operazioni *)
%token EQ

(* Sequenza e blocco *)
%token SEQ
%token LBRACKET RBRACKET

(* Dichiarazione di variabili *)
%token LET MUT 

(* Dichiarazione di procedure e funzioni *)
%token FUN COLON ARROW 
%token LPAREN RPAREN 
%token COMMA

(* Assegnamento *)
%token ASSIGN BORROW
%token PUSHSTR

(* Condizionale *)
%token IF ELSE

(* Iterazione *)
%token LOOP BREAK

(* Stampa *)
%token PRINTLN 

(* Altri *)
%token EOF

(*Associativita'*)
%nonassoc NOT
%left OR 
%left AND
%left EQ LEQ
%left ADD SUB 
%left MUL DIV MOD

%start <cmd> prog

%%
prog:
  | dl = pf_decl_seq ; EOF { Seq(dl, Proc("main", [])) }
;

ty:
  | BOOLT { BoolT }
  | I32 { I32 }
  | STRINGT { StringT }

param:
  | x = IDE; COLON; t = ty { (x, t) }

params_list:
  | { [] }
  | p = param { [p] }
  | p = param ; COMMA; pl = params_list { p :: pl }

pf_decl:
  | FUN; f = IDE; LPAREN; pl = params_list; RPAREN; b = block { ProcDecl(f, pl, b) }                                                                                    
  | FUN; f = IDE; LPAREN; pl = params_list; RPAREN ; ARROW; t = ty; b = block { FunDecl(f, pl, t, b) }

pf_decl_seq:
  | d = pf_decl { d }
  | d = pf_decl; dl = pf_decl_seq { Seq(d, dl) }

pf_call:
  | pf = IDE; LPAREN; el = expr_list; RPAREN { (pf, el) }

block:
  | LBRACKET; cs = cmd_seq; RBRACKET { cs }
  | LBRACKET; cs = cmd_seq_expr; RBRACKET { cs }

expr:
  (* Valori *)
  | b = BOOL { BoolConst(bool_of_string b) }
  | n = INT { IntConst(int_of_string n) }
  | s = STRING { StringConst(remove_quotes s) } 
  (* Variabili *)
  | x = IDE { Var(x) }
  (* Operazioni sui booleani *)
  | NOT; e = expr { Not(e) }
  | e1 = expr; AND; e2 = expr { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  (* Operazioni sugli interi *)
  | e1 = expr; ADD; e2 = expr { Add(e1, e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1, e2) }
  | e1 = expr; MOD; e2 = expr { Mod(e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1, e2) }
  (* Operazioni sulle stringhe *)
  | STRFROM; LPAREN; s = STRING ; RPAREN { Strfrom(remove_quotes s) }
  (* Operazioni *)
  | e1 = expr; EQ; e2 = expr { Eq(e1, e2) }
  (* Blocchi e procedure/funzioni *)
  | pfel = pf_call { Fun(pfel) }
  | cs = block { ExprBlock(cs) }
  (* Precedenza *)
  | LPAREN; e = expr; RPAREN { e }
;

expr_list: 
  | { [] }
  | e = expr { [e] }
  | e = expr; COMMA; es = expr_list { e :: es }
;

block_cmd:
  (* Condizionale *)
  | IF; e = expr; cs = block { If(e, DeclBlock(cs), Skip) }
  | IF; e = expr; cs1 = block; ELSE; cs2 = block { If(e, DeclBlock(cs1), DeclBlock(cs2)) }
  (* Iterazione *)
  | LOOP; cs = block { Loop(DeclBlock(cs)) }
  (* Blocchi *)
  | cs = block { DeclBlock(cs) }
  (* Dichiarazione di funzione*)
  | d = pf_decl { d }

cmd:
  (* Dichiarazione di variabili *)
  | LET; x = IDE; ASSIGN; e = expr { Decl(NMut, x, e) }
  | LET; MUT; x = IDE; ASSIGN; e = expr { Decl(Mut, x, e) }
  (* Dichiarazione di variabili con borrowing *)
  | LET; x = IDE; ASSIGN; BORROW; y = IDE { Borrow(NMut, NMut, x, y) }
  | LET; x = IDE; ASSIGN; BORROW; MUT; y = IDE { Borrow(NMut, Mut, x, y) }
  | LET; MUT; x = IDE; ASSIGN; BORROW; y = IDE { Borrow(Mut, NMut, x, y) }
  | LET; MUT; x = IDE; ASSIGN; BORROW; MUT; y = IDE { Borrow(Mut, Mut, x, y) } 
  (* Assegnamento *)                                                                                                                            
  | x = IDE; ASSIGN; e = expr { Assign(x, e) }
  | x = IDE; PUSHSTR; LPAREN; s = STRING ; RPAREN { Pushstr(x, remove_quotes s) } 
  (* Condizionale e iterazione *)
  | BREAK { Break }
  (* Stampa *)
  | PRINTLN; LPAREN; s = STRING ; RPAREN { Println(remove_quotes s) }
  (* Procedure e funzioni*)
  | pfel = pf_call { Proc(pfel) }
;

cmd_seq:
  (*Con punto e virgola*)
  | bc = block_cmd { bc }
  | bc = block_cmd; SEQ { bc }
  | bc = block_cmd; cs = cmd_seq { Seq(bc, cs) }
  | bc = block_cmd; SEQ; cs = cmd_seq { Seq(bc, cs) }
  | c = cmd { c }
  | c = cmd; SEQ { c }
  | c = cmd; SEQ; cs = cmd_seq { Seq(c, cs) }

cmd_seq_expr:
  (*Con punto e virgola seguito da expr*)
  | bc = block_cmd; SEQ; e = expr { Seq(bc, Decl(NMut, "_return_", e)) }
  | bc = block_cmd; e = expr { Seq(bc, Decl(NMut, "_return_", e)) }
  | bc = block_cmd; SEQ; cs = cmd_seq_expr { Seq(bc, cs) }
  | bc = block_cmd; cs = cmd_seq_expr { Seq(bc, cs) }
  | c = cmd; SEQ; e = expr { Seq(c, Decl(NMut, "_return_", e)) }
  | c = cmd; SEQ; cs = cmd_seq_expr { Seq(c, cs) }
;