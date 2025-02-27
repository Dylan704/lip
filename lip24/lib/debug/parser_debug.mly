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
  | dl = pf_decl_seq ; EOF { 
      debug_print "Parsed program"; 
      Seq(dl, Proc("main", [])) 
    }
;

ty:
  | BOOLT { 
      debug_print "Parsed type BOOL"; 
      BoolT 
    }
  | I32 { 
      debug_print "Parsed type I32"; 
      I32 
    }
  | STRINGT { 
      debug_print "Parsed type STRING"; 
      StringT 
    }

param:
  | x = IDE; COLON; t = ty { 
      debug_print ("Parsed parameter: " ^ x); 
      (x, t) 
    }

params_list:
  | { [] }
  | p = param { 
      debug_print ("Parsed parameter list with one parameter: " ^ fst p); 
      [p] 
    }
  | p = param ; COMMA; pl = params_list { 
      debug_print ("Parsed parameter: " ^ fst p); 
      p :: pl 
    }

pf_decl:
  | FUN; f = IDE; LPAREN; pl = params_list; RPAREN; b = block { 
      debug_print ("Parsed function declaration: " ^ f); 
      ProcDecl(f, pl, b) 
    }                                                                                    
  | FUN; f = IDE; LPAREN; pl = params_list; RPAREN ; ARROW; t = ty; b = block { 
      debug_print ("Parsed function declaration with return type: " ^ f); 
      FunDecl(f, pl, t, b) 
    }

pf_decl_seq:
  | d = pf_decl { 
      debug_print "Parsed function declaration sequence"; 
      d 
    }
  | d = pf_decl; dl = pf_decl_seq { 
      debug_print "Parsed function declaration sequence with multiple declarations"; 
      Seq(d, dl) 
    }

pf_call:
  | pf = IDE; LPAREN; el = expr_list; RPAREN { 
      debug_print ("Parsed function call: " ^ pf); 
      (pf, el) 
    }

block:
  | LBRACKET; cs = cmd_seq; RBRACKET { 
      debug_print "Parsed block"; 
      cs 
    }
  | LBRACKET; cs = cmd_seq_expr; RBRACKET { 
      debug_print "Parsed block with expression"; 
      cs 
    }

expr:
  (* Valori *)
  | b = BOOL { 
      debug_print ("Parsed boolean constant: " ^ b); 
      BoolConst(bool_of_string b) 
    }
  | n = INT { 
      debug_print ("Parsed integer constant: " ^ n); 
      IntConst(int_of_string n) 
    }
  | s = STRING { 
      debug_print ("Parsed string constant: " ^ s); 
      StringConst(remove_quotes s) 
    } 
  (* Variabili *)
  | x = IDE { 
      debug_print ("Parsed variable: " ^ x); 
      Var(x) 
    }
  (* Operazioni sui booleani *)
  | NOT; e = expr { 
      debug_print "Parsed NOT operation"; 
      Not(e) 
    }
  | e1 = expr; AND; e2 = expr { 
      debug_print "Parsed AND operation"; 
      And(e1, e2) 
    }
  | e1 = expr; OR; e2 = expr { 
      debug_print "Parsed OR operation"; 
      Or(e1, e2) 
    }
  (* Operazioni sugli interi *)
  | e1 = expr; ADD; e2 = expr { 
      debug_print "Parsed ADD operation"; 
      Add(e1, e2) 
    }
  | e1 = expr; SUB; e2 = expr { 
      debug_print "Parsed SUB operation"; 
      Sub(e1, e2) 
    }
  | e1 = expr; MUL; e2 = expr { 
      debug_print "Parsed MUL operation"; 
      Mul(e1, e2) 
    }
  | e1 = expr; DIV; e2 = expr { 
      debug_print "Parsed DIV operation"; 
      Div(e1, e2) 
    }
  | e1 = expr; MOD; e2 = expr { 
      debug_print "Parsed MOD operation"; 
      Mod(e1, e2) 
    }
  | e1 = expr; LEQ; e2 = expr { 
      debug_print "Parsed LEQ operation"; 
      Leq(e1, e2) 
    }
  (* Operazioni sulle stringhe *)
  | STRFROM; LPAREN; s = STRING ; RPAREN { 
      debug_print ("Parsed STRFROM operation with string: " ^ s); 
      Strfrom(remove_quotes s) 
    }
  (* Operazioni *)
  | e1 = expr; EQ; e2 = expr { 
      debug_print "Parsed EQ operation"; 
      Eq(e1, e2) 
    }
  (* Blocchi e procedure/funzioni *)
  | pfel = pf_call { 
      debug_print ("Parsed function call expression: " ^ fst pfel); 
      Fun(pfel) 
    }
  | cs = block { 
      debug_print "Parsed block expression"; 
      ExprBlock(cs) 
    }
  (* Precedenza *)
  | LPAREN; e = expr; RPAREN { 
      debug_print "Parsed parenthesized expression"; 
      e 
    }
;

(* Lista di espressioni *)
expr_list: 
  | { [] }
  | e = expr { 
      debug_print "Parsed expression list with one expression"; 
      [e] 
    }
  | e = expr; COMMA; es = expr_list { 
      debug_print "Parsed expression list with multiple expressions"; 
      e :: es 
    }
;

block_cmd: (*Senza punto e virgola*)
  (* Condizionale *)
  | IF; e = expr; cs = block  { 
      debug_print "Parsed IF command"; 
      If(e, DeclBlock(cs), Skip) 
    }
  | IF; e = expr; cs1 = block; ELSE; cs2 = block { 
      debug_print "Parsed IF-ELSE command"; 
      If(e, DeclBlock(cs1), DeclBlock(cs2)) 
    }
  (* Iterazione *)
  | LOOP; cs = block { 
      debug_print "Parsed LOOP command"; 
      Loop(DeclBlock(cs)) 
    }
  (* Blocchi *)
  | cs = block { 
      debug_print "Parsed block command"; 
      DeclBlock(cs) 
    }
  (* Dichiarazione di funzione*)
  | d = pf_decl { 
      debug_print "Parsed function declaration command"; 
      d 
    }

cmd: (*Senza punto e virgola*)
  (* Dichiarazione di variabili *)
  | LET; x = IDE; ASSIGN; e = expr { 
      debug_print ("Parsed variable declaration: " ^ x); 
      Decl(NMut, x, e) 
    }
  | LET; MUT; x = IDE; ASSIGN; e = expr { 
      debug_print ("Parsed mutable variable declaration: " ^ x); 
      Decl(Mut, x, e) 
    }
  (* Dichiarazione di variabili con borrowing *)
  | LET; x = IDE; ASSIGN; BORROW; y = IDE { 
      debug_print ("Parsed borrowing declaration: " ^ x ^ " from " ^ y); 
      Borrow(NMut, NMut, x, y) 
    }
  | LET; x = IDE; ASSIGN; BORROW; MUT; y = IDE { 
      debug_print ("Parsed mutable borrowing declaration: " ^ x ^ " from " ^ y); 
      Borrow(NMut, Mut, x, y) 
    }
  | LET; MUT; x = IDE; ASSIGN; BORROW; y = IDE { 
      debug_print ("Parsed borrowing declaration: " ^ x ^ " from " ^ y); 
      Borrow(Mut, NMut, x, y) 
    }
  | LET; MUT; x = IDE; ASSIGN; BORROW; MUT; y = IDE { 
      debug_print ("Parsed mutable borrowing declaration: " ^ x ^ " from " ^ y); 
      Borrow(Mut, Mut, x, y) 
    } 
  (* Assegnamento *)                                                                                                                            
  | x = IDE; ASSIGN; e = expr { 
      debug_print ("Parsed assignment: " ^ x); 
      Assign(x, e) 
    }
  | x = IDE; PUSHSTR; LPAREN; s = STRING ; RPAREN { 
      debug_print ("Parsed PUSHSTR operation for: " ^ x); 
      Pushstr(x, remove_quotes s) 
    } 
  (* Condizionale e iterazione *)
  | BREAK { 
      debug_print "Parsed BREAK command"; 
      Break 
    }
  (* Stampa *)
  | PRINTLN; LPAREN; s = STRING ; RPAREN { 
      debug_print ("Parsed PRINTLN command with string: " ^ s); 
      Println(remove_quotes s) 
    }
  (* Procedure e funzioni*)
  | pfel = pf_call { 
      debug_print ("Parsed procedure call: " ^ fst pfel); 
      Proc(pfel) 
    }
;

cmd_seq:
  (*Con punto e virgola*)
  | bc = block_cmd { 
      debug_print "Parsed command sequence with one command"; 
      bc 
    }
  | bc = block_cmd; SEQ { 
      debug_print "Parsed command sequence with multiple commands"; 
      bc 
    }
  | bc = block_cmd; cs = cmd_seq { 
      debug_print "Continuing command sequence"; 
      Seq(bc, cs) 
    }
  | bc = block_cmd; SEQ; cs = cmd_seq { 
      debug_print "Continuing command sequence with SEQ"; 
      Seq(bc, cs) 
    }
  | c = cmd { 
      debug_print "Parsed final command without semicolon"; 
      c 
    }
  | c = cmd; SEQ { 
      debug_print "Parsed single command in sequence"; 
      c 
    }
  | c = cmd; SEQ; cs = cmd_seq { 
      debug_print "Continuing command sequence with single command"; 
      Seq(c, cs) 
    }

cmd_seq_expr:
  (*Con punto e virgola seguito da expr*)
  | bc = block_cmd; SEQ; e = expr { 
      debug_print "Parsed command sequence with block command and final expression"; 
      Seq(bc, Decl(NMut, "_return_", e)) 
    }
  | bc = block_cmd; e = expr { 
      debug_print "Parsed command sequence with block command and final expression"; 
      Seq(bc, Decl(NMut, "_return_", e)) 
    }
  | bc = block_cmd; SEQ; cs = cmd_seq_expr { 
      debug_print "Continuing command sequence with block command"; 
      Seq(bc, cs) 
    }
  | bc = block_cmd; cs = cmd_seq_expr { 
      debug_print "Continuing command sequence with block command"; 
      Seq(bc, cs) 
    }
  | c = cmd; SEQ; e = expr { 
      debug_print "Parsed command sequence with command and final expression"; 
      Seq(c, Decl(NMut, "_return_", e)) 
    }
  | c = cmd; SEQ; cs = cmd_seq_expr { 
      debug_print "Continuing command sequence with command"; 
      Seq(c, cs) 
    }
