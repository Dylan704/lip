{
open Parser

exception Error of string
}

(* Spazi bianchi e caratteri *)
let white = [' ' '\t' '\n']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let char = digit | letter

(*Commenti*)
let comment = ['/'] ['/'] [^ '\n']* ['\n']

(* Regole per i tipi principali *)
let num = ['0'] | ['1'-'9'] digit*
let str = ['\"'] [^'\"']* ['\"']

let ide = letter char*

rule read =
  parse
  | white { read lexbuf }  
  | comment { read lexbuf}
  (* Tipi di valore *)
  | "bool"  { print_endline "Token: BOOL";  BOOLT }
  | "i32" { print_endline "Token: BOOL";  I32}
  | "String" { print_endline "Token: BOOL";  STRINGT }
  (* Operazioni su booleani *)
  | "not" { print_endline "Token: NOT"; NOT }
  | "and" { print_endline "Token: AND"; AND }
  | "or" { print_endline "Token: OR"; OR }
  (* Operazioni su interi *)
  | "+" { print_endline "Token: ADD"; ADD }
  | "-" { print_endline "Token: SUB"; SUB }
  | "*" { print_endline "Token: MUL"; MUL }
  | "/" { print_endline "Token: MUL"; DIV }
  | "%" { print_endline "Token: MUL"; MOD }
  | "<=" { print_endline "Token: EQ"; LEQ }
  (* Operazioni su stringhe *)
  | "String::from" { print_endline "Token: STRING::FROM"; STRFROM}
  (* Operazioni *)
  | "==" { print_endline "Token: EQ"; EQ }
  (* Sequenza e blocco *)
  | ";" { print_endline "Token: SEQ"; SEQ }
  | "{" { print_endline "Token: LBRACKET"; LBRACKET }
  | "}" { print_endline "Token: RBRACKET"; RBRACKET }
  (* Dichiarazione di variabili *)
  | "let" { print_endline "Token: LET"; LET }
  | "mut" { print_endline "Token: MUT"; MUT }
  | "&"  { print_endline "Token: BORROW"; BORROW }
  (* Dichiarazione di procedure e funzioni *)
  | "fn" { print_endline "Token: FUNCTION"; FUN}
  | ":" { print_endline "Token: COLON"; COLON }
  | "->" { print_endline "Token: ARROW"; ARROW }
  | "(" { print_endline "Token: LPAREN"; LPAREN }
  | ")" { print_endline "Token: RPAREN"; RPAREN }
  | "," { print_endline "Token: COMMA"; COMMA }
  | "_return_" { raise (Error (Printf.sprintf "Private keyword used: %s" (Lexing.lexeme lexbuf))) }
  (* Assegnamento *)
  | "=" { print_endline "Token: ASSIGN"; ASSIGN }
  | ".push_str" { print_endline "Token: PUSHSTR"; PUSHSTR}
  (* Condizionale *)
  | "if" { print_endline "Token: IF"; IF }
  | "else" { print_endline "Token: ELSE"; ELSE }
  (* Iterazione *)
  | "loop" { print_endline "Token : LOOP"; LOOP }
  | "break" { print_endline "Token : BREAK"; BREAK }
  (* Stampa *)
  | "println!" { print_endline "Token: PRINTLN"; PRINTLN}
  (*Identificatori di variabili e funzioni*)
  | ide { print_endline ("Token: IDE (" ^ Lexing.lexeme lexbuf ^ ")"); IDE (Lexing.lexeme lexbuf) }
  (*Costanti*)
  | "true" { print_endline "Token: TRUE"; BOOL (Lexing.lexeme lexbuf) }
  | "false" { print_endline "Token: FALSE"; BOOL (Lexing.lexeme lexbuf) }
  | num { print_endline ("Token: NUM (" ^ Lexing.lexeme lexbuf ^ ")"); INT (Lexing.lexeme lexbuf) }  
  | str { print_endline ("Token: STR (" ^ Lexing.lexeme lexbuf ^ ")"); STRING (Lexing.lexeme lexbuf) }
 
  | eof { print_endline "Token: EOF"; EOF }
  (*Errore per carattere non trovato *)
  | _ as c { raise (Error (Printf.sprintf "Unexpected character: %c" c)) }