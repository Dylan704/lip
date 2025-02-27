{
open Parser

exception Error of string
}

(* Spazi bianchi e caratteri *)
let white = [' ' '\t' '\n']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let char = digit | letter

(* Commenti *)
let comment = ['/'] ['/'] [^ '\n']* ['\n']

(* Regole per i tipi principali *)
let num = ['0'] | ['1'-'9'] digit*
let str = ['\"'] [^'\"']* ['\"']

let ide = letter char*

rule read =
  parse
  | white { read lexbuf }  
  | comment { read lexbuf }
  (* Tipi di valore *)
  | "bool"  { BOOLT }
  | "i32" { I32 }
  | "String" { STRINGT }
  (* Operazioni su booleani *)
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  (* Operazioni su interi *)
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "%" { MOD }
  | "<=" { LEQ }
  (* Operazioni su stringhe *)
  | "String::from" { STRFROM }
  (* Operazioni *)
  | "==" { EQ }
  (* Sequenza e blocco *)
  | ";" { SEQ }
  | "{" { LBRACKET }
  | "}" { RBRACKET }
  (* Dichiarazione di variabili *)
  | "let" { LET }
  | "mut" { MUT }
  (* Dichiarazione di procedure e funzioni *)
  | "fn" { FUN }
  | ":" { COLON }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "_return_" { raise (Error (Printf.sprintf "Private keyword used: %s" (Lexing.lexeme lexbuf))) }
  (* Assegnamento *)
  | "=" { ASSIGN }
  | "&" { BORROW }
  | ".push_str" { PUSHSTR }
  (* Condizionale *)
  | "if" { IF }
  | "else" { ELSE }
  (* Iterazione *)
  | "loop" { LOOP }
  | "break" { BREAK }
  (* Stampa *)
  | "println!" { PRINTLN }
  (* Identificatori di variabili e funzioni *)
  | ide { IDE (Lexing.lexeme lexbuf) }
  (* Costanti *)
  | "true" { BOOL (Lexing.lexeme lexbuf) }
  | "false" { BOOL (Lexing.lexeme lexbuf) }
  | num { INT (Lexing.lexeme lexbuf) }  
  | str { STRING (Lexing.lexeme lexbuf) }
 
  | eof { EOF }
  (* Errore per carattere non trovato *)
  | _ as c { raise (Error (Printf.sprintf "Unexpected character: %c" c)) }