{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let var = letter (letter | digit)*
let const = digit+

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE } | "false" { FALSE }
  | "not" { NOT } | "and" { AND } | "or" { OR }
  | "+" { ADD } | "-" { SUB } | "*" { MUL }
  | "=" { EQ } | "<=" { LEQ }
  | ":=" { ASSIGN }
  | ";" { SEQ }
  | "if" { IF } | "then" { THEN } | "else" { ELSE }
  | "while" { WHILE } | "do" { DO }
  | "(" {LPAREN} | ")" {RPAREN}
  | var as id { VAR id }
  | const as n { CONST (int_of_string n) }
  | eof { EOF }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "int" { INT }
  | "bool" { BOOL }


