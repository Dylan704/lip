{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let exadecimal = ['0']['x'][ '0'-'9' 'A'-'F' ]*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULTIPLICATION}
  | "/" { DIVISION}
  | num { CONST (Lexing.lexeme lexbuf) }
  | exadecimal { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
