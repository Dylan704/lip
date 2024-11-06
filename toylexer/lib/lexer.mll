{
  open Token
}

let ATOK = ['A'-'Z'](['a'-'z']|['0'-'9'])*
let BTOK = ['a'|'e'|'i'|'0'|'u']*
let CTOK = [^BTOK]*[BTOK]?[^BTOK]*
let DTOK = ['-']?[num]*['.']?[num]?
let ETOK = ['0']['x'|'X']['0'-'9','A'-'F','a'-'f']*

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']+

rule read_token =
  parse
  | white { read_token lexbuf } 
  | ATOK { "ATOK" (Lexing.lexeme lexbuf)}
  | BTOK { "BTOK" (Lexing.lexeme lexbuf)}
  | CTOK { "CTOK" (Lexing.lexeme lexbuf)}
  | DTOK { "DTOK" (Lexing.lexeme lexbuf)}
  | ETOK { "ETOK" (Lexing.lexeme lexbuf)}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
