open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int 

let string_of_result n = string_of_int n
    
(* eval : ast -> result *)
    
let rec eval = function
Const(n) -> n
| Add(e1,e2) -> eval e1 + eval e2
| Minus(e1, e2) -> eval e1 - eval e2
| Uminus e1 -> - eval e1
| Multiplication(e1, e2) -> eval e1 * eval e2 
| Division(e1, e2) -> if eval e2 = 0 then failwith "Errore divisione per 0" else eval e1 / eval e2 
;;

                    
