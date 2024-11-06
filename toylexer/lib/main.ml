open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let rec max n counter = function
  | a::l -> if n = counter then [] else a :: max n (counter + 1) l 
  | [] -> []
;;

let sorting_reverse l = 
  List.rev (List.sort (fun (a, _) (a', _) -> if a > a' then 1 else if a = a' then 0 else -1) l)
;;

let rec unique = function 
  | a::l -> if List.mem a l then unique l else a :: unique l
  | [] -> []
;;

let frequency n l = 
  max n 0 (sorting_reverse (unique (List.map (fun first -> 
      (List.fold_left (fun acc x -> if x = first then acc + 1 else acc) 0 l, first)) l)))
  |> List.map (fun (count, elem) -> (elem, count))
;;
