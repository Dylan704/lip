open Ast
open Types

let bind st id v =
  fun x -> if x = id then v else st x

let bottom =
  fun _ -> raise (UnboundVar "Access to uninitialized variable")

(* Parsing dell'input e generazione del comando *)
let parse (s : string) : conf =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  Cmd(ast, fun _ -> Nat 0) 

(* Evaluating expressions *)
let rec eval_expr (st : state) = function
  | True -> Bool true
  | False -> Bool false
  | Var id -> 
    (try st id with Not_found -> raise (UnboundVar id))
  | Const i -> Nat i
  | Not e -> 
    (match eval_expr st e with
     | Bool b -> Bool (not b)
     | _ -> failwith "Not expects a boolean")
  | And (e1, e2) -> 
    (match (eval_expr st e1, eval_expr st e2) with
     | (Bool b1, Bool b2) -> Bool (b1 && b2)
     | _ -> failwith "And expects booleans")
  | Or (e1, e2) -> 
    (match (eval_expr st e1, eval_expr st e2) with
     | (Bool b1, Bool b2) -> Bool (b1 || b2)
     | _ -> failwith "Or expects booleans")
  | Add (e1, e2) -> 
    (match (eval_expr st e1, eval_expr st e2) with
     | (Nat n1, Nat n2) -> Nat (n1 + n2)
     | _ -> failwith "Add expects naturals")
  | Sub (e1, e2) -> 
    (match (eval_expr st e1, eval_expr st e2) with
     | (Nat n1, Nat n2) -> Nat (n1 - n2)
     | _ -> failwith "Sub expects naturals")
  | Mul (e1, e2) -> 
    (match (eval_expr st e1, eval_expr st e2) with
     | (Nat n1, Nat n2) -> Nat (n1 * n2)
     | _ -> failwith "Mul expects naturals")
  | Eq (e1, e2) -> 
    (match (eval_expr st e1, eval_expr st e2) with
     | (Nat n1, Nat n2) -> Bool (n1 = n2)
     | _ -> failwith "Eq expects naturals")
  | Leq (e1, e2) -> 
    (match (eval_expr st e1, eval_expr st e2) with
     | (Nat n1, Nat n2) -> Bool (n1 <= n2)
     | _ -> failwith "Leq expects naturals")

(* Reducing commands (small-step semantics) *)
let rec trace1 = function
  | Cmd (Skip, st) -> St st
  | Cmd (Assign (x, e), st) -> 
    let v = eval_expr st e in
    St (bind st x v)
  | Cmd (Seq (c1, c2), st) -> 
    (match trace1 (Cmd (c1, st)) with
     | St st' -> Cmd (c2, st')
     | Cmd (c1', st') -> Cmd (Seq (c1', c2), st'))
  | Cmd (If (e, c1, c2), st) -> 
    (match eval_expr st e with
     | Bool true -> Cmd (c1, st)
     | Bool false -> Cmd (c2, st)
     | _ -> failwith "If condition must be boolean")
  | Cmd (While (e, c), st) -> 
    (match eval_expr st e with
     | Bool true -> Cmd (Seq (c, While (e, c)), st)
     | Bool false -> St st
     | _ -> failwith "While condition must be boolean")
  | _ -> raise NoRuleApplies

(* Executing n steps *)
let rec trace n conf = 
  if n <= 0 then [conf]
  else 
    try 
      let conf' = trace1 conf in
      conf :: trace (n - 1) conf'
    with NoRuleApplies -> [conf]