open Ast

type exprval = Bool of bool | Nat of int

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Zero -> "Zero"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | And(e0,e1) -> "And(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Or(e0,e1) -> "Or(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Not(e0) -> "Not(" ^ (string_of_expr e0) ^ ")"
  | Succ(e0) -> "Succ(" ^ (string_of_expr e0) ^ ")"
  | Pred(e0) -> "Pred(" ^ (string_of_expr e0) ^ ")"
  | IsZero(e0) -> "IsZero(" ^ (string_of_expr e0) ^ ")"

  let string_of_val = function
  | Bool a -> string_of_bool a
  | Nat a -> string_of_int a


let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec trace1 = function
  (* Casi di If *)
  | If (True, e1, _) -> trace1 e1
  | If (False, _, e2) -> trace1 e2
  | If (e1, e2, e3) -> If (trace1 e1, e2, e3)

  (* Casi di And *)
  | And (True, e2) -> trace1 e2
  | And (e1, True) -> trace1 e1
  | And (False, _) -> False
  | And (_, False) -> False
  | And (e1, e2) -> trace1 (And (trace1 e1, trace1 e2))

  (* Casi di Or *)
  | Or (True, _) -> True
  | Or (_, True) -> True
  | Or (False, e2) -> trace1 e2
  | Or (e1, False) -> trace1 e1
  | Or (e1, e2) -> trace1 (Or (trace1 e1, trace1 e2))

  (* Casi di Not *)
  | Not True -> False
  | Not False -> True
  | Not e1 -> trace1 (Not (trace1 e1))

  (* Casi di Succ *)
  | Succ Zero -> Succ Zero
  | Succ e1 -> trace1 (Succ (trace1 e1))

  (* Casi di Pred *)
  | Pred Zero -> Zero
  | Pred (Succ e1) -> e1
  | Pred e1 -> trace1 (Pred (trace1 e1))

  (* Casi di IsZero *)
  | IsZero Zero -> True
  | IsZero (Succ _) -> False
  | IsZero e1 -> trace1 (IsZero (trace1 e1))

  (* Se non c'è una regola applicabile, solleva l'eccezione *)
  | _ -> raise NoRuleApplies


let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

let rec eval = function
| True -> Bool true
| False -> Bool false
| Zero -> Nat 0
| If(e0, e1, e2) -> (
    match eval e0 with
    | Bool true -> eval e1
    | Bool false -> eval e2
    | _ -> failwith "Condition of If must be a boolean"
  )
| Not e0 -> (
    match eval e0 with
    | Bool b -> Bool (not b)
    | _ -> failwith "Argument of Not must be a boolean"
  )
| And (e0, e1) -> (
    match (eval e0, eval e1) with
    | (Bool b0, Bool b1) -> Bool (b0 && b1)
    | _ -> failwith "Arguments of And must be booleans"
  )
| Or (e0, e1) -> (
    match (eval e0, eval e1) with
    | (Bool b0, Bool b1) -> Bool (b0 || b1)
    | _ -> failwith "Arguments of Or must be booleans"
  )
| Succ e0 -> (
    match eval e0 with
    | Nat n -> Nat (n + 1)
    | _ -> failwith "Argument of Succ must be a natural number"
  )
| Pred e0 -> (
    match eval e0 with
    | Nat n when n > 0 -> Nat (n - 1)
    | _ -> failwith "Argument of Pred must be a natural number"
  )
| IsZero e0 -> (
    match eval e0 with
    | Nat n -> Bool (n = 0)
    | _ -> failwith "Argument of IsZero must be a natural number"
  )
