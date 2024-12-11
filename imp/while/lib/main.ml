open Ast

type exprval = Bool of bool | Nat of int
(*SARITHEXPR NEW TYPE*)
type exprtype = BoolT | NatT
exception TypeError of string
exception NegativeNumberError of string


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

let string_of_type = function
    BoolT -> "BoolT"
  | NatT -> "NatT"

let string_of_val = function
  | Bool a -> string_of_bool a
  | Nat a -> string_of_int a


let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec trace1 = function
  | If(True, e1, _) -> e1                     (* [S-IfTrue] *)
  | If(False, _, e2) -> e2                    (* [S-IfFalse] *)
  | If(e0, e1, e2) ->                         (* [S-If] *)
      let e0' = trace1 e0 in
      If(e0', e1, e2)

  | Not(True) -> False                        (* [S-NotTrue] *)
  | Not(False) -> True                        (* [S-NotFalse] *)
  | Not(e) ->                                 (* [S-Not] *)
      let e' = trace1 e in
      Not(e')

  | And(True, e2) -> e2   
  | And(e2,True ) -> e2                      (* [S-AndTrue] *)
  | And(False, _) -> False    
  | And(_,False ) -> False                (* [S-AndFalse] *)
  | And(e1, e2) ->                           (* [S-And] *)
    let e1' = trace1 e1 in
    And(e1', e2)

  | Or(True, _) -> True                       (* [S-OrTrue] *)
  | Or(False, e2) -> e2                       (* [S-OrFalse] *)
  | Or(e1, e2) ->                             (* [S-Or] *)
      let e1' = trace1 e1 in
      Or(e1', e2)

  | Succ(e) ->                                (* [S-Succ] *)
      let e' = trace1 e in
      Succ(e')                       (* [S-PredZero] *)

  | Pred(Succ(e)) -> e                        (* [S-PredSucc] *)
  | Pred(e) ->                                (* [S-Pred] *)
      let e' = trace1 e in
      Pred(e')

  | IsZero(Zero) -> True                      (* [S-IsZeroZero] *)
  | IsZero(Succ(_)) -> False                  (* [S-IsZeroSucc] *)
  | IsZero(e) ->                              (* [S-IsZero] *)
      let e' = trace1 e in
      IsZero(e')

  | _ -> raise NoRuleApplies                  (* Nessuna regola applicabile *)

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


let rec typecheck = function 
| True -> BoolT
| False -> BoolT
| Zero -> NatT
| If(e0, e1, e2) -> (
    if(typecheck e1 = BoolT) then   
      if(typecheck e1 = typecheck e2)
        then  match eval e0 with
      | Bool true -> typecheck e1
      | Bool false -> typecheck e2
      | _ -> failwith "Condition of If must be a boolean"
      else
        failwith "Branches of if must have the same type"
    else
        failwith "Condition of if must be boolean"
  )
| Not e0 -> (
    match eval e0 with
    | Bool _ -> BoolT
    | _ -> failwith "Argument of Not must be a boolean"
  )
| And (e0, e1) -> (
    match (eval e0, eval e1) with
    | (Bool _, Bool _) -> BoolT
    | _ -> failwith "Arguments of And must be booleans"
  )
| Or (e0, e1) -> (
    match (eval e0, eval e1) with
    | (Bool _, Bool _) -> BoolT
    | _ -> failwith "Arguments of Or must be booleans"
  )
| Succ e0 -> (
    match eval e0 with
    | Nat _ -> NatT
    | _ -> failwith "Argument of Succ must be a natural number"
  )
| Pred e0 -> (
    match eval e0 with
    | Nat n when n > 0 -> NatT 
    | _ -> failwith "Argument of Pred must be a natural number"
  )
| IsZero e0 -> (
    match eval e0 with
    | Nat _ -> BoolT
    | _ -> failwith "Argument of IsZero must be a natural number"
  )