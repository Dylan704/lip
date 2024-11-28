open ArithexprLib.Ast
open ArithexprLib.Main


(**********************************************************************
 Test big-step semantics
 **********************************************************************)

let weval e = try Some (eval e)
  with _ -> None

let test_bigstep expr exp_result =
  (expr |> parse |> weval) = exp_result
  
let%test "test_bigstep1" = test_bigstep "if true then true else false and false" (Some (Bool true))

let%test "test_bigstep2" = test_bigstep "if true then false else false or true" (Some (Bool false))

let%test "test_bigstep3" = test_bigstep "succ 0" (Some (Nat 1))

let%test "test_bigstep4" = test_bigstep "succ succ succ pred pred succ succ pred succ pred succ 0" (Some (Nat 3))

let%test "test_bigstep5" = test_bigstep "iszero pred succ 0" (Some (Bool true))

let%test "test_bigstep6" = test_bigstep "iszero pred succ 0 and not iszero succ pred succ 0" (Some (Bool true))

let%test "test_bigstep7" = test_bigstep "iszero true" None

let%test "test_bigstep8" = test_bigstep "succ iszero 0" None

let%test "test_bigstep9" = test_bigstep "not 0" None

let%test "test_bigstep10" = test_bigstep "pred 0" None

let%test "test_bigstep11" = test_bigstep "pred pred succ 0" None


(**********************************************************************
 Test small-step semantics
 **********************************************************************)

(* last element of a list *)
let rec last = function
    [] -> failwith "last on empty list"
  | [x] -> x
  | _::l -> last l

(* convert nat values to int *)
let rec int_of_nat = function
    Zero -> 0
  | Succ n -> 1 + int_of_nat n
  | _ -> failwith "int_of_nat on non-nat"

let weval_smallstep e =
    let final_expr = last (trace e) in
    (* Stampa l'espressione finale *)
    Printf.printf "Final expression: %s\n" (string_of_expr final_expr);
    match final_expr with
    | True -> Some (Bool true)
    | False -> Some (Bool false)
    | e when is_nv e -> Some (Nat (int_of_nat e))
    | _ -> None
  
      
let test_smallstep expr exp_result =
  (expr |> parse |> weval_smallstep) = exp_result

let%test "test_smallstep5" = test_smallstep "iszero pred succ 0" (Some (Bool true))

let%test "test_smallstep1" = test_smallstep "if true then true else false and false" (Some (Bool true))

let%test "test_smallstep2" = test_smallstep "if true then false else false or true" (Some (Bool false))

let%test "test_smallstep3" = test_smallstep "succ 0" (Some (Nat 1))

let%test "test_smallstep4" = test_smallstep "succ succ succ pred pred succ succ pred succ pred succ 0" (Some (Nat 3))

let%test "test_smallstep6" = test_smallstep "iszero pred succ 0 and not iszero succ pred succ 0" (Some (Bool true))

let%test "test_smallstep7" = test_smallstep "iszero true" None

let%test "test_smallstep8" = test_smallstep "succ iszero 0" None

let%test "test_smallstep9" = test_smallstep "not 0" None

let%test "test_smallstep10" = test_smallstep "pred 0" None

let%test "test_smallstep11" = test_smallstep "pred pred succ 0" None

(*SARITHEXPR NEW TESTS*)

type wexprval = exprtype option

let string_of_wtype = function 
    Some t -> string_of_type t
  | _ -> "Error"

let wtypecheck e = try Some (typecheck e) 
  with _ -> None

(**********************************************************************
 Test type checker
 **********************************************************************)

let test_type expr exp_result =
  (expr |> parse |> wtypecheck) = exp_result
  
let%test "test_type1" = test_type "if true then true else false and false" (Some BoolT)

let%test "test_type2" = test_type "if true then false else false or true" (Some BoolT)

let%test "test_type3" = test_type "if iszero 0 then iszero succ 0 else false or true" (Some BoolT)

let%test "test_type4" = test_type "succ 0" (Some NatT)

let%test "test_type5" = test_type "succ succ succ pred pred succ succ pred succ pred succ 0" (Some NatT)

let%test "test_type6" = test_type "iszero pred succ 0" (Some BoolT)

let%test "test_type7" = test_type "iszero pred succ 0 and not iszero succ pred succ 0" (Some BoolT)

let%test "test_type8" = test_type "pred 0" (Some NatT)

let%test "test_type9" = test_type "pred pred succ 0" (Some NatT)
    
let%test "test_type10" = test_type "iszero true" None

let%test "test_type11" = test_type "succ iszero 0" None

let%test "test_type12" = test_type "not 0" None

let%test "test_type13" = test_type "if 0 then true else false" None

let%test "test_type14" = test_type "if succ 0 then true else false" None

let%test "test_type15" = test_type "if iszero 0 then true else 0" None

let%test "test_type16" = test_type "if iszero 0 then 0 else true" None

let%test "test_type17" = test_type "iszero 0 and succ 0" None

let%test "test_type18" = test_type "succ 0 and iszero 0" None

let%test "test_type19" = test_type "iszero 0 or succ 0" None

let%test "test_type20" = test_type "succ 0 or iszero 0" None
