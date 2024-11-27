type expr =
    True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr

  let rec isvalue : expr -> bool = function
  | True -> true
  | False -> true
  | Zero -> true
  | Succ e when isvalue e -> true
  | Pred e when isvalue e -> true
  | _ -> false


let rec is_nv : expr -> bool = function
  | Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false