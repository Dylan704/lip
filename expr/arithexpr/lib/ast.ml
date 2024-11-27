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

let isvalue : expr -> bool = function
  | True -> true
  | False -> true
  | _ -> false

let is_nv : expr -> bool = function
  | Zero -> true
  | Succ(_) -> true
  | _ -> false