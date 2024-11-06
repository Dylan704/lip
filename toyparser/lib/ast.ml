type ast =
    Const of int
  | Add of ast * ast
  | Minus of ast * ast
  | Uminus of ast
  | Multiplication of ast * ast
  | Division of ast * ast

