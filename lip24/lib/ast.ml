type mut = Mutable | Immutable

type borrow_ty = mut

(* Tipi per identificatori e locazioni *)
type ide = string

type loc_i = int
type loc_ty = mut
type loc = loc_i * loc_ty

(* Tipi di valore *)
type ty = BoolT | I32 | StringT | ProcT | FuncT | PointerT

type func = { name : ide; params : param list; ret_type : ty; body : cmd }

and proc = { name : ide; params : param list; body : cmd }
(*nuovo tipo ogetto per parametri
(loc_ty : mut\nmut, name , val_ty : tipo del valore)*)

and param = {
  loc_ty : loc_ty;
  name : ide;
  borrow_ty : borrow_ty option;
  val_ty : ty;
}

(*Valori d'ambiente: locazioni*)
and envval =
  | BVar of loc
  | IVar of loc
  | SVar of loc
  | PVar of loc
  | FVar of loc
  | PointerVar of loc

and pointer = envval * borrow_ty

(* Tipi di espressione *)
and expr =
  (* Costanti *)
  | BoolConst of bool
  | IntConst of int
  | StringConst of string
  | ProcConst of proc
  | FuncConst of func
  | PointerConst of pointer
  (* Variabili *)
  | Var of ide
  | Reference of ide * borrow_ty
  (* Operazioni su booleani *)
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  (* Operazioni su interi *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Leq of expr * expr
  (* Operazioni su stringhe *)
  | Strfrom of string
  (* Operazioni *)
  | Eq of expr * expr
  (* Expression Blocks e Funzioni *)
  | ExprBlock of cmd
  | Fun of (ide * expr list)

(* Tipi di comdando *)
and cmd =
  | Skip
  (* Sequenza e blocco *)
  | Seq of cmd * cmd
  | Block of cmd
  (* Dichiarazione di variabili *)
  | Decl of loc_ty * ide * expr
  (* Dichiarazione di procedure e funzioni *)
  | ProcDecl of ide * param list * cmd
  | FunDecl of ide * param list * ty * cmd
  (* Assegnamento *)
  | Assign of ide * expr
  | Pushstr of ide * string
  (* Condizionale *)
  | If of expr * cmd * cmd
  (* Iterazione *)
  | Loop of cmd
  | ExecLoop of cmd * cmd (*Runtime*)
  | Break
  (* Stampa *)
  | Println of string
  (* Declaration Blocks e Procedure *)
  | DeclBlock of cmd
  | Proc of (ide * expr list)
  | ExecDeclBlock of cmd

let make_param lt n bt vt =
  { loc_ty = lt; name = n; borrow_ty = bt; val_ty = vt }
