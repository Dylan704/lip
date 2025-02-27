open Ast
open Types

let string_of_ty = function
  | BoolT -> "bool"
  | I32 -> "i32"
  | StringT -> "String"
  | FuncT -> "Fun"
  | ProcT -> "Proc"
  | PointerT -> "Pointer"

let string_of_val = function
  | Bool b -> if b then "true" else "false"
  | Int n -> string_of_int n
  | String s -> s
  | Func f -> f.name
  | Proc p -> p.name
  | Pointer (e, t) ->
      let mut = if t = Mutable then "M" else "" in
      mut ^ string_of_int (getenval_index e)

let string_of_param p =
  let mut = if p.loc_ty = Mutable then "M" else "" in
  mut ^ p.name ^ ":" ^ string_of_ty p.val_ty

let rec string_of_params pl =
  match pl with
  | [] -> ""
  | p :: pl' -> string_of_param p ^ string_of_params pl'

let rec string_of_expr = function
  (*Valori*)
  | BoolConst b -> string_of_bool b
  | IntConst n -> string_of_int n
  | StringConst s -> s
  | ProcConst p -> p.name
  | FuncConst f -> f.name
  | PointerConst (e, t) ->
      let mut = if t = Mutable then "M" else "" in
      mut ^ string_of_int (getenval_index e)
  (*Variabili*)
  | Var x -> x
  | Reference (x, t) ->
      let mut = if t = Mutable then "M" else "" in
      "&" ^ mut ^ x
  (*Operazioni sui booleani*)
  | Not e -> "not " ^ string_of_expr e
  | And (e1, e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or (e1, e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2
  (*Operazioni sugli interi*)
  | Add (e1, e2) -> string_of_expr e1 ^ "+" ^ string_of_expr e2
  | Sub (e1, e2) -> string_of_expr e1 ^ "-" ^ string_of_expr e2
  | Mul (e1, e2) -> string_of_expr e1 ^ "*" ^ string_of_expr e2
  | Div (e1, e2) -> string_of_expr e1 ^ "/" ^ string_of_expr e2
  | Mod (e1, e2) -> string_of_expr e1 ^ "%" ^ string_of_expr e2
  | Leq (e1, e2) -> string_of_expr e1 ^ "<=" ^ string_of_expr e2
  (*Operazioni sulle stringhe*)
  | Strfrom s -> "String::from(" ^ s ^ ")"
  (*Uguaglianza*)
  | Eq (e1, e2) -> string_of_expr e1 ^ "=" ^ string_of_expr e2
  (* Expression Blocks e Funzioni *)
  | ExprBlock c -> "{" ^ string_of_cmd c ^ "}"
  | Fun (x, b) -> x ^ "(" ^ string_of_expr_list b ^ ")"

and string_of_expr_list el =
  match el with
  | [] -> ""
  | e :: el' -> string_of_expr e ^ ", " ^ string_of_expr_list el'

and string_of_cmd = function
  | Skip -> " skip "
  (*Sequenza*)
  | Seq (c1, c2) -> string_of_cmd c1 ^ "; " ^ string_of_cmd c2
  | Block c -> "{ " ^ string_of_cmd c ^ " }"
  (*Dichiarazione di variabili*)
  | Decl (ty, x, e) ->
      let mut = if ty = Mutable then "mut " else "" in
      "let " ^ mut ^ x ^ " = " ^ string_of_expr e
  (* Dichiarazione di procedure e funzioni *)
  | ProcDecl (p, pl, b) ->
      "Fn " ^ p ^ " (" ^ string_of_params pl ^ ")" ^ string_of_cmd (Block b)
  | FunDecl (p, pl, ret_ty, b) ->
      "Fn" ^ p ^ "(" ^ string_of_params pl ^ ")" ^ " -> " ^ string_of_ty ret_ty
      ^ string_of_cmd (Block b)
  (* Assegnamento *)
  | Assign (x, e) -> x ^ " = " ^ string_of_expr e
  | Pushstr (x, s) -> x ^ ".push_str(" ^ s ^ ")"
  (*Condizionale*)
  | If (e, c1, c2) ->
      " if " ^ string_of_expr e ^ " then " ^ string_of_cmd c1 ^ " else "
      ^ string_of_cmd c2
  (*Iterazione*)
  | Loop c -> " loop " ^ string_of_cmd c
  | Break -> " break "
  | ExecLoop (c, l) -> " exec loop " ^ string_of_cmd c ^ "; " ^ string_of_cmd l
  (*Stampa*)
  | Println s -> "println!(\"" ^ s ^ "\")"
  | DeclBlock c -> "{" ^ string_of_cmd c ^ "}"
  | Proc (x, b) -> x ^ "(" ^ string_of_expr_list b ^ ")"
  | ExecDeclBlock c -> "{" ^ string_of_cmd c ^ "}"

let string_of_envval st x =
  let env, _ = topenv st in
  let l_i, l_ty = Main.var_loc st x in
  let mut = if l_ty = Mutable then "M" else "" in
  match env x with
  | IVar _ | BVar _ | PointerVar _ -> mut ^ x ^ "/S" ^ string_of_int l_i
  | SVar _ | PVar _ | FVar _ -> mut ^ x ^ "/H" ^ string_of_int l_i

let rec string_of_env st xl =
  match xl with
  | [] -> ""
  | [ x ] -> ( try string_of_envval st x with _ -> "")
  | x :: xl' -> (
      try string_of_envval st x ^ ", " ^ string_of_env st xl'
      with _ -> string_of_env st xl')

(*MEM*)
let string_of_stackval st l =
  let stack = getstack st in
  match stack l with
  | v -> string_of_int l ^ "/" ^ string_of_val v

let rec string_of_stack st ll =
  match ll with
  | [] -> ""
  | [ l ] -> ( try string_of_stackval st l with _ -> "")
  | l :: ll' -> (
      try string_of_stackval st l ^ ", " ^ string_of_stack st ll'
      with _ -> string_of_stack st ll')

(*HEAP*)
let string_of_heapval st l =
  let heap = getheap st in
  match heap l with
  | v -> string_of_int l ^ "/" ^ string_of_val v

let rec string_of_heap st ll =
  match ll with
  | [] -> ""
  | [ l ] -> ( try string_of_heapval st l with _ -> "")
  | l :: ll' -> (
      try string_of_heapval st l ^ ", " ^ string_of_heap st ll'
      with _ -> string_of_heap st ll')

(* STATE *)
let rec getstacklocs env xl =
  match xl with
  | [] -> []
  | x :: xl -> (
      try
        let l = env x in
        match l with
        | IVar (l_i, _) | BVar (l_i, _) | PointerVar (l_i, _) ->
            l_i :: getstacklocs env xl
        | SVar _ | PVar _ | FVar _ -> getstacklocs env xl
      with _ -> getstacklocs env xl)

let rec getheaplocs env xl =
  match xl with
  | [] -> []
  | x :: xl -> (
      try
        let l = env x in
        match l with
        | IVar _ | BVar _ | PointerVar _ -> getheaplocs env xl
        | SVar (l_i, _) | PVar (l_i, _) | FVar (l_i, _) ->
            l_i :: getheaplocs env xl
      with _ -> getheaplocs env xl)

let string_of_state st xl =
  let env, _ = topenv st in
  "Envstack: "
  ^ string_of_int (List.length (getenv_stack st))
  ^ ", " ^ "Topenv: [" ^ string_of_env st xl ^ "], " ^ "Stack: ["
  ^ string_of_stack st (getstacklocs env xl)
  ^ "]" ^ ", " ^ "Heap: ["
  ^ string_of_heap st (getheaplocs env xl)
  ^ "]" ^ ", Stackloc: "
  ^ string_of_int (getstack_floc st)
  ^ ", Heaploc: "
  ^ string_of_int (getheap_floc st)

let string_of_conf vars = function
  | St st -> "\t" ^ string_of_state st vars
  | Cmd (c, st) ->
      "\t<" ^ string_of_state st vars ^ ", \n\t" ^ string_of_cmd c ^ ">"

let rec string_of_trace vars = function
  | [] -> ""
  | [ x ] -> string_of_conf vars x
  | x :: l -> string_of_conf vars x ^ "\n -> " ^ string_of_trace vars l
