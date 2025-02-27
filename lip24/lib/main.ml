open Ast
open Types

let extract_var s =
  let re = Str.regexp "[^{]*{\\([^}]*\\)}[^}]*" in
  (*re in cui il \\(gruppo 1\\) corrisponde al nome della variabile tra graffe*)
  if Str.string_match re s 0 then
    (* Ricerca la prima occorrenza della variabile sulla stringa *)
    Str.matched_group 1 s (* Restituisce il nome della variabile *)
  else failwith "Nessuna variabile trovata"

let rec extract_vars s =
  try
    let var = extract_var s in
    (* Usa la funzione extract_var per ottenere il nome della prima variabile della stringa *)
    (* Costruiamo la parte rimanente della stringa escludendo tutte le occorrenze della variabile trovata *)
    let rest_of_string =
      Str.global_replace (Str.regexp ("{" ^ var ^ "}")) "" s
    in
    var :: extract_vars rest_of_string
    (* Restituiamo la variabile trovata e chiamiamo ricorsivamente extract_vars sul resto della stringa *)
  with _ -> [] (* Se non ci sono più variabili, restituisce una lista vuota *)

let rec vars_of_expr = function
  | BoolConst _ | IntConst _ | StringConst _ | ProcConst _ | FuncConst _
  | PointerConst _ ->
      []
  | Strfrom s -> extract_vars s
  | Var x -> [ x ]
  | Reference (x, _) -> [ x ]
  | Not e -> vars_of_expr e
  | And (e1, e2)
  | Or (e1, e2)
  | Add (e1, e2)
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2)
  | Mod (e1, e2)
  | Leq (e1, e2)
  | Eq (e1, e2) ->
      union (vars_of_expr e1) (vars_of_expr e2)
  | ExprBlock c -> vars_of_cmd c
  | Fun (p, el) -> union [ p ] (List.flatten (List.map vars_of_expr el))

and vars_of_cmd = function
  | Skip -> []
  | Seq (c1, c2) -> union (vars_of_cmd c1) (vars_of_cmd c2)
  | Block c -> vars_of_cmd c
  | Decl (_, x, e) -> union [ x ] (vars_of_expr e)
  | ProcDecl (p, pl, b) ->
      union
        (union [ p ] (vars_of_cmd b))
        (List.flatten
           (List.map
              (function
                | p -> [ p.name ])
              pl))
  | FunDecl (f, pl, _, b) ->
      union
        (union [ f ] (vars_of_cmd b))
        (List.flatten
           (List.map
              (function
                | p -> [ p.name ])
              pl))
  | Assign (x, e) -> union [ x ] (vars_of_expr e)
  | Pushstr (x, s) -> union [ x ] (extract_vars s)
  | If (e, c1, c2) ->
      union (vars_of_expr e) (union (vars_of_cmd c1) (vars_of_cmd c2))
  | Loop c -> vars_of_cmd c
  | ExecLoop (c, l) -> union (vars_of_cmd c) (vars_of_cmd l)
  | Break -> []
  | Println s -> extract_vars s
  | DeclBlock c -> vars_of_cmd c
  | Proc (p, el) -> union [ p ] (List.flatten (List.map vars_of_expr el))
  | ExecDeclBlock c -> vars_of_cmd c

let rec expr_declared_vars st = function
  | BoolConst _ | IntConst _ | StringConst _ | ProcConst _ | FuncConst _
  | PointerConst _ ->
      []
  | Strfrom _ -> []
  | Var _ -> []
  | Reference (_, _) -> []
  | Not e -> expr_declared_vars st e
  | And (e1, e2)
  | Or (e1, e2)
  | Add (e1, e2)
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2)
  | Mod (e1, e2)
  | Leq (e1, e2)
  | Eq (e1, e2) ->
      union (expr_declared_vars st e1) (expr_declared_vars st e2)
  | ExprBlock c -> declared_vars st c
  | Fun (_, el) -> List.flatten (List.map (expr_declared_vars st) el)

and declared_vars st = function
  | Skip -> []
  | Seq (c1, c2) -> union (declared_vars st c1) (declared_vars st c2)
  | Block c -> declared_vars st c
  | Decl (_, x, e) -> union [ x ] (expr_declared_vars st e)
  | ProcDecl (p, pl, body) ->
      scope_check st body pl;
      [ p ]
  | FunDecl (f, pl, _, body) ->
      scope_check st body pl;
      [ f ]
  | Assign (_, e) -> expr_declared_vars st e
  | Pushstr (_, _) -> []
  | If (e, c1, c2) ->
      union (expr_declared_vars st e)
        (union (declared_vars st c1) (declared_vars st c2))
  | Loop c -> declared_vars st c
  | ExecLoop (c, l) -> union (declared_vars st c) (declared_vars st l)
  | Break -> []
  | Println _ -> []
  | DeclBlock c -> declared_vars st c
  | Proc (_, el) -> List.flatten (List.map (expr_declared_vars st) el)
  | ExecDeclBlock c -> declared_vars st c

and used_vars = function
  | Skip -> []
  | Seq (c1, c2) -> union (used_vars c1) (used_vars c2)
  | Block c -> used_vars c
  | Decl (_, x, e) -> union [ x ] (vars_of_expr e)
  | ProcDecl (_, _, _) -> []
  | FunDecl (_, _, _, _) -> []
  | Assign (x, e) -> union [ x ] (vars_of_expr e)
  | Pushstr (x, s) -> union [ x ] (extract_vars s)
  | If (e, c1, c2) ->
      union (vars_of_expr e) (union (used_vars c1) (used_vars c2))
  | Loop c -> used_vars c
  | ExecLoop (c, l) -> union (used_vars c) (used_vars l)
  | Break -> []
  | Println s -> extract_vars s
  | DeclBlock c -> used_vars c
  | Proc (p, el) -> union [ p ] (List.flatten (List.map vars_of_expr el))
  | ExecDeclBlock c -> used_vars c

and print_var_lists dl ul =
  Printf.printf "Variabili dichiarate (dl):\n";
  List.iter (fun x -> Printf.printf "  - %s\n" x) dl;
  Printf.printf "Variabili usate (ul):\n";
  List.iter (fun x -> Printf.printf "  - %s\n" x) ul;
  Printf.printf "\n"

and scope_check st body pl =
  let dl =
    union (declared_vars st body)
      (List.flatten
         (List.map
            (function
              | p -> [ p.name ])
            pl))
  in
  let ul = used_vars body in
  let check_var x =
    if not (List.mem x dl) then
      let gf, _ = globalenv st in
      let _ = gf x in
      ()
  in
  List.iter check_var ul

(*Restituisce la locazione associata a un identificativo*)
let var_enval st x =
  let env, _ = topenv st in
  try env x (* cerca la variabile nel topen*)
  with UnboundVar _ ->
    (* se non torva  la variabile nel topen la cerca nel globalenv(il piu in fondo)*)
    let genv, _ = globalenv st in
    genv x

let var_loc st x =
  let env, _ = topenv st in
  match env x with
  | BVar l | IVar l | SVar l | PVar l | FVar l | PointerVar l -> l

(*Restituisce il valore associato a un identificativo*)
let var_memval st x =
  let stack = getstack st in
  let heap = getheap st in
  match var_enval st x with
  | IVar l | BVar l | PointerVar l -> stack (getloc_index l)
  | SVar l when getloc_index l = -1 -> raise (MovedValue x)
  | SVar l | PVar l | FVar l -> heap (getloc_index l)

let var_val st x =
  let stack = getstack st in
  let heap = getheap st in
  match var_enval st x with
  | IVar l | BVar l -> stack (getloc_index l)
  | SVar l when getloc_index l = -1 -> raise (MovedValue x)
  | SVar l | PVar l | FVar l -> heap (getloc_index l)
  | PointerVar l -> (
      match stack (getloc_index l) with
      | Pointer (enval', _) -> (
          match enval' with
          | IVar l | BVar l -> stack (getloc_index l)
          | SVar l when getloc_index l = -1 -> raise (MovedValue x)
          | SVar l | PVar l | FVar l -> heap (getloc_index l)
          | _ -> failwith "Unreachable")
      | _ -> failwith "Unreachable")

let var_ty st x =
  match var_memval st x with
  | Bool _ -> BoolT
  | Int _ -> I32
  | String _ -> StringT
  | Proc _ -> ProcT
  | Func _ -> FuncT
  | Pointer _ -> PointerT

let string_of_var_val st x =
  let v = var_val st x in
  match v with
  | Bool b -> string_of_bool b
  | Int n -> string_of_int n
  | String s -> s
  | Proc p -> p.name
  | Func f -> f.name
  | _ -> failwith "Unreachable"

let string_with_var s =
  let re = Str.regexp "[^\"]*{[^}]*}[^\"]*" in
  Str.string_match re s 0

let replace_braces_with_value str st x =
  let re = Str.regexp ("[^{]*\\({" ^ x ^ "}\\)[^}]*") in
  (* Troviamo il contenuto tra parentesi graffe. *)
  if Str.string_match re str 0 then
    (* Sostituiamo il contenuto con il valore derivato da st e x *)
    let content = Str.matched_group 1 str in
    let new_value = string_of_var_val st x in
    Str.global_replace (Str.regexp content) new_value str
  else str
(* Se non troviamo parentesi graffe, restituiamo la stringa originale *)

let rec replace_all_braces_with_values str st vars =
  match vars with
  | [] ->
      str (* Se la lista di variabili è vuota, restituiamo la stringa finale *)
  | var :: rest ->
      (* Sostituiamo tutte le occorrenze di {var} con il valore derivato da st e var *)
      let updated_str = replace_braces_with_value str st var in
      (* Chiamiamo ricorsivamente replace_all_braces_with_values sul resto della lista di variabili *)
      replace_all_braces_with_values updated_str st rest

let string_from st s =
  if string_with_var s then replace_all_braces_with_values s st (extract_vars s)
  else s

let enval_equal x y =
  match (x, y) with
  | BVar (xi, xt), BVar (yi, yt)
  | IVar (xi, xt), IVar (yi, yt)
  | SVar (xi, xt), SVar (yi, yt) ->
      xi == yi && xt == yt
  | _ -> false

(*Funzione che controlla che tutte le referenze siano immutabili*)
let check_borrow_all_mut st loc_x =
  let _, ide_l = topenv st in
  (* Debug print della lista *)
  List.fold_right
    (fun y acc ->
      match var_memval st y with
      | Pointer (loc_y, b_ty_y) when enval_equal loc_x loc_y ->
          b_ty_y = Mutable && acc
      | _ -> acc)
    ide_l true

let rec check_borrow_all_mut_rec st loc_x =
  match getenv_stack st with
  | [] -> true
  | _ :: _ ->
      check_borrow_all_mut st loc_x
      && check_borrow_all_mut_rec (setenv_stack st (popenv st)) loc_x

(*Funzione di parse*)
let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec eval_expr st e =
  match e with
  (*Costanti*)
  | BoolConst b -> (st, Bool b)
  | IntConst n -> (st, Int n)
  | StringConst s -> (st, String s)
  | ProcConst s -> (st, Proc s)
  | FuncConst s -> (st, Func s)
  | PointerConst (e, b_ty) -> (st, Pointer (e, b_ty))
  (*Variabili*)
  | Var x -> (st, var_memval st x)
  | Reference (x, _) -> (st, var_memval st x)
  (*Operazioni sui booleani*)
  | Not e -> (
      match eval_expr st e with
      | st', Bool b -> (st', Bool (not b))
      | _ -> raise (TypeError "Not"))
  | And (e1, e2) -> (
      let st', v1 = eval_expr st e1 in
      let st'', v2 = eval_expr st' e2 in
      match (v1, v2) with
      | Bool b1, Bool b2 -> (st'', Bool (b1 && b2))
      | _ -> raise (TypeError "And"))
  | Or (e1, e2) -> (
      let st', v1 = eval_expr st e1 in
      let st'', v2 = eval_expr st' e2 in
      match (v1, v2) with
      | Bool b1, Bool b2 -> (st'', Bool (b1 || b2))
      | _ -> raise (TypeError "Or"))
  (*Operazioni sugli interi*)
  | Add (e1, e2) -> (
      let st', v1 = eval_expr st e1 in
      let st'', v2 = eval_expr st' e2 in
      match (v1, v2) with
      | Int n1, Int n2 -> (st'', Int (n1 + n2))
      | _ -> raise (TypeError "Add"))
  | Sub (e1, e2) -> (
      let st', v1 = eval_expr st e1 in
      let st'', v2 = eval_expr st' e2 in
      match (v1, v2) with
      | Int n1, Int n2 -> (st'', Int (n1 - n2))
      | _ -> raise (TypeError "Sub"))
  | Mul (e1, e2) -> (
      let st', v1 = eval_expr st e1 in
      let st'', v2 = eval_expr st' e2 in
      match (v1, v2) with
      | Int n1, Int n2 -> (st'', Int (n1 * n2))
      | _ -> raise (TypeError "Mul"))
  | Div (e1, e2) -> (
      let st', v1 = eval_expr st e1 in
      let st'', v2 = eval_expr st' e2 in
      match (v1, v2) with
      | Int n1, Int n2 -> (st'', Int (n1 / n2))
      | _ -> raise (TypeError "Div"))
  | Mod (e1, e2) -> (
      let st', v1 = eval_expr st e1 in
      let st'', v2 = eval_expr st' e2 in
      match (v1, v2) with
      | Int n1, Int n2 -> (st'', Int (n1 mod n2))
      | _ -> raise (TypeError "Mod"))
  | Leq (e1, e2) -> (
      let st', v1 = eval_expr st e1 in
      let st'', v2 = eval_expr st' e2 in
      match (v1, v2) with
      | Int n1, Int n2 -> (st'', Bool (n1 <= n2))
      | _ -> raise (TypeError "Eq"))
  (*Operazioni sulle stringhe*)
  | Strfrom s -> (st, String (string_from st s))
  (*Uguaglianza*)
  | Eq (e1, e2) -> (
      let st', v1 = eval_expr st e1 in
      let st'', v2 = eval_expr st' e2 in
      match (v1, v2) with
      | Bool b1, Bool b2 -> (st'', Bool (b1 = b2))
      | Int n1, Int n2 -> (st'', Bool (n1 = n2))
      | String s1, String s2 -> (st'', Bool (String.equal s1 s2))
      | _ -> raise (TypeError "Eq"))
  | ExprBlock c -> (
      let new_state = setenv_stack st (push_env st (topenv st)) in
      let prog = Cmd (Block c, new_state) in
      let confl = trace_rec 1000 0 prog in
      let conf' = last confl in
      match conf' with
      | St st' | Cmd (_, st') -> (
          try (setenv_stack st' (popenv st'), var_memval st' "_return_")
          with UnboundVar _ -> failwith "DeclBlock called as ExprBlock"))
  | Fun (f, el) -> (
      (*lista parametri funzione*)
      let c, pl, t =
        match var_memval st f with
        | Proc _ -> failwith "Procedure called as Function"
        | Func f -> (f.body, f.params, f.ret_type)
        | _ -> raise (TypeError "Proc")
      in
      (* pusho nuovo env sullo stack *)
      let new_env_stack = push_env st env0 in
      let new_state = setenv_stack st new_env_stack in
      (* creo nuovo stato *)
      let new_state' = consecutive_decl el pl st new_state in
      (*lancio body funzione*)
      let prog = Cmd (Block c, new_state') in
      let confl = trace_rec 1000 0 prog in
      let conf' = last confl in
      match conf' with
      | St st' | Cmd (_, st') -> (
          try
            if t = expr_ty st' (Var "_return_") then
              (setenv_stack st' (popenv st'), var_memval st' "_return_")
            else
              failwith "Function return type doesn't match returned value type"
          with UnboundVar _ -> failwith "DeclBlock called as ExprBlock"))

and expr_ty st e =
  match eval_expr st e with
  | _, Bool _ -> BoolT
  | _, Int _ -> I32
  | _, String _ -> StringT
  | _, Proc _ -> ProcT
  | _, Func _ -> FuncT
  | _, Pointer _ -> PointerT

and assign old_st x e =
  let envval = var_enval old_st x in
  (*Assegno solo se variabili mutabili*)
  let st, v = eval_expr old_st e in
  let new_stack, new_heap =
    match (envval, v) with
    | BVar (l_i, _), Bool _ -> (update_stack (getstack st) l_i v, getheap st)
    | IVar (l_i, _), Int _ -> (update_stack (getstack st) l_i v, getheap st)
    | SVar (l_i, _), String _ -> (getstack st, update_heap (getheap st) l_i v)
    | PVar (l_i, _), Proc _ -> (getstack st, update_heap (getheap st) l_i v)
    | FVar (l_i, _), Func _ -> (getstack st, update_heap (getheap st) l_i v)
    | PointerVar (l_i, _), Pointer _ ->
        (update_stack (getstack st) l_i v, getheap st)
    | _ -> raise (TypeError "Assign")
  in
  let new_state =
    make_state (getenv_stack st) new_stack new_heap (getstack_floc st)
      (getheap_floc st) (getout st)
  in
  new_state

and assign_to_original_owner old_st x e =
  let memval = var_memval old_st x in
  (*Assegno solo se variabili mutabili*)
  let st, v = eval_expr old_st e in
  let new_stack, new_heap =
    match (memval, v) with
    | Pointer (envval, _), Bool _ ->
        (update_stack (getstack st) (getenval_index envval) v, getheap st)
    | Pointer (envval, _), Int _ ->
        (update_stack (getstack st) (getenval_index envval) v, getheap st)
    | Pointer (envval, _), String _ ->
        (getstack st, update_heap (getheap st) (getenval_index envval) v)
    | _ -> raise (TypeError "Assign Owner")
  in
  let new_state =
    make_state (getenv_stack st) new_stack new_heap (getstack_floc st)
      (getheap_floc st) (getout st)
  in
  new_state

(*Implementazione dichiarazione*)
and decl old_st ty x e =
  let st, v = eval_expr old_st e in
  let l, new_stack_floc, new_heap_floc, v' =
    match v with
    | Bool b ->
        ( BVar (getstack_floc st, ty),
          getstack_floc st + 1,
          getheap_floc st,
          BoolConst b )
    | Int i ->
        ( IVar (getstack_floc st, ty),
          getstack_floc st + 1,
          getheap_floc st,
          IntConst i )
    | String s ->
        ( SVar (getheap_floc st, ty),
          getstack_floc st,
          getheap_floc st + 1,
          StringConst s )
    | Proc p ->
        ( PVar (getheap_floc st, ty),
          getstack_floc st,
          getheap_floc st + 1,
          ProcConst p )
    | Func f ->
        ( FVar (getheap_floc st, ty),
          getstack_floc st,
          getheap_floc st + 1,
          FuncConst f )
    | Pointer (e, t) ->
        ( PointerVar (getstack_floc st, ty),
          getstack_floc st + 1,
          getheap_floc st,
          PointerConst (e, t) )
  in
  let new_topenv = update_env (topenv st) x l in
  let new_envstack = swap_topenv st new_topenv in
  let new_state =
    make_state new_envstack (getstack st) (getheap st) new_stack_floc
      new_heap_floc (getout st)
  in
  assign new_state x v'

(*trasferisce ownership *)
and owner_trans st ty x y =
  let enval_y = var_enval st y in
  (*Assegno locazione y a x*)
  let new_topenv =
    let new_enval_y =
      try
        let enval_x = var_enval st x in
        match (enval_x, enval_y) with
        | SVar _, SVar l -> SVar (setloc_index l (-1))
        | _, _ -> raise (TypeError "owner trans")
      with UnboundVar _ -> (
        match enval_y with
        | SVar l -> SVar (setloc_index l (-1))
        | _ -> raise (TypeError "owner trans"))
    in
    let new_topenv1 = update_env (topenv st) x (setenval_type enval_y ty) in
    let new_topenv2 = update_env new_topenv1 y new_enval_y in
    new_topenv2
  in
  let new_envstack = swap_topenv st new_topenv in
  let new_state = setenv_stack st new_envstack in
  new_state

and owner_trans_on_parameter st ty p x =
  let env, _ = callerenv st in
  let enval_x = env x in
  (*Assegno locazione x a p*)
  let new_topenv, new_callerenv =
    let new_enval_x =
      match enval_x with
      | SVar l -> SVar (setloc_index l (-1))
      | _ -> raise (TypeError "owner trans")
    in
    ( update_env (topenv st) p (setenval_type enval_x ty),
      update_env (callerenv st) x new_enval_x )
  in
  let new_envstack1 = swap_topenv st new_topenv in
  let new_state1 = setenv_stack st new_envstack1 in
  let new_envstack2 = swap_callerenv new_state1 new_callerenv in
  let new_state2 = setenv_stack new_state1 new_envstack2 in
  new_state2

and borrow st ty x y borrow_ty =
  let _, loc_ty_y = var_loc st y in
  if
    match (loc_ty_y, borrow_ty) with
    | Mutable, Mutable ->
        if check_borrow_all_mut_rec st (var_enval st y) then true
        else raise (DataRace (y, Mutable, Immutable))
    | Mutable, Immutable -> true
    | Immutable, Mutable -> false
    | Immutable, Immutable -> true
  then decl st ty x (PointerConst (var_enval st y, borrow_ty))
  else raise (MutBorrowOfNonMut y)

and borrow_on_parameter st ty p x borrow_ty =
  let env, _ = callerenv st in
  let enval_x = env x in
  let loc_ty_x = getenval_type enval_x in
  if
    match (loc_ty_x, borrow_ty) with
    | Mutable, Mutable ->
        if check_borrow_all_mut_rec st enval_x then true
        else raise (DataRace (x, Mutable, Immutable))
    | Mutable, Immutable -> true
    | Immutable, Mutable -> false
    | Immutable, Immutable -> true
  then decl st ty p (PointerConst (enval_x, borrow_ty))
  else raise (MutBorrowOfNonMut x)

and expr_b_ty e =
  match e with
  | Reference (_, b_ty) -> Some b_ty
  | _ -> None

and equal_b_ty b_t1 b_t2 =
  match (b_t1, b_t2) with
  | None, None -> true
  | Some t1, Some t2 when t1 == t2 -> true
  | _ -> false

and consecutive_decl el pl caller_st st =
  match (el, pl) with
  | [], [] -> st
  | [], _ -> failwith "Not enough parameters"
  | _, [] -> failwith "Not enough expressions"
  | e :: el', p :: pl' ->
      let e_ty = expr_ty caller_st e in
      let e_b_ty = expr_b_ty e in
      if e_ty == p.val_ty && equal_b_ty e_b_ty p.borrow_ty then
        let st' =
          match e with
          | Reference (y, b_ty) -> borrow_on_parameter st p.loc_ty p.name y b_ty
          | Var y -> (
              match var_enval caller_st y with
              | BVar _ | IVar _ ->
                  let caller_st', st' =
                    match eval_expr caller_st e with
                    | caller_st', Bool b ->
                        (caller_st', decl st p.loc_ty p.name (BoolConst b))
                    | caller_st', Int i ->
                        (caller_st', decl st p.loc_ty p.name (IntConst i))
                    | _ -> failwith "Unreachable"
                  in
                  let new_envstack2 = swap_callerenv st' (topenv caller_st') in
                  let new_state2 = setenv_stack st' new_envstack2 in
                  new_state2
              | SVar _ -> owner_trans_on_parameter st p.loc_ty p.name y
              | _ -> raise (TypeError ("Parameter declaration " ^ p.name)))
          | _ ->
              let caller_st', st' =
                match eval_expr caller_st e with
                | caller_st', Bool b ->
                    (caller_st', decl st p.loc_ty p.name (BoolConst b))
                | caller_st', Int i ->
                    (caller_st', decl st p.loc_ty p.name (IntConst i))
                | caller_st', String s ->
                    (caller_st', decl st p.loc_ty p.name (StringConst s))
                | _ -> failwith "Unreachable"
              in
              let new_envstack2 = swap_callerenv st' (topenv caller_st') in
              let new_state2 = setenv_stack st' new_envstack2 in
              new_state2
        in
        consecutive_decl el' pl' caller_st st'
      else raise (TypeError ("Parameter declaration " ^ p.name))

and trace_cmd conf =
  match conf with
  | St _ -> raise NoRuleApplies
  | Cmd (c, st) -> (
      match c with
      | Skip -> St st
      (* Sequenza e blocco *)
      | Seq (c1, c2) -> (
          let conf' = trace_cmd (Cmd (c1, st)) in
          match conf' with
          | St st' -> Cmd (c2, st')
          | Cmd (c1', st') -> Cmd (Seq (c1', c2), st'))
      | Block c -> (
          let conf' = trace_cmd (Cmd (c, st)) in
          match conf' with
          | St st' -> St st'
          | Cmd (c1', st') -> Cmd (Block c1', st'))
      (*Dichiarazione di variabili*)
      | Decl (ty, x, e) ->
          let st' =
            match e with
            | Reference (y, b_ty) -> borrow st ty x y b_ty
            | Var y -> (
                match var_enval st y with
                | BVar _ | IVar _ -> decl st ty x e
                | SVar _ -> owner_trans st ty x y
                | _ -> raise (TypeError "Decl"))
            | _ -> decl st ty x e
          in
          St st'
      (* Dichiarazione di procedure e funzioni *)
      | ProcDecl (f, p, c) ->
          let new_proc = { name = f; params = p; body = c } in
          scope_check st c p;
          let new_state = decl st Immutable f (ProcConst new_proc) in
          St new_state
      | FunDecl (f, p, t, c) ->
          let new_fun = { name = f; params = p; ret_type = t; body = c } in
          scope_check st c p;
          let new_state = decl st Immutable f (FuncConst new_fun) in
          St new_state
      (*Assegnamenti*)
      | Assign (x, e) ->
          let l_ty = getenval_type (var_enval st x) in
          if l_ty = Mutable then
            let st' =
              match e with
              | Var y -> (
                  match var_enval st y with
                  | BVar _ | IVar _ | PointerVar _ -> assign st x e
                  | SVar _ -> owner_trans st l_ty x y
                  | _ -> raise (TypeError "Decl"))
              | _ -> assign st x e
            in
            St st'
          else raise (CannotMutate x)
      | Pushstr (x, s) ->
          let l_ty =
            match var_memval st x with
            | Pointer (envval, _) -> getenval_type envval
            | String _ -> getenval_type (var_enval st x)
            | _ -> raise (TypeError "Pushstr")
          in
          if l_ty = Mutable then
            let st' =
              if check_borrow_all_mut_rec st (var_enval st x) then
                match var_enval st x with
                | SVar _ ->
                    let xs = string_of_var_val st x in
                    let s' = string_from st s in
                    assign st x (StringConst (xs ^ s'))
                | PointerVar _ ->
                    let xs = string_of_var_val st x in
                    let s' = string_from st s in
                    assign_to_original_owner st x (StringConst (xs ^ s'))
                | _ -> raise (TypeError "Pushstr")
              else raise (DataRace (x, Mutable, Immutable))
            in
            St st'
          else raise (CannotMutate x)
      (*Condizionale *)
      | If (e, c1, c2) -> (
          match eval_expr st e with
          | st', Bool b -> if b then Cmd (c1, st') else Cmd (c2, st')
          | _ -> raise (TypeError "If "))
      | Loop c -> Cmd (ExecLoop (c, Loop c), st)
      | ExecLoop (c, l) -> (
          try
            let conf' = trace_cmd (Cmd (c, st)) in
            match conf' with
            | St st' -> Cmd (l, st')
            | Cmd (c', st') -> Cmd (ExecLoop (c', l), st')
          with NotInLoop -> St st)
      | Break -> raise NotInLoop
      (*Stampa*)
      | Println s ->
          let s' = string_from st s in
          let st' = setout st (getout st ^ s' ^ "\n") in
          St st'
      (*Blocco di dichiarazioni*)
      | DeclBlock c ->
          let new_state = setenv_stack st (push_env st (topenv st)) in
          Cmd (ExecDeclBlock c, new_state)
      | Proc (p, el) ->
          (*lista parametri funzione*)
          let n, c, pl =
            match var_memval st p with
            | Proc p -> (p.name, p.body, p.params)
            | Func f -> (f.name, f.body, f.params)
            | _ -> raise (TypeError "Proc")
          in
          (* pusho nuovo env sullo stack *)
          let new_env_stack = push_env st env0 in
          let new_state = setenv_stack st new_env_stack in
          (* creo nuovo stato *)
          let new_state' = consecutive_decl el pl st new_state in
          (*lancio body funzione*)
          if String.equal n "main" then Cmd (Block c, new_state')
          else Cmd (ExecDeclBlock c, new_state')
      | ExecDeclBlock c -> (
          let conf' = trace_cmd (Cmd (c, st)) in
          match conf' with
          | St st' -> St (setenv_stack st' (popenv st'))
          | Cmd (c1', st') -> Cmd (ExecDeclBlock c1', st')))

and trace_rec gas n conf =
  if n >= gas then raise (OutOfGas n)
  else
    try
      let conf' = trace_cmd conf in
      conf :: trace_rec gas (n + 1) conf'
    with NoRuleApplies -> [ conf ]

type trace_error =
  | TypeError of string
  | UnboundVar of ide
  | CannotMutate of ide
  | MovedValue of ide
  | MutBorrowOfNonMut of ide
  | DataRace of ide * mut * mut
  | OutOfGas of int
  | NotInLoop
  | OtherErrors of string

type output = string
type result = Ok of output | Error of trace_error

let trace gas p =
  let p_conf = Cmd (p, state0) in
  try
    let confl = trace_rec gas 0 p_conf in
    let st =
      match last confl with
      | St st -> st
      | Cmd (_, st) -> st
    in
    let output = getout st in
    (confl, Ok output)
  with
  | TypeError s -> ([ St state0 ], Error (TypeError s))
  | UnboundVar x -> ([ St state0 ], Error (UnboundVar x))
  | CannotMutate x -> ([ St state0 ], Error (CannotMutate x))
  | MovedValue x -> ([ St state0 ], Error (MovedValue x))
  | MutBorrowOfNonMut x -> ([ St state0 ], Error (MutBorrowOfNonMut x))
  | DataRace (x, m1, m2) -> ([ St state0 ], Error (DataRace (x, m1, m2)))
  | OutOfGas g -> ([ St state0 ], Error (OutOfGas g))
  | NotInLoop -> ([ St state0 ], Error NotInLoop)
  | exn -> ([ St state0 ], Error (OtherErrors (Printexc.to_string exn)))

let trace_debug n p =
  let p_conf = Cmd (p, state0) in
  trace_rec n 0 p_conf
