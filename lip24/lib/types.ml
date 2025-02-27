open Ast

let rec last = function
  | [] -> failwith "last on empty list"
  | [ x ] -> x
  | _ :: l -> last l

let rec rem_last = function
  | [] -> failwith "rem_last on empty list"
  | [ _ ] -> []
  | x :: l -> x :: rem_last l

let getloc_index (l : loc) =
  match l with
  | i, _ -> i

let setloc_index (l : loc) i =
  match l with
  | _, ty -> (i, ty)

let getloc_type (l : loc) =
  match l with
  | _, t -> t

let setloc_type (l : loc) ty =
  match l with
  | i, _ -> (i, ty)

let setenval_type (e : envval) ty =
  match e with
  | BVar l -> BVar (setloc_type l ty)
  | IVar l -> IVar (setloc_type l ty)
  | SVar l -> SVar (setloc_type l ty)
  | PVar l -> PVar (setloc_type l ty)
  | FVar l -> FVar (setloc_type l ty)
  | PointerVar l -> PointerVar (setloc_type l ty)

let getenval_type (e : envval) =
  match e with
  | BVar l -> getloc_type l
  | IVar l -> getloc_type l
  | SVar l -> getloc_type l
  | PVar l -> getloc_type l
  | FVar l -> getloc_type l
  | PointerVar l -> getloc_type l

let getenval_index (e : envval) =
  match e with
  | BVar l -> getloc_index l
  | IVar l -> getloc_index l
  | SVar l -> getloc_index l
  | PVar l -> getloc_index l
  | FVar l -> getloc_index l
  | PointerVar l -> getloc_index l

(*Valori di memoria: valori*)
type memval =
  | Bool of bool
  | Int of int
  | String of string
  | Proc of proc
  | Func of func
  | Pointer of pointer

(*Ambiente -> fun da identificativo a locazione*)
type env = (ide -> envval) * ide list

(*Memoria -> fun da locazione a valore*)
type stack_mem = loc_i -> memval
type heap_mem = loc_i -> memval

(*Stato*)
type state = {
  env_stack : env list; (*Stack di ambienti (uno per scope)*)
  stack : stack_mem; (*Memoria*)
  heap : heap_mem; (*Memoria heap per le stringhe*)
  stack_floc : loc_i; (*Prima locazione libera in memoria*)
  heap_floc : loc_i; (*Prima locazione libera in memoria*)
  output : string;
}
(*restituisce loc di una variabile *)

(*Funzione di creazione dello stato*)
let make_state es s h sfl_i hfl_i o =
  let new_st =
    {
      env_stack = es;
      stack = s;
      heap = h;
      stack_floc = sfl_i;
      heap_floc = hfl_i;
      output = o;
    }
  in
  new_st

(*Funzioni di recupero degli attributi dello stato*)
let getenv_stack st = st.env_stack

let getstack st = st.stack

let getheap st = st.heap

let getstack_floc st = st.stack_floc

let getheap_floc st = st.heap_floc

let getout st = st.output

(*Funzioni di modifica degli attributi dello stato (restituiscono lo stato modificato)*)
let setenv_stack st es =
  let new_st =
    make_state es (getstack st) (getheap st) (getstack_floc st)
      (getheap_floc st) (getout st)
  in
  new_st

let setstack st s =
  let new_st =
    make_state (getenv_stack st) s (getheap st) (getstack_floc st)
      (getheap_floc st) (getout st)
  in
  new_st

let setheap st h =
  let new_st =
    make_state (getenv_stack st) (getstack st) h (getstack_floc st)
      (getheap_floc st) (getout st)
  in
  new_st

let setstack_floc st sfl =
  let new_st =
    make_state (getenv_stack st) (getstack st) (getheap st) sfl
      (getheap_floc st) (getout st)
  in
  new_st

let setheap_floc st hfl =
  let new_st =
    make_state (getenv_stack st) (getstack st) (getheap st) (getstack_floc st)
      hfl (getout st)
  in
  new_st

let setout st o =
  let new_st =
    make_state (getenv_stack st) (getstack st) (getheap st) (getstack_floc st)
      (getheap_floc st) o
  in
  new_st

(*Funzione di aggiornamento di un env con una nuova associzione (restituisce l'env aggiornato)*)
let update_env (old_env : env) new_ide new_loc : env =
  match old_env with
  | old_f, old_l ->
      ( (fun x -> if String.equal new_ide x then new_loc else old_f x),
        new_ide :: old_l )

(*Funzione di aggiornamento della mem con una nuova associzione (restituisce la mem aggiornata)*)
let update_stack old_stack new_loc_i new_val : stack_mem =
 fun loc_i -> if Int.equal new_loc_i loc_i then new_val else old_stack loc_i

let update_heap old_heap new_loc_i new_val : stack_mem =
 fun loc_i -> if Int.equal new_loc_i loc_i then new_val else old_heap loc_i

(*Funzione di recupero del primo env dall'envstack di uno stato*)
let topenv st =
  match getenv_stack st with
  | [] -> failwith "empty environment stack topenv"
  | firstenv :: _ -> firstenv

(*Funzione di rimozione del primo env dall'envstack di uno stato (restituisce l'envstack modificato)*)
let popenv st =
  match getenv_stack st with
  | [] -> failwith "empty environment stack popenv"
  | _ :: new_envstack -> new_envstack

(* aggiunge un nuovo ambiente in cima alla stack*)
let push_env st new_topenv =
  let new_envstack = new_topenv :: getenv_stack st in
  new_envstack

let swap_topenv st new_topenv =
  let new_envstack = new_topenv :: popenv st in
  new_envstack

let callerenv st =
  let callerenv_stack = popenv st in
  let callerenv_st = setenv_stack st callerenv_stack in
  topenv callerenv_st

let swap_callerenv st new_callerenv =
  (* Salviamo il topenv *)
  let topenv = topenv st in
  (* Rimuoviamo il topenv *)
  let callerenv_stack = popenv st in
  let callerenv_st = setenv_stack st callerenv_stack in
  let new_callerenv_stack = swap_topenv callerenv_st new_callerenv in
  let new_callerenv_st = setenv_stack callerenv_st new_callerenv_stack in
  (* Rinseriamo il topenv *)
  let new_envstack = push_env new_callerenv_st topenv in
  new_envstack

let globalenv st = last (getenv_stack st)

(* Exception *)
exception TypeError of string
exception UnboundVar of ide
exception UnboundLoc of loc_i
exception CannotMutate of ide
exception MovedValue of ide
exception MutBorrowOfNonMut of ide
exception DataRace of ide * mut * mut
exception OutOfGas of int
exception NotInLoop
exception NoRuleApplies

(*Creazione dello stato iniziale*)
let env0 : env = ((fun x -> raise (UnboundVar x)), [])

let env_stack0 = [ env0 ]

let stack0 : stack_mem = fun loc_i -> raise (UnboundLoc loc_i)

let heap0 : heap_mem = fun loc_i -> raise (UnboundLoc loc_i)

let state0 = make_state env_stack0 stack0 heap0 0 0 ""

type conf = St of state | Cmd of cmd * state

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | x :: l1' -> (if List.mem x l2 then [] else [ x ]) @ union l1' l2
