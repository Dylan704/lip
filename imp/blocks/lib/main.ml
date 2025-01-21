open Ast
open Types

exception TypeError of string
exception UnboundVar of string
exception NoRuleApplies

(* Funzione ausiliaria per bind *)
let bind st id v = fun x -> if x = id then v else st x

(* Ambiente e memoria iniziali *)
let bottom = fun _ -> raise (UnboundVar "Access to uninitialized variable")

(* Parsing dell'input e generazione del comando *)
let parse (s : string) : conf =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  Cmd(ast, state0)

let extract_cmd = function
  | Cmd (c, _) -> c  (* Estrae solo il comando, ignorando lo stato *)
  | _ -> failwith "Expected Cmd"


(* Funzione ausiliaria per controllare il tipo booleano *)
let assert_bool = function
  | Bool b -> b
  | _ -> raise (TypeError "Expected a boolean value")

(* Funzione ausiliaria per controllare il tipo intero *)
let assert_int = function
  | Int i -> i
  | _ -> raise (TypeError "Expected an integer value")

(* Valutazione delle espressioni *)
let rec eval_expr (st : state) = function
  | True -> Bool true
  | False -> Bool false
  | Var id -> 
    let env = topenv st in
    let loc = try env id with Not_found -> raise (UnboundVar id) in
    (match loc with
     | BVar l -> (match getmem st l with
                  | Bool b -> Bool b
                  | _ -> raise (TypeError "Expected a Bool in memory"))
     | IVar l -> (match getmem st l with
                  | Int i -> Int i
                  | _ -> raise (TypeError "Expected an Int in memory")))

  | Const i -> Int i
  | Not e -> Bool (not (assert_bool (eval_expr st e)))
  | And (e1, e2) -> Bool ((assert_bool (eval_expr st e1)) && (assert_bool (eval_expr st e2)))
  | Or (e1, e2) -> Bool ((assert_bool (eval_expr st e1)) || (assert_bool (eval_expr st e2)))
  | Add (e1, e2) -> Int ((assert_int (eval_expr st e1)) + (assert_int (eval_expr st e2)))
  | Sub (e1, e2) -> Int ((assert_int (eval_expr st e1)) - (assert_int (eval_expr st e2)))
  | Mul (e1, e2) -> Int ((assert_int (eval_expr st e1)) * (assert_int (eval_expr st e2)))
  | Eq (e1, e2) -> Bool ((assert_int (eval_expr st e1)) = (assert_int (eval_expr st e2)))
  | Leq (e1, e2) -> Bool ((assert_int (eval_expr st e1)) <= (assert_int (eval_expr st e2)))

(* Valutazione delle dichiarazioni *)
let eval_decl st decls =
  let loc = ref (getloc st) in
  let new_env = List.fold_left (fun acc decl -> 
    match decl with
    | IntVar id -> 
        let l = !loc in
        loc := l + 1;
        bind acc id (IVar l)
    | BoolVar id -> 
        let l = !loc in
        loc := l + 1;
        bind acc id (BVar l)
  ) (topenv st) decls in
  let new_mem = List.fold_left (fun acc decl -> 
    match decl with
    | IntVar _ -> bind acc !loc (Int 0)
    | BoolVar _ -> bind acc !loc (Bool false)
  ) (getmem st) decls in
  setenv (setmem (setloc st !loc) new_mem) (new_env :: getenv st)

(* Traccia della semantica small-step *)
let rec trace1 = function
  (* Caso base: Skip
     Se il comando è Skip, non succede nulla e restituiamo lo stato invariato. *)
  | Cmd (Skip, st) -> St st

  (* Caso: Assign
     Se il comando è un'assegnazione (x := e), valutiamo l'espressione 'e' e
     assegniamo il valore della valutazione alla variabile 'x'. *)
  | Cmd (Assign (x, e), st) ->
    let v = eval_expr st e in  (* Valutiamo l'espressione 'e' nello stato corrente *)
    (match topenv st x with   (* Verifichiamo se la variabile 'x' è una variabile intera o booleana *)
     | IVar l -> St (setmem st (bind (getmem st) l v))  (* Assegniamo il valore alla variabile intera *)
     | BVar l -> St (setmem st (bind (getmem st) l v))) (* Assegniamo il valore alla variabile booleana *)

  (* Caso: Seq
     Se il comando è una sequenza di due comandi (c1; c2), prima eseguiamo 'c1',
     poi, a seconda del risultato, eseguiamo 'c2'. *)
  | Cmd (Seq (c1, c2), st) ->
    (match trace1 (Cmd (c1, st)) with  (* Eseguiamo il primo comando c1 *)
     | St st' -> Cmd (c2, st')  (* Se c1 è stato eseguito con successo, eseguiamo il comando c2 *)
     | Cmd (c1', st') -> Cmd (Seq (c1', c2), st'))  (* Se il comando c1 non ha prodotto uno stato finale, ricorsivamente rielaboriamo *)

  (* Caso: If
     Se il comando è una condizione If, prima valutiamo l'espressione della condizione,
     quindi eseguiamo il primo o il secondo ramo in base al risultato booleano. *)
  | Cmd (If (e, c1, c2), st) ->
    (match eval_expr st e with  (* Valutiamo l'espressione della condizione 'e' *)
     | Bool true -> Cmd (c1, st)  (* Se la condizione è vera, eseguiamo il ramo c1 *)
     | Bool false -> Cmd (c2, st)  (* Se la condizione è falsa, eseguiamo il ramo c2 *)
     | _ -> raise (TypeError "If condition must be boolean"))  (* Se la condizione non è un booleano, lanciamo un errore *)

  (* Caso: While
     Se il comando è un ciclo While, eseguiamo un'operazione simile a un If,
     ma invece di eseguire uno dei rami, ripetiamo l'esecuzione del ciclo fino a quando la condizione è falsa. *)
  | Cmd (While (e, c), st) ->
    Cmd (If (e, Seq (c, While (e, c)), Skip), st)  (* Costruiamo una sequenza che esegue 'c' e poi ricorsivamente il ciclo While *)

  (* Caso: Block
     Se il comando è un blocco di codice (racchiuso tra parentesi graffe),
     eseguiamo il comando all'interno di un nuovo stato con un ambiente temporaneo. *)
     | Cmd (Block c, st) ->
      let st' = setenv st (bottom :: getenv st) in  (* Creiamo un nuovo stato con un ambiente temporaneo (bottom) *)
      (match trace1 (Cmd (c, st')) with  (* Eseguiamo il comando all'interno di questo stato aggiornato *)
       | St st'' -> St (setenv st'' (popenv st''))  (* Quando il comando finisce, ripristiniamo l'ambiente e ritorniamo lo stato finale *)
       | Cmd (c', st'') -> Cmd (Block c', st''))  (* Se il blocco produce ancora un comando da eseguire, ritorniamo quel comando con il nuovo stato *)
  
  (* Caso: Decl
     Se il comando è una dichiarazione di variabili seguita da un comando,
     eseguiamo prima la dichiarazione delle variabili (aggiornando lo stato),
     poi eseguiamo il comando successivo. *)
  | Cmd (Decl (decls, c), st) ->
    (* Valutiamo la dichiarazione delle variabili nello stato corrente, ottenendo lo stato aggiornato *)
    let st' = eval_decl st decls in
    (* Dopo aver dichiarato le variabili, eseguiamo il comando 'c' con lo stato aggiornato *)
    trace1 (Cmd (c, st'))  (* Chiamata ricorsiva per eseguire il comando dopo la dichiarazione *)

  (* Caso di fallback: NoRuleApplies
     Se il comando non corrisponde a nessun caso sopra (un errore di sintassi o comando sconosciuto),
     lanciamo un'eccezione NoRuleApplies. *)
  | _ -> raise NoRuleApplies



(* Traccia n passi di esecuzione *)
let rec trace n conf =
  if n <= 0 then [conf]
  else
    try
      let conf' = trace1 conf in
      conf :: trace (n - 1) conf'
    with NoRuleApplies -> [conf]
