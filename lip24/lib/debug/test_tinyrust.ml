open Tinyrust

let find_vars (trace : Types.conf list) varl =
  let lc = Types.last trace in
  let rec inner_fv (lc : Types.conf) varl =
    match varl with
    | [] -> true (* Se tutte le variabili sono state verificate, ritorna true *)
    | x :: varl' -> (
        (*Fare che parte dall'ultima anzi che dalla prima*)
        match lc with
        | St st -> (
            try
              let _ = Main.var_loc st x in
              let v = Main.var_memval st x in
              Printf.printf
                "Test passed: variable %s -> %s declared and assigned \
                 successfully.\n"
                x
                (Prettyprint.string_of_val v);
              inner_fv lc
                varl' (* Chiamata ricorsiva per la variabile successiva *)
            with ex ->
              Printf.printf "Unexpected exception: %s\n" (Printexc.to_string ex);
              false)
        | Cmd (_, st) -> (
            try
              let _ = Main.var_loc st x in
              let v = Main.var_memval st x in
              Printf.printf
                "Test passed: variable %s -> %s declared and assigned \
                 successfully.\n"
                x
                (Prettyprint.string_of_val v);
              inner_fv lc
                varl' (* Chiamata ricorsiva per la variabile successiva *)
            with ex ->
              Printf.printf "Unexpected exception: %s\n" (Printexc.to_string ex);
              false))
  in
  inner_fv lc varl

let test_var_declaration () =
  let program =
    " 
    fn main () {
      println!(\"Hello World!\");
    }
    "
  in
  try
    (*Successo parsing*)
    let ast = Main.parse program in
    let idel = Main.vars_of_cmd ast in
    Printf.printf "\nAST: %s\n\n" (Prettyprint.string_of_cmd ast);
    try
      (*Successo tracing*)
      let trace = Main.trace_debug 50 ast in

      Printf.printf "\n\nTRACE: \n%s\n\n"
        (Prettyprint.string_of_trace idel trace);

      true
    with
    (*Fallimento tracing*)
    | ex ->
      Printf.printf "Unexpected exception: %s\n" (Printexc.to_string ex);
      false
  with
  (*Fallimento parsing*)
  | Parser.Error ->
      Printf.printf "Parser error: failed to parse '%s'\n" program;
      false
  | Lexer.Error msg ->
      Printf.printf "Lexer error: %s\n" msg;
      false
  | ex ->
      Printf.printf "Unexpected exception: %s\n" (Printexc.to_string ex);
      false

(*
let%test "Variable declaration test" = test_var_declaration ()
*)
