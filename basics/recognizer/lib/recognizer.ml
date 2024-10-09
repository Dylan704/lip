let lang1 l = 
  (List.length (List.filter(fun x -> x = '0' || x = '1' ) l) > 0) && (List.length (List.filter(fun x -> x != '0' && x != '1' ) l) = 0)
;;

let lang2 l = match l with 
    [] -> true 
  | a::l' -> if a = '0' then 
        List.length( List.filter(fun x -> x = '1') l') = List.length l'
      else if a = '1' then List.length( List.filter(fun x -> x = '1') l) = List.length l
      else false
;;

let first_l = function 
  a::l -> a
;;

let rec last_l = function 
  a::l -> last_l l
  | a::[] -> a
  | [] -> failwith"stringa vuota"
;;

let rec lang3 l = 
let lang4 _ = failwith ""

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
