(* Wiktor Adamski *)

(* Zadanie 1 *)
let rec fib1 n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib1 (n - 1) + fib1 (n - 2);;

let fib2 num = 
  let rec fib' n next act = 
    if n = num then act 
    else fib' (n + 1) (act + next) next
  in fib' 0 1 0;;

fib1 0;;
fib1 1;;
fib1 5;;

fib2 0;;
fib2 1;;
fib2 5;;

(*
Testy wykomentowane ze względu na powolność funkcji fib1

fib1 42;;
fib2 42;;
*)

(* Zadanie 2 *)
let (<->) (x1, x2, x3) (y1, y2, y3) =
  sqrt @@ (x1 -. y1)**2. +. (x2 -. y2)**2. +. (x3 -. y3)**2.;;

(1.,1.,1.) <-> (1.,1.,0.);;
(0.,0.,0.) <-> (0.,0.,0.);;

(* Zadanie 3 *)
let rec (<--) li elem = 
  match li with
  | []   -> [elem]
  | h::t -> if elem < h then elem :: li else h :: (t <-- elem);;

[1;3;5;5;7] <-- 2;;
[] <-- 1;;
[1;2;3] <-- 4;;

(* Zadanie 4 *)
let take num li =
  let rec take' num li acc = 
    match li with
    | []   -> List.rev acc
    | h::t -> if num = 0 then List.rev acc else take' (num - 1) t (h::acc)
  in if num < 0 then [] else take' num li [];;

take 2 [1;2;3;5;6];;
take (-2) [1;2;3;5;6];;
take 8 [1;2;3;5;6];;

(* Zadanie 5 *)
let drop num li = 
  let rec drop' num li = 
    match li with
    | []   -> []
    | _::t -> if num = 0 then li else drop' (num - 1) t
  in if num < 0 then li else drop' num li;;

drop 2 [1;2;3;5;6];;
drop (-2) [1;2;3;5;6];;
drop 8 [1;2;3;5;6];;
				 
