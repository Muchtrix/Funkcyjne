(* Wiktor Adamski *)



(* Zadanie 1 *)

(* Stara, nieefektywna wersja *)
(*
let rec store tr = 
  match tr with
  | L x        -> [Some x]
  | N (lt, rt) -> store lt @ store rt @ [None];;
*)

type 'a tree = L of 'a | N of 'a tree * 'a tree;;

let store tr =
  let rec aux = function
    | (acc, L x)        -> Some x :: acc
    | (acc, N (lt, rt)) -> None :: aux (aux (acc, lt), rt)
  in List.rev @@ aux ([], tr);;

store @@ L 14;;
store @@ N(L 13, L 14);;
store @@ N(N(L 13, L 14), N(L 13, L 14));;

(* Zadanie 2 *)
let rec load li = 
  let rec loadAux = function
    | ([x], [])                                  -> x
    | ( _ , []) | ([], None::_) | ([_], None::_) -> failwith "Invalid argument"
    | (a::b::trs, None::xs)                      -> loadAux (N(b,a)::trs, xs)
    | (st, Some x ::xs)                          -> loadAux (L x ::st, xs)
  in loadAux ([], li);;

load @@ store @@ L 14 = L 14;;
load @@ store @@ N(L 13, L 14) = N(L 13, L 14);;
load @@ store @@ N(N(L 13, L 14),N(L 13, L 14)) = N(N(L 13, L 14),N(L 13, L 14));;
load [];;
load [None];;

