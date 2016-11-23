(* Wiktor Adamski *)

(* Zadanie 1 *)
let rec czyIstnieje f li = 
    match li with
    | []    -> false
    | x::xs -> f x || czyIstnieje f xs;;

czyIstnieje (fun x -> x = 2) [1;2;3;4;5];;
czyIstnieje (fun x -> x = 2) [1;3;4;5];;
czyIstnieje (fun x -> x = 2) [];;

let czyIstniejeLeft f li = List.fold_left (fun acc x -> acc || f x) false li;;

czyIstniejeLeft (fun x -> x = 2) [1;2;3;4;5];;
czyIstniejeLeft (fun x -> x = 2) [1;3;4;5];;
czyIstniejeLeft (fun x -> x = 2) [];;

let czyIstniejeRight f li = List.fold_right (fun x acc -> acc || f x) li false;;

czyIstniejeRight (fun x -> x = 2) [1;2;3;4;5];;
czyIstniejeRight (fun x -> x = 2) [1;3;4;5];;
czyIstniejeRight (fun x -> x = 2) [];;

(* Zadanie 2 *)
let filter f li = List.fold_right (fun x acc -> if f x then x :: acc else acc) li [];;

filter (fun x -> x mod 2 = 0) [1;2;3;4;5;6;7];;
filter (fun x -> x mod 2 = 0) [];;

(* Zadanie 3 *)
let rec usun1 f li =
    match li with
    | []    -> []
    | x::xs -> if f x then xs else x :: usun1 f xs;;

usun1 (fun x -> x = 2) [1;2;3;2;5];;
usun1 (fun x -> x = 2) [];;

let rec usun1tail f li =
    let rec aux f li acc =
        match li with
        | []    -> List.rev acc
        | x::xs -> if f x then List.rev_append acc xs else aux f xs (x::acc)
    in aux f li [];;

usun1tail (fun x -> x = 2) [1;2;3;2;5];;
usun1tail (fun x -> x = 2) [];;