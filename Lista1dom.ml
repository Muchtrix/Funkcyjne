(* Wiktor Adamski*)

(* Zadanie 1 *)
let fun1 (x,y) = (x + 1, y + 1);;
fun1 (14, 12);;

let fun2 (x, y) = x = y +. 0.1;;
fun2 (0.12, 0.12);;
fun2 (0.1, 0.0);;

let rec fun3 (li, a) = if a = 0 then [] else li @ fun3 (li, a - 1);;
fun3 ([1 ; 2 ; 3], 3);;
fun3 ([], 4);;
fun3 ([1; 2], 0);;

(* Zadanie 2 *)
let ends li = 
    match li with
    | []      -> failwith "Pusta lista"
    | _       -> let rec kon l = if List.tl l = [] then List.hd l 
                 else kon (List.tl l)
    in (List.hd li, kon li);;
ends [1;2;3;4;5];;
ends [1];;
ends [];;

(* Zadanie 3 *)
let isSorted li = 
    let rec isSorted' a li =
        match li with
        |   x::y        -> if a <= x then isSorted' x y else false
        |   []          -> true
    in if List.length li = 0 then true
                             else isSorted' (List.hd li) (List.tl li);;

isSorted [1;2;3;4];;
isSorted [4;3;2;1];;
isSorted [];;

(* Zadanie 4 *)
let powers (b, p) =
    let rec powList b p act cnt =
        if p = cnt then [act]
        else act :: powList b p (act * b) (cnt + 1)
    in powList b p 1 0;; 

powers (2, 3);;
powers (5, 0);;

(* Zadanie 5 *)
let split (li, a) = (List.filter (fun x -> x <= a) li, List.filter (fun x -> x > a) li);;

split(['a';'s';'h';'g'], 'g');;
split([], 18);;

(* Zadanie 6 *)
let segments (li, a) = 
    let rec aux li act acc a actl = 
        match li with
        |   []      ->  (List.rev act)::acc
        |   x::y    ->  if actl < a then aux y (x::act) acc a (actl + 1)
                        else aux li [] ((List.rev act)::acc ) a 0         
    in List.rev (aux li [] [] a 0);;

segments([1;2;3;4;5;6;7;8;9], 2);;

(* Zadanie 7 *)
let swap (li, num) = 
    let rec aux li acc num = 
        if num > 0 then aux  (List.tl li)  ((List.hd li)::acc) (num - 1)
        else li @ (List.rev acc)
    in  if num > List.length li || num < 0 then failwith "Nieprawidlowa wartosc podzialu" 
        else aux li [] num;;

swap (["a"; "b"; "5"; "6"], 2);;
swap ([1;2;3;4;5], 5);;
swap ([], 0);;
swap ([1;2;3], 4);;