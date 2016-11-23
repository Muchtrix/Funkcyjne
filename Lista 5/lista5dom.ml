(* Wiktor Adamski *)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
type 'a lBT = LEmpty | LNode of 'a * ('a lBT Lazy.t) * ('a lBT Lazy.t);;

let rec lfrom x = LCons (x, fun () -> lfrom (x + 1));;

let rec ltake = function
  | (0, _)           -> []
  | (_, LNil)        -> []
  | (n, LCons(x,xf)) -> x::ltake(n-1, xf());;

(* Zadanie 1 *)
let lrepeat fn li =
  let rec auxRep ile pos li =
    match (ile, pos, li) with
    | (  _,   _, LNil)          -> LNil
    | (  0, pos, LCons (_, rs)) -> auxRep (fn pos+1) (pos+1) (rs())
    | (ile, pos, LCons (a, rs)) -> LCons (a, fun () -> auxRep (ile-1) pos li)
  in auxRep (fn 0) 0 li;;

ltake (10, lrepeat (fun i -> i+1) LNil);;
ltake (10, lrepeat (fun i -> i+1) (LCons(1, fun () -> LCons(2, fun () -> LNil))));;
ltake (20, lrepeat (fun i -> i+3) (lfrom 1));;

(* Zadanie 2 *)
let lremove xs li =
  let rec auxRem pos xs li =
    match (pos, xs, li) with
    | (_, [], li)                 -> li
    | (_, _, LNil)                -> LNil
    | (pos, x::xs, LCons (_, rs)) when pos = x -> auxRem (pos+1) xs (rs())
    | (pos, xs, LCons(a, rs))     -> LCons(a, fun () -> auxRem (pos+1) xs (rs()))
  in auxRem 0 (List.sort (-) xs) li;;

ltake (10, (lremove [] (lfrom 10)));;
ltake (10, (lremove [1;2;4] LNil));;
ltake (10, (lremove [0] (LCons(1, fun () -> LCons(2, fun () -> LNil)))));;
ltake (10, (lremove [1;4;7;2] (lfrom 10)));;

(* Zadanie 3 *)
(* Uwaga: przedstawione rozwiązanie działa całkowicie tylko dla list skończonych *)
(*        dla nieskończonych list liczb naturalnych wystąpi błąd przepełnienia stosu *)

let splitLL li =
  let rec lfilter f li =
    match li with
    | LNil          -> LNil
    | LCons (a, rs) -> if f a then LCons (a, fun () -> lfilter f (rs()))
                              else lfilter f (rs())
  in (lfilter (fun i -> i >= 0) li, lfilter (fun i -> i < 0) li);;

let (a, b) = splitLL @@ LCons(1,  fun () -> LCons(2, fun () -> LNil)) in (ltake (10,a), ltake (10,b));;
let (a, b) = splitLL @@ LCons(-1, fun () -> LCons(0, fun () -> LCons(1, fun () -> LNil))) in (ltake (10,a), ltake (10,b));;
let (a, b) = splitLL @@ lfrom (-5) in (ltake (10, a), ltake (10, b));;

(* Zadanie 4 *)
(* Wersja przerobiona z list leniwych*)
(*let toLBST li =
  let rec auxLBST li lo hi =
    match li with
    | []    -> LEmpty
    | x::xs -> if lo < x && x < hi then LNode (x, lazy(auxLBST xs lo x), lazy(auxLBST xs x hi))
                                           else auxLBST xs lo hi
  in
  let rec auxLeft li hi =
    match li with
    | []    -> LEmpty
    | x::xs -> if x < hi then LNode (x, lazy(auxLeft xs x), lazy(auxLBST xs x hi))
                                 else auxLeft xs hi
  in
  let rec auxRight li lo =
    match li with
    | []    -> LEmpty
    | x::xs -> if lo < x then LNode (x, lazy(auxLBST xs lo x), lazy(auxRight xs x))
                                 else auxRight xs lo
  in match li with
    | []    -> LEmpty
    | x::xs -> LNode (x, lazy(auxLeft xs x), lazy(auxRight xs x))
    ;;*)

(* Wersja poprawna *)
let toLBST li =
  let rec insert x tr =
    match tr with
    |LEmpty -> LNode (x, lazy LEmpty, lazy LEmpty)
    |LNode(a, lazy l, lazy r) -> if a < x then LNode (a, lazy l, lazy(insert x r))
				 else LNode (a, lazy (insert x l), lazy r)
  in List.fold_left (fun tr x -> insert x tr) LEmpty li;;

let rec traverse tree =
  match tree with
  | LEmpty                           -> []
  | LNode (a, lazy left, lazy right) -> (traverse left) @ [a] @ (traverse right);;

traverse @@ toLBST @@ [];;
traverse @@ toLBST @@ [6;3;4;2;5;1];;

