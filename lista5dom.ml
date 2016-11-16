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
    | (ile, pos, LCons (a, rs)) -> LCons (a,fun()-> auxRep (ile-1) pos li)
  in auxRep (fn 0) 0 li;;

ltake (10, lrepeat (fun i -> i+1) LNil);;
ltake (10, lrepeat (fun i -> i+1) (LCons(1, fun ()-> LCons(2, fun()->LNil))));;
ltake (20, lrepeat (fun i -> i+3) (lfrom 1));;

(* Zadanie 2 *)
let lremove xs li =
  let rec auxRem pos xs li =
    match (pos, xs, li) with
    | (_, [], li)                 -> li
    | (_, _, LNil)                -> LNil
    | (pos, x::xs, LCons (_, rs)) when pos = x -> auxRem (pos+1) xs (rs())
    | (pos, xs, LCons(a, rs))     -> LCons(a, fun()->auxRem (pos+1) xs (rs()))
  in auxRem 0 (List.sort (-) xs) li;;

ltake (10, (lremove [] (lfrom 10)));;
ltake (10, (lremove [1;2;4] LNil));;
ltake (10, (lremove [0] (LCons(1, fun ()-> LCons(2, fun()->LNil)))));;
ltake (10, (lremove [1;4;7;2] (lfrom 10)));;

(* Zadanie 3 *)
(* Uwaga: przedstawione rozwiązanie działa tylko dla list skończonych*)
(*        dla list nieskończonych dostaniemy błąd przepełnienia stosu*)

let splitLL li =
  let rec lfilter f li =
    match li with
    | LNil          -> LNil
    | LCons (a, rs) -> if f a then LCons (a, fun()->lfilter f (rs()))
                              else lfilter f (rs())
  in (lfilter (fun i -> i >= 0) li, lfilter (fun i -> i < 0) li);;

splitLL @@ LCons(1,  fun () -> LCons(2, fun () -> LNil));;
splitLL @@ LCons(-1, fun () -> LCons(0, fun () -> LCons(1, fun () -> LNil)));;

(* Zadanie 4 *)
let toLBST li =
  let rec auxLBST li lo hi =
    match li with
    | LNil          -> LEmpty
    | LCons (a, li) -> if lo < a && a < hi then LNode (a, lazy(auxLBST (li()) lo a), lazy(auxLBST (li()) a hi))
                                           else auxLBST (li()) lo hi
  in
  let rec auxLeft li hi =
    match li with
    | LNil          -> LEmpty
    | LCons (a, li) -> if a < hi then LNode (a, lazy(auxLeft (li()) a), lazy(auxLBST (li()) a hi))
                                 else auxLeft (li()) hi
  in
  let rec auxRight li lo =
    match li with
    | LNil          -> LEmpty
    | LCons (a, li) -> if lo < a then LNode (a, lazy(auxLBST (li()) lo a), lazy(auxRight (li()) a))
                                 else auxRight (li()) lo
  in match li with
    | LNil          -> LEmpty
    | LCons (a, li) -> LNode (a, lazy(auxLeft (li()) a), lazy(auxRight (li()) a))
    ;;

let rec traverse tree =
  match tree with
  | LEmpty                           -> []
  | LNode (a, lazy left, lazy right) -> (traverse left) @ [a] @ (traverse right);;

traverse @@ toLBST @@ LNil
  = [];;
traverse @@ toLBST @@ LCons(6, fun()->LCons(1, fun()->LCons(4, fun()->LCons(2, fun()->LCons(5, fun()->LCons(3, fun()->LNil))))))
  = [1;2;3;4;5;6];;