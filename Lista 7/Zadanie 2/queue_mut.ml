(* Wiktor Adamski *)
(* Zadanie 2 *)
(* Struktura dla kolejek mutowalnych *)
type 'a t = {mutable f:int; mutable r:int; mutable n:int; mutable arr: 'a option array}
exception Empty of string
exception Full of string
let create x = {f = 0; r = 0; n = x; arr = Array.make x None}
let enqueue (x, q) = if q.f = (q.r + 1) mod q.n then raise @@ Full "module Queue_mut: enqueue"
                                                else q.arr.(q.r) <- Some x;
                                                     q.r <- (q.r + 1) mod q.n
let dequeue q = if q.f <> q.r then q.f <- (q.f + 1) mod q.n else ()
let first q = if q.f = q.r then raise @@ Empty "module Queue_mut: first"
                           else match q.arr.(q.f) with
                           | Some x -> x
                           | None   -> failwith "IMPOSSIBLE!!!"
let isEmpty q = q.r = q.f
let isFull q = q.f = (q.r + 1) mod q.n