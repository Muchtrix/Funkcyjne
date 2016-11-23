(* Wiktor Adamski *)
(* Zadanie 2 *)
(* Sygnatura dla kolejek modyfikowalnych*)

type 'a t
exception Empty of string
exception Full of string
val create: int -> 'a t
val enqueue: 'a * 'a t -> unit
val dequeue: 'a t -> unit
val first: 'a t -> 'a
val isEmpty: 'a t -> bool
val isFull: 'a t -> bool