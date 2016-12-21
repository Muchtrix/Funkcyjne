(* Wiktor Adamski *)
(* Zadnanie 1 *)
module type QUEUE_FUN = 
sig
	type 'a t
	exception Empty of string
	val create: unit -> 'a t
	val enqueue: 'a * 'a t -> 'a t
	val dequeue: 'a t -> 'a t
	val first: 'a t -> 'a
	val isEmpty: 'a t -> bool
end;;

(* Zadanie 1 a *)
module Queue: QUEUE_FUN = 
struct
  type 'a t = EmptyQueue | Enqueue of 'a * 'a t
  exception Empty of string
  let create () = EmptyQueue
  let rec enqueue (elem, que) = 
    match que with 
    | EmptyQueue       -> Enqueue (elem, EmptyQueue)
    | Enqueue (x, que) -> Enqueue (x, enqueue (elem, que))
  let dequeue = function
    | EmptyQueue       -> EmptyQueue
    | Enqueue (_, que) -> que
  let first = function
    | EmptyQueue       -> raise (Empty "module Queue: first")
    | Enqueue (x, que) -> x
  let isEmpty queue = EmptyQueue = queue
end;;

(* Zadanie 1 b *)
module Queue_list: QUEUE_FUN =
struct
  type 'a t = 'a list
  exception Empty of string
  let create() = []
  let rec enqueue (elem, que) =
    match que with
    | []    -> [elem]
    | x::xs -> x:: enqueue (elem, xs)
  let dequeue = function
    | []    -> []
    | x::xs -> xs
  let first = function
    | []    -> raise (Empty "module Queue_list: first")
    | x::_  -> x
  let isEmpty queue = [] = queue
end;;

(* Zadanie 1 c *)
module Queue_struct: QUEUE_FUN =
struct
  type 'a t = 'a list * 'a list
  exception Empty of string
  let create() = ([],[])
  let normalize = function
    | ([], ys) -> (List.rev ys, [])
    | queue    -> queue
  let enqueue (elem, (xs, ys)) = normalize (xs, elem::ys)
  let dequeue = function
    | ([], _)     -> ([],[])
    | (x::xs, ys) -> normalize (xs, ys)
  let first = function
    | ([], _)     -> raise (Empty "module Queue_struct: first")
    | (x::_, _)   -> x
  let isEmpty queue = ([],[]) = queue
end;;

(* Program testujący *)
let menu opt = 
  let numItems = Array.length opt-1 in
  begin
    print_endline "\n\n===================================================";
    print_endline opt.(0);
    for i=1 to numItems do  print_int i; print_endline (". "^opt.(i)) done;
    print_string "\nSelect an option: ";
    flush stdout;
    let choice = ref (read_int())
    in 
      while !choice < 1 || !choice > numItems do 
        print_string ("Choose number between 1 and " ^ string_of_int numItems ^ ": ");
        choice := read_int();
      done; 
      !choice
    end;;

let menuItems = [|
  "Choose implementation";
  "Custom type implementation";
  "List implementation";
  "Double list implementation";
  "Quit testing"
|];;

let subMenuItems = [|
  "Queue operations";
  "enqueue";
  "first";
  "dequeue";
  "isEmpty";
  "quit testing"
|];;

let test_queue(crt, enq, fst, deq, emp) = 
  let q = ref @@ crt() in
  let quit = ref false in
  let choice = ref 7 in
  while not !quit do
    begin
      choice := menu subMenuItems;
      match !choice with
      | 1 ->
      begin
        print_endline "Queue item = ";
        q := enq (read_int(), !q);
      end
      | 2 ->
      begin
	      try print_endline @@ string_of_int (fst !q)  with 
		        Queue.Empty m | Queue_list.Empty m | Queue_struct.Empty m -> print_endline ("Exception: "^m);
	    end
      | 3 ->
      begin
        q := deq !q;
        print_endline "Dequeued";  
      end
      | 4 -> print_endline ("Queue is "^(if emp (!q) then "" else "not ")^"empty.");
      | 5 -> quit := true;
      | _ -> print_endline "IMPOSSIBLE!!!"
    end
  done;;


let quit = ref false;;
let choice = ref 7;;


while not !quit do
  begin
    choice := menu menuItems;
    match !choice with
    | 1 -> test_queue(Queue.create, Queue.enqueue, Queue.first, Queue.dequeue, Queue.isEmpty);
    | 2 -> test_queue(Queue_list.create, Queue_list.enqueue, Queue_list.first, Queue_list.dequeue, Queue_list.isEmpty);
    | 3 -> test_queue(Queue_struct.create, Queue_struct.enqueue, Queue_struct.first, Queue_struct.dequeue, Queue_struct.isEmpty);
    | 4 -> quit := true;
    | _ -> print_endline "IMPOSSIBLE!!!";
  end
done