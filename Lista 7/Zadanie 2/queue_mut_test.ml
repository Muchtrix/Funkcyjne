(* Wiktor Adamski *)
(* Zadanie 2 *)
(* Program testujacy *)

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
  "Queue operations";
  "enqueue";
  "first";
  "dequeue";
  "isEmpty";
  "isFull";
  "quit testing"
|];;

let q = ref @@ Queue_mut.create 4;;
let quit = ref false;;
let choice = ref 7;;

while not !quit do
  begin
    choice := menu menuItems;
    match !choice with
      | 1 ->
      begin
        print_endline "Queue item = ";
        try Queue_mut.enqueue (read_int(), !q) with
            Queue_mut.Full m -> print_endline ("Exception: "^m)
      end
      | 2 ->
      begin
        try print_endline @@ string_of_int (Queue_mut.first !q)  with 
          Queue_mut.Empty m -> print_endline ("Exception: "^m)
      end
      | 3 ->
      begin
        Queue_mut.dequeue !q;
        print_endline "Dequeued";  
      end
      | 4 -> print_endline ("Queue is "^(if Queue_mut.isEmpty (!q) then "" else "not ")^"empty.");
      | 5 -> print_endline ("Queue is "^(if Queue_mut.isFull (!q) then "" else "not ")^"full.");
      | 6 -> quit := true;
      | _ -> print_endline "IMPOSSIBLE!!!" 
  end
done
