(* Wiktor Adamski *)
(* Lista 7 zadanie 4 *)
(* Rozwiazanie kompiluje sie, ale nie zdazylem napisac testow *)

type ordering = LT | EQ | GT;;

module type ORDER =
sig
  type t
  val compare: t -> t -> ordering
end;;

module StringOrder : ORDER with type t = string =
struct
  type t = string
  let compare a b = if a=b then EQ else if a<b then LT else GT
end;;

module IntOrder : ORDER with type t = int =
struct
  type t = int
  let compare a b = if a=b then EQ else if a<b then LT else GT
end;;

module type DICTIONARY =
sig
  type key (* type of keys *)
  type 'a t (* type of dictionaries *)
  exception DuplicatedKey of key (* error in insert *)
  val empty: unit -> 'a t (* empty dictionary *)
  val lookup: 'a t -> key -> 'a option
  val insert: 'a t -> key * 'a -> 'a t
  val delete: 'a t -> key -> 'a t
  val update: 'a t -> key * 'a -> 'a t (* not necessary *)
end;;

module Dict (Key :ORDER) :DICTIONARY with type key = Key.t = 
struct
  type key = Key.t
  type 'a t = (key * 'a) list
  exception DuplicatedKey of key
  let empty () = []
  let rec lookup d k =
    match d with
    | []           -> None
    | (ke, va)::ds -> match Key.compare k ke with
		  | EQ -> Some va
		  | LT -> None
		  | GT -> lookup ds k
  let rec insert d (k, v) =
    match d with
    | []           -> [(k,v)]
    | (ke, va)::ds -> match Key.compare k ke with
		  | EQ -> raise (DuplicatedKey k)
		  | LT -> (k,v)::d
		  | GT -> (ke,va):: insert ds (k,v)
  let rec delete d k = 
    match d with
    | []          -> []
    |(ke, va)::ds -> match Key.compare k ke with
		  | EQ -> ds
		  | LT -> d
		  | GT -> (ke,va):: delete ds k
  let rec update d (k,v) =
    match d with
    | []           -> []
    | (ke, va)::ds -> match Key.compare k ke with
		  | EQ -> (k,v)::ds
		  | LT -> (k,v)::d
		  | GT -> (ke,va):: update ds (k,v)
end;;

module StringDict = Dict(StringOrder);;
module IntDict = Dict(IntOrder);;

let ( <| ) d (k,x) = StringDict.update d (k,x);;
let dict = StringDict.empty() 
	   <| ("kot","cat")
	   <| ("slon","elephant")
	   <| ("pies","dog")
	   <| ("ptak","bird");;
