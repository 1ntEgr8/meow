type location = int

type 'a t = (location, 'a) Hashtbl.t

let free_locations = ref []

let counter = ref 0

let empty () = Hashtbl.create 10

(** Adds `x` to the store and returns the location *)
let alloc store x =
  let loc =
    match !free_locations with
    | [] ->
        counter := !counter + 1 ;
        !counter
    | hd :: tl ->
        free_locations := tl ;
        hd
  in
  Hashtbl.add store loc x ; loc

let find store l = Hashtbl.find store l

let set store l v = Hashtbl.replace store l v

let string_of_loc = string_of_int
