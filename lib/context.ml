(*
- compile
- typing
  - typing.mli
  - implement for expr
  - implement for cast
- interpret
  - implement for cast
*)

(* Associative list mapping idents to elements *)
type 'a t = (Ident.t * 'a) list

let empty = []

exception IdentNotFound

let rec index (ctxt : 'a t) ident_name =
  match ctxt with
  | hd :: tl ->
      let key = fst hd in
      if key.name = ident_name then key else index tl ident_name
  | [] ->
      raise IdentNotFound

let extend ctxt ident v = (ident, v) :: ctxt

(* NB: Bad time complexity, but its a prototype so don't care ;) *)
let lookup ctxt ident = snd (List.find (fun el -> fst el = ident) ctxt)
