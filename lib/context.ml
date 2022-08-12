module type T = sig
  type key

  type 'a t

  val empty : 'a t

  val index : 'a t -> key -> int

  val extend : 'a t -> key -> 'a -> 'a t

  val lookup : 'a t -> key -> 'a
end

module Make (C : Utils.Eq) : T with type key = C.t = struct
  type key = C.t

  type 'a t = (key * 'a) list

  let empty = []

  exception KeyNotFound

  let index ctxt k =
    let rec helper ctxt i =
      match ctxt with
      | (k', _) :: tl ->
          if C.equals k k' then i else helper tl (i + 1)
      | [] ->
          raise KeyNotFound
    in
    helper ctxt 0

  let extend ctxt k v = (k, v) :: ctxt

  (* NB: Bad time complexity, but its a prototype so don't care ;) *)
  let lookup ctxt k = snd (List.find (fun el -> C.equals (fst el) k) ctxt)
end

module TypingContext = Make (Utils.StringEq)
