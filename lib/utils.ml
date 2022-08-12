(* TODO prepend todo *)
let todo msg = failwith msg

(* TODO prepend panic, fail with custom error *)
let panic msg = failwith msg

module type Eq = sig
  type t

  val equals : t -> t -> bool
end

module StringEq : Eq with type t = string = struct
  type t = string

  let equals = String.equal
end
