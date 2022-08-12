type t = CInt of int | CBool of bool | CSucc

let string_of_const c =
  match c with
  | CInt i ->
      string_of_int i
  | CBool b ->
      if b then "true" else "false"
  | CSucc ->
      "succ"
