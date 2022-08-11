type t = {
  name: string;
  scope: int;
}

(* Compare idents by name *)
let eq_name i1 i2 = (i1.name = i2.name)

(* Compare idents by scope (binding location) *)
let eq_scope i1 i2 = (i1.scope = i2.scope)

(* Compare idents by name and scope *)
let eq i1 i2 = (eq_name i1 i2) && (eq_scope i1 i2)
