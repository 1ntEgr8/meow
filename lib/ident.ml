(** Identifier type name: User provided name scope: De Bruijn index *)
type t = {name: string; scope: int}

(* Compare idents by name *)
module EqName = struct
  let equals i1 i2 = i1.name = i2.name
end

(* Compare idents by De Bruijn index *)
module EqIdx = struct
  let equals i1 i2 = i1.scope = i2.scope
end

(* Compare idents by name and De Bruijn index *)
module Eq = struct
  let equals i1 i2 = EqName.equals i1 i2 && EqIdx.equals i1 i2
end
