open Expr

(** Input AST, output by the parser. Variables are referred by their
    string names. *)
type expr =
  | IVar of string
  | IConst of Constant.t
  | ILambda of (string * Types.t) * expr
  | IApp of expr * expr
  | IRef of expr
  | IDeref of expr
  | IAssign of expr * expr

(** Lowers Input.expr to the Expr.expr *)
let lower expr =
  let rec helper expr ctxt depth =
    match expr with
    | IVar x -> EVar (Context.index ctxt x)
    | ILambda ((x, ty), body) ->
        let ctxt' = Context.extend ctxt { name = x; scope = depth } () in
        ELambda ((x, ty), helper body ctxt' (depth + 1))
    | IConst c -> EConst c
    | IApp (e1, e2) -> EApp (helper e1 ctxt depth, helper e2 ctxt depth)
    | IRef e -> ERef (helper e ctxt depth)
    | IDeref e -> EDeref (helper e ctxt depth)
    | IAssign (e1, e2) -> EAssign (helper e1 ctxt depth, helper e2 ctxt depth)
  in
    helper expr (Context.empty) 0
