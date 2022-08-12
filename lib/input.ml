open Expr

(** Input AST, output by the parser. Variables are referred by their string
    names. *)
type expr =
  | IVar of string
  | IConst of Constant.t
  | ILambda of (string * Types.t option) * expr
  | IApp of expr * expr
  | IRef of expr
  | IDeref of expr
  | IAssign of expr * expr

module NameContext = Context.Make (Utils.StringEq)

(** Lowers Input.expr to the Expr.expr *)
let lower expr =
  let rec helper expr ctxt =
    match expr with
    | IVar x ->
        let idx = NameContext.index ctxt x in
        EVar {name= x; scope= idx}
    | ILambda ((x, ty), body) ->
        let ctxt' = NameContext.extend ctxt x () in
        let ty =
          (match ty with
          | Some ty -> ty
          | None -> TUnknown)
        in
        ELambda ((x, ty), helper body ctxt')
    | IConst c ->
        EConst c
    | IApp (e1, e2) ->
        EApp (helper e1 ctxt, helper e2 ctxt)
    | IRef e ->
        ERef (helper e ctxt)
    | IDeref e ->
        EDeref (helper e ctxt)
    | IAssign (e1, e2) ->
        EAssign (helper e1 ctxt, helper e2 ctxt)
  in
  helper expr NameContext.empty
