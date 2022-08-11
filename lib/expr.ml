open Printf

type expr =
  | EVar of Ident.t
  | EConst of Constant.t
  | ELambda of (string * Types.t) * expr
  | EApp of expr * expr
  | ERef of expr
  | EDeref of expr
  | EAssign of expr * expr

let rec string_of_expr expr =
  match expr with
  | EVar x ->
      sprintf "%s" x.name
  | EConst c ->
      Constant.string_of_const c
  | ELambda (binder, body) ->
      string_of_lambda binder body
  | EApp (e1, e2) ->
      sprintf "( %s %s )" (string_of_expr e1) (string_of_expr e2)
  | ERef e ->
      sprintf "ref %s" (string_of_expr e)
  | EDeref e ->
      sprintf "!%s" (string_of_expr e)
  | EAssign (e1, e2) ->
      sprintf "( %s <- %s )" (string_of_expr e1) (string_of_expr e2)

and string_of_lambda (x, ty) body =
  sprintf "( fun %s : %s . %s )" x (Types.string_of_ty ty) (string_of_expr body)

module Typing :
  Typing.Checker with type t = expr with type context = Types.t Context.t =
struct
  open Types

  type t = expr

  type context = Types.t Context.t

  type ty_err =
    (* expression, actual type, expected *)
    | TypeMismatch of expr * Types.t * string
    (* left type, right type *)
    | InconsistentTypes of Types.t * Types.t
    | Adhoc

  exception TypeError of ty_err

  (* TODO better error messages *)
  let typeof expr ctxt =
    let rec helper expr ctxt scope =
      match expr with
      | EVar x ->
          Context.lookup ctxt x
      | EConst c -> (
        match c with CInt _ -> TGround TInt | CBool _ -> TGround TBool )
      | ELambda ((x, ty), body) ->
          let ctxt' = Context.extend ctxt {name= x; scope} ty in
          let ty' = helper body ctxt' (scope + 1) in
          TArrow (ty, ty')
      | EApp (e1, e2) -> (
          let e1_ty = helper e1 ctxt scope in
          let e2_ty = helper e2 ctxt scope in
          match e1_ty with
          | TArrow (ty, ty') ->
              if consistent e2_ty ty then ty'
              else raise (TypeError (InconsistentTypes (e2_ty, ty)))
          | _ ->
              raise (TypeError (TypeMismatch (e1, e1_ty, "arrow"))) )
      | ERef e ->
          TRef (helper e ctxt scope)
      | EDeref e -> (
          let e_ty = helper e ctxt scope in
          match e_ty with
          | TUnknown ->
              TUnknown
          | TRef ty ->
              ty
          | _ ->
              raise (TypeError (TypeMismatch (e, e_ty, "ref"))) )
      | EAssign (e1, e2) -> (
          let e1_ty = helper e1 ctxt scope in
          let e2_ty = helper e2 ctxt scope in
          match e1_ty with
          | TUnknown ->
              TRef e2_ty
          | TRef ty ->
              if consistent e2_ty ty then TRef ty
              else raise (TypeError (InconsistentTypes (e2_ty, ty)))
          | _ ->
              raise (TypeError Adhoc) )
    in
    helper expr ctxt 0
end
