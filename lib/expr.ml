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

module Tc = struct
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
      | EConst c -> Types.typeof_constant c
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

(** Lowers Expr.expr to (Cast.expr * Types.t) *)
let lower expr =
  let rec helper expr ctxt scope =
    match expr with
    | EVar x ->
        (Cast.CVar x, Tc.typeof expr ctxt)
    | EConst c ->
        (Cast.CConst c, Tc.typeof expr ctxt)
    | ELambda ((x, ty), body) ->
        let ctxt' = Context.extend ctxt {name= x; scope} ty in
        let body', ty' = helper body ctxt' (scope + 1) in
        (Cast.CLambda ((x, ty), body'), TArrow (ty, ty'))
    | EApp (e1, e2) -> (
        let e1', e1'_ty = helper e1 ctxt scope in
        let e2', e2'_ty = helper e2 ctxt scope in
        match e1'_ty with
        | TUnknown ->
            ( Cast.CApp (Cast.CCast (TArrow (e2'_ty, TUnknown), e1'), e2')
            , TUnknown )
        | TArrow (ty, ty') ->
            if e2'_ty <> ty then
              if Types.consistent e2'_ty ty then
                (Cast.CApp (e1', Cast.CCast (ty, e2')), ty')
              else failwith "Expected types to be consistent"
            else (Cast.CApp (e1', e2'), ty')
        | _ ->
            failwith "Expected either unknown or arrow type" )
    | ERef e ->
        let e', ty = helper e ctxt scope in
        (Cast.CRef e', TRef ty)
    | EDeref e -> (
        let e', ty = helper e ctxt scope in
        match ty with
        | TUnknown ->
            (Cast.CDeref (Cast.CCast (TRef TUnknown, e')), TUnknown)
        | TRef ty' ->
            (Cast.CDeref e', ty')
        | _ ->
            failwith "Expected unknown or ref type" )
    | EAssign (e1, e2) -> (
        let e1', e1'_ty = helper e1 ctxt scope in
        let e2', e2'_ty = helper e2 ctxt scope in
        match e1'_ty with
        | TUnknown ->
            (Cast.CAssign (Cast.CCast (TRef e2'_ty, e1'), e2'), TRef e2'_ty)
        | TRef ty ->
            if e2'_ty <> ty then
              if Types.consistent e2'_ty ty then
                (Cast.CAssign (e1', Cast.CCast (ty, e2')), TRef ty)
              else failwith "Expected types to be consistent"
            else (Cast.CAssign (e1', e2'), TRef ty)
        | _ ->
            failwith "Expected either unknown or ref type" )
  in
  helper expr Context.empty 0
