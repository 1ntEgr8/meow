open Expr
open Types

type ty_err =
  (* expression, actual type, expected *)
  | TypeMismatch of expr * Types.t * string
  (* left type, right type *)
  | InconsistentTypes of Types.t * Types.t
  | TypeNotFound of expr
  | Adhoc

exception TypeError of ty_err

let rec consistent t1 t2 =
  match (t1, t2) with
  | _, TUnknown | TUnknown, _ ->
      true
  | TArrow (s1, s2), TArrow (t1, t2) ->
      consistent s1 t1 && consistent s2 t2
  | _ ->
      t1 = t2

(* TODO better error messages *)
let tysynth expr ctxt =
  let rec helper expr ctxt scope =
    match expr with
    | EVar x ->
        Context.lookup ctxt x
    | EConst c -> (
      match c with
        | CInt _ -> TGround TInt 
        | CBool _ -> TGround TBool )
    | ELambda ((x, ty), body) -> (
        let ctxt' = Context.extend ctxt { name = x; scope = scope } ty in
        let ty' = helper body ctxt' (scope + 1) in
        TArrow (ty, ty') )
    | EApp (e1, e2) -> (
        let e1_ty = helper e1 ctxt scope in
        let e2_ty = helper e2 ctxt scope in
        match e1_ty with
        | TArrow (ty, ty') ->
            if consistent e2_ty ty then ty'
            else raise (TypeError (InconsistentTypes (e2_ty, ty)))
        | _ ->
            raise (TypeError (TypeMismatch (e1, e1_ty, "arrow"))) )
    | ERef e -> TRef (helper e ctxt scope)
    | EDeref e -> (
        let e_ty = helper e ctxt scope in
        match e_ty with
        | TUnknown -> TUnknown
        | TRef ty -> ty
        | _ -> raise (TypeError (TypeMismatch (e, e_ty, "ref"))) )
    | EAssign (e1, e2) -> (
        let e1_ty = helper e1 ctxt scope in
        let e2_ty = helper e2 ctxt scope in
        match e1_ty with
        | TUnknown -> TRef e2_ty
        | TRef ty ->
            if consistent e2_ty ty then
              TRef ty
            else
              raise (TypeError (InconsistentTypes (e2_ty, ty)))
        | _ -> raise (TypeError Adhoc)
    )
  in
    helper expr ctxt 0
