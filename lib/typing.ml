open Expr

type ty_err =
  (* expression, actual type, expected *)
  | TypeMismatch of expr * ty * string
  (* left type, right type *)
  | InconsistentTypes of ty * ty
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
let rec tysynth expr ctxt =
  match expr with
  | EVar x ->
      Hashtbl.find ctxt x
  | EConst c -> (
    match c with
      | CInt _ -> TGround TInt 
      | CBool _ -> TGround TBool )
  | ELambda ((x, ty), body) ->
      Hashtbl.add ctxt x ty ;
      let ty' = tysynth body ctxt in
      Hashtbl.remove ctxt x ;
      TArrow (ty, ty')
  | EApp (e1, e2) -> (
      let e1_ty = tysynth e1 ctxt in
      let e2_ty = tysynth e2 ctxt in
      match e1_ty with
      | TArrow (ty, ty') ->
          if consistent e2_ty ty then ty'
          else raise (TypeError (InconsistentTypes (e2_ty, ty)))
      | _ ->
          raise (TypeError (TypeMismatch (e1, e1_ty, "arrow"))) )
  | ERef e -> TRef (tysynth e ctxt)
  | EDeref e -> (
      let e_ty = tysynth e ctxt in
      match e_ty with
      | TUnknown -> TUnknown
      | TRef ty -> ty
      | _ -> raise (TypeError (TypeMismatch (e, e_ty, "ref"))) )
  | EAssign (e1, e2) -> (
      let e1_ty = tysynth e1 ctxt in
      let e2_ty = tysynth e2 ctxt in
      match e1_ty with
      | TUnknown -> TRef e2_ty
      | TRef ty ->
          if consistent e2_ty ty then
            TRef ty
          else
            raise (TypeError (InconsistentTypes (e2_ty, ty)))
      | _ -> raise (TypeError Adhoc)
  )
