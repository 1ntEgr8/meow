open Printf
open Context

type expr =
  | CVar of Ident.t
  | CConst of Constant.t
  | CLambda of (string * Types.t) * expr
  | CApp of expr * expr
  | CRef of expr
  | CDeref of expr
  | CAssign of expr * expr
  | CCast of Types.t * expr
  | CLoc of Store.location

let rec string_of_expr expr =
  match expr with
  | CVar x ->
      sprintf "%s" x.name
  | CConst c ->
      Constant.string_of_const c
  | CLambda (binder, body) ->
      string_of_lambda binder body
  | CApp (e1, e2) ->
      sprintf "( %s %s )" (string_of_expr e1) (string_of_expr e2)
  | CRef e ->
      sprintf "ref %s" (string_of_expr e)
  | CDeref e ->
      sprintf "!%s" (string_of_expr e)
  | CAssign (e1, e2) ->
      sprintf "( %s <- %s )" (string_of_expr e1) (string_of_expr e2)
  | CCast (ty, e) ->
      sprintf "( <%s> %s )" (Types.string_of_ty ty) (string_of_expr e)
  | CLoc loc ->
      sprintf "loc:%s" (Store.string_of_loc loc)

and string_of_lambda (x, ty) body =
  sprintf "( fun %s : %s . %s )" x (Types.string_of_ty ty) (string_of_expr body)

module Tc = struct
  type t = expr

  type context = Types.t TypingContext.t

  let typeof expr ctxt =
    let rec helper expr ctxt =
      match expr with
      | CVar x ->
          TypingContext.lookup ctxt x.name
      | CConst c ->
          Types.typeof_constant c
      | CLambda ((x, ty), body) ->
          let ctxt' = TypingContext.extend ctxt x ty in
          let ty' = helper body ctxt' in
          TArrow (ty, ty')
      | CApp (e1, e2) -> (
          let e1_ty = helper e1 ctxt in
          let e2_ty = helper e2 ctxt in
          match e1_ty with
          | TArrow (ty, ty') ->
              if e2_ty = ty then ty' else failwith "type mismatch"
          | _ ->
              failwith "expected arrow type" )
      | CCast (ty, e) ->
          let sigma = helper e ctxt in
          if Types.consistent sigma ty then ty
          else failwith "expected types to be consistent"
      | CRef e ->
          let ty = helper e ctxt in
          TRef ty
      | CDeref e -> (
          let ty = helper e ctxt in
          match ty with TRef ty' -> ty' | _ -> failwith "expected ref type" )
      | CAssign (e1, e2) -> (
          let e1_ty = helper e1 ctxt in
          let e2_ty = helper e2 ctxt in
          match e1_ty with
          | TRef ty' ->
              if ty' = e2_ty then e1_ty else failwith "type mismatch"
          | _ ->
              failwith "expected ref type" )
      | _ ->
          failwith
            "CLoc appeared during type-checking! Values of type CLoc can only \
             be produced at run-time."
    in
    helper expr ctxt
end
