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

module Tc = struct
  type t = expr

  type context = Types.t TypingContext.t

  let typeof expr ctxt store =
    let rec helper expr ctxt store =
      match expr with
      | CVar x ->
          TypingContext.lookup ctxt x.name
      | CConst c ->
          Types.typeof_constant c
      | CLambda ((x, ty), body) ->
          let ctxt' = TypingContext.extend ctxt x ty in
          let ty' = helper body ctxt' store in
          TArrow (ty, ty')
      | CApp (e1, e2) -> (
          let e1_ty = helper e1 ctxt store in
          let e2_ty = helper e2 ctxt store in
          match e1_ty with
          | TArrow (ty, ty') ->
              if e2_ty = ty then ty' else failwith "type mismatch"
          | _ ->
              failwith "expected arrow type" )
      | CCast (ty, e) ->
          let sigma = helper e ctxt store in
          if Types.consistent sigma ty then ty
          else failwith "expected types to be consistent"
      | CRef e ->
          let ty = helper e ctxt store in
          TRef ty
      | CDeref e -> (
          let ty = helper e ctxt store in
          match ty with TRef ty' -> ty' | _ -> failwith "expected ref type" )
      | CAssign (e1, e2) -> (
          let e1_ty = helper e1 ctxt store in
          let e2_ty = helper e2 ctxt store in
          match e1_ty with
          | TRef ty' ->
              if ty' = e2_ty then e1_ty else failwith "type mismatch"
          | _ ->
              failwith "expected ref type" )
      | CLoc l -> (
        let e = Store.find store l in
        let ty = helper e ctxt store in
        TRef ty )
    in
    helper expr ctxt store
end
