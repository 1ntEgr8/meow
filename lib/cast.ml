open Printf

type expr =
  | CVar of Ident.t
  | CConst of Constant.t
  | CLambda of (string * Types.t) * expr
  | CApp of expr * expr
  | CRef of expr
  | CDeref of expr
  | CAssign of expr * expr
  | CCast of Types.t * expr

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

and string_of_lambda (x, ty) body =
  sprintf "( fun %s : %s . %s )" x (Types.string_of_ty ty) (string_of_expr body)

module Tc = struct
  type t = expr

  type context = Types.t Context.t

  let typeof expr ctxt =
    let rec helper expr ctxt scope =
      match expr with
      | CVar x -> Context.lookup ctxt x
      | CConst c -> Types.typeof_constant c
      | CLambda ((x, ty), body) ->
          let ctxt' = Context.extend ctxt { name = x; scope = scope } ty in
          let ty' = helper body ctxt' (scope + 1) in
          TArrow (ty, ty')
      | CApp (e1, e2) ->
          let e1_ty = helper e1 ctxt scope in
          let e2_ty = helper e2 ctxt scope in
          (match e1_ty with
          | TArrow (ty, ty') ->
              if e2_ty = ty then
                ty'
              else
                failwith "type mismatch"
          | _ -> failwith "expected arrow type" )
      | CCast (ty, e) ->
            let sigma = helper e ctxt scope in
            if Types.consistent sigma ty then
              ty
            else
              failwith "expected types to be consistent"
        | CRef e ->
            let ty = helper e ctxt scope in
            TRef ty
        | CDeref e ->
            let ty = helper e ctxt scope in
            (match ty with
            | TRef ty' -> ty'
            | _ -> failwith "expected ref type" )
        | CAssign (e1, e2) ->
            let e1_ty = helper e1 ctxt scope in
            let e2_ty = helper e2 ctxt scope in
            (match e1_ty with
            | TRef ty' ->
                if ty' = e2_ty then
                  e1_ty
                else
                  failwith "type mismatch"
            | _ -> failwith "expected ref type" )
    in
      helper expr ctxt 0
end
