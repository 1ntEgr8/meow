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
  | EVar x -> sprintf "%s" x.name
  | EConst c -> Constant.string_of_const c
  | ELambda (binder, body) -> string_of_lambda binder body
  | EApp (e1, e2) -> 
      sprintf 
        "( %s %s )"
        (string_of_expr e1)
        (string_of_expr e2)
  | ERef e -> sprintf "ref %s" (string_of_expr e)
  | EDeref e -> sprintf "!%s" (string_of_expr e)
  | EAssign (e1, e2) ->
      sprintf
        "( %s <- %s )"
        (string_of_expr e1)
        (string_of_expr e2)
and string_of_lambda (x, ty) body =
  sprintf
    "( fun %s : %s . %s )"
    x
    (Types.string_of_ty ty)
    (string_of_expr body)
