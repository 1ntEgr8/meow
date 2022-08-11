open Printf

type ground_ty = TInt | TBool

type ty = TGround of ground_ty | TUnknown | TArrow of ty * ty | TRef of ty

type constant = CInt of int | CBool of bool

type binder = string * ty

(* TODO change to De-bruijn indexing *)
type expr =
  | EVar of string
  | EConst of constant
  | ELambda of binder * expr
  | EApp of expr * expr
  | ERef of expr
  | EDeref of expr
  | EAssign of expr * expr

let string_of_const c =
  match c with
  | CInt i -> string_of_int i
  | CBool b -> if b then "true" else "false"

let string_of_ground_ty g =
  match g with
  | TInt -> "int"
  | TBool -> "bool"

let rec string_of_ty ty =
  match ty with
  | TGround g -> string_of_ground_ty g
  | TUnknown -> "?"
  | TArrow (t1, t2) ->
      sprintf
        "( %s -> %s )"
        (string_of_ty t1)
        (string_of_ty t2)
  | TRef t -> sprintf "ref %s" (string_of_ty t)

let rec string_of_expr expr =
  match expr with
  | EVar x -> sprintf "%s" x
  | EConst c -> string_of_const c
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
    (string_of_ty ty)
    (string_of_expr body)
