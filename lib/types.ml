open Printf

type ground_ty = TInt | TBool

type t = TGround of ground_ty | TUnknown | TArrow of t * t | TRef of t

let string_of_ground_ty g = match g with TInt -> "int" | TBool -> "bool"

let rec string_of_ty ty =
  match ty with
  | TGround g ->
      string_of_ground_ty g
  | TUnknown ->
      "?"
  | TArrow (t1, t2) ->
      sprintf "( %s -> %s )" (string_of_ty t1) (string_of_ty t2)
  | TRef t ->
      sprintf "ref %s" (string_of_ty t)

let rec consistent t1 t2 =
  match (t1, t2) with
  | _, TUnknown | TUnknown, _ ->
      true
  | TArrow (s1, s2), TArrow (t1, t2) ->
      consistent s1 t1 && consistent s2 t2
  | _ ->
      t1 = t2

let typeof_constant c =
  match c with
  | Constant.CInt _ -> TGround TInt
  | Constant.CBool _ -> TGround TBool
  | Constant.CSucc -> TArrow (TGround TInt, TGround TInt)
