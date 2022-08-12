open Cast

type err = CastError | TypeError | KillError | FatalError

exception EvalError of err

type boxed_value = expr * Types.t

let cats =
  ["😺"; "😸"; "😹"; "😻"; "😼"; "😽"; "🙀"; "😿"; "😾"]

let builtins =
  let tbl = Hashtbl.create 10 in
  Hashtbl.add tbl Constant.CSucc (fun e ->
      match e with
      | CConst (Constant.CInt i) ->
          CConst (Constant.CInt (i + 1))
      | _ ->
          failwith "bad argument" ) ;
  Hashtbl.add tbl Constant.CMeow (fun e -> e) ;
  tbl

(** n-place shift of an expression expr above cutoff c *)
let rec shift n c expr =
  match expr with
  | CVar ident ->
      if ident.scope < c then expr else CVar {ident with scope= ident.scope + n}
  | CConst _ | CLoc _ ->
      expr
  | CLambda (_, body) ->
      shift n (c + 1) body
  | CApp (e1, e2) ->
      CApp (shift n c e1, shift n c e2)
  | CRef e ->
      CRef (shift n c e)
  | CDeref e ->
      CDeref (shift n c e)
  | CAssign (e1, e2) ->
      CAssign (shift n c e1, shift n c e2)
  | CCast (ty, e) ->
      CCast (ty, shift n c e)

let rec subst expr (x : Ident.t) y =
  match expr with
  | CVar ident ->
      if ident.scope = x.scope then y else expr
  | CLambda (binder, body) ->
      let body' = subst body {x with scope= x.scope + 1} (shift 1 0 y) in
      CLambda (binder, body')
  | CConst _ | CLoc _ ->
      expr
  | CApp (e1, e2) ->
      CApp (subst e1 x y, subst e2 x y)
  | CRef e ->
      CRef (subst e x y)
  | CDeref e ->
      CDeref (subst e x y)
  | CAssign (e1, e2) ->
      CAssign (subst e1 x y, subst e2 x y)
  | CCast (ty, e) ->
      CCast (ty, subst e x y)

let unbox expr = match expr with CCast (_, expr) -> expr | _ -> expr

let eval expr =
  let rec helper expr store =
    match expr with
    | CConst Constant.CMeow ->
        (* Pick a random cat and print it ;) *)
        let n = Random.int (List.length cats) in
        let cat = List.nth cats n in
        Printf.printf "%s\n" cat ; expr
    | CConst _ | CLambda _ | CLoc _ ->
        expr
    | CCast (ty, e) -> (
        let v = unbox (helper e store) in
        (* TODO instead of invoking type checker, tag the AST with types *)
        let ty' = Tc.typeof v Context.TypingContext.empty in
        match ty with
        | TGround _ ->
            if ty = ty' then v else raise (EvalError CastError)
        | TUnknown ->
            CCast (TUnknown, v)
        | TRef _ ->
            if ty = ty' then v else raise (EvalError CastError)
        | TArrow (s, s') -> (
          match ty' with
          | TArrow (t, _) ->
              if Types.consistent ty ty' then
                let var_name = "meow" in
                CLambda
                  ( (var_name, s)
                  , CCast
                      (s', CApp (v, CCast (t, CVar {name= var_name; scope= 0})))
                  )
              else raise (EvalError CastError)
          | _ ->
              raise (EvalError CastError) ) )
    | CApp (e1, e2) -> (
        let v1 = helper e1 store in
        let v2 = helper e2 store in
        match (v1, v2) with
        | CConst c1, CConst _ ->
            let delta = Hashtbl.find builtins c1 in
            delta v2
        | CLambda ((x, _), body), _ ->
            let e3 = subst body {name= x; scope= 0} v2 in
            helper e3 store
        | _ ->
            raise (EvalError FatalError) )
    | CRef e ->
        let v = helper e store in
        let loc = Store.alloc store v in
        CLoc loc
    | CDeref e -> (
        let l = helper e store in
        match l with
        | CLoc l ->
            Store.find store l
        | _ ->
            raise (EvalError FatalError) )
    | CAssign (e1, e2) -> (
        let l = helper e1 store in
        let v = helper e2 store in
        match l with
        | CLoc l' ->
            Store.set store l' v ; l
        | _ ->
            raise (EvalError FatalError) )
    | _ ->
        failwith "unimplemented"
  in
  helper expr (Store.empty ())
