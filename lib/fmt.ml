open Format

let indentation = 2

let kwd ppf s = fprintf ppf "%s" s

let ident ppf s = fprintf ppf "%s" s

let space ppf = fprintf ppf " "

let str_fmt pr x = pr str_formatter x ; flush_str_formatter ()

module Types = struct
  open Types

  let pr_ground_ty ppf g =
    match g with TInt -> kwd ppf "int" | TBool -> kwd ppf "bool"

  let rec pr_ty ppf ty =
    match ty with
    | TGround g ->
        pr_ground_ty ppf g
    | TUnknown ->
        kwd ppf "?"
    | TArrow (t1, t2) ->
        pp_open_hovbox ppf indentation ;
        kwd ppf "(" ;
        pr_ty ppf t1 ;
        space ppf ;
        kwd ppf "->" ;
        pp_print_space ppf () ;
        pr_ty ppf t2 ;
        kwd ppf ")" ;
        pp_close_box ppf ()
    | TRef t ->
        pp_open_hovbox ppf indentation ;
        kwd ppf "ref" ;
        space ppf ;
        pr_ty ppf t ;
        pp_close_box ppf ()

  let string_of_ground_ty = str_fmt pr_ground_ty

  let string_of_ty = str_fmt pr_ty
end

module Constant = struct
  open Constant

  let pr_const ppf c =
    match c with
    | CInt i ->
        fprintf ppf "%d" i
    | CBool b ->
        fprintf ppf "%b" b
    | CSucc ->
        kwd ppf "succ"
    | CMeow ->
        kwd ppf "😺😸😹😻😼😽🙀😿😾 meeoooow"

  let string_of_constant = str_fmt pr_const
end

module Cast = struct
  open Cast

  let rec pr_expr ppf expr =
    match expr with
    | CVar x ->
        ident ppf x.name
    | CConst c ->
        Constant.pr_const ppf c
    | CLoc l ->
        fprintf ppf "loc:%d" l
    | CLambda (binder, body) ->
        pr_lambda ppf binder body
    | CApp (e1, e2) ->
        pp_open_hovbox ppf indentation ;
        pr_expr ppf e1 ;
        pp_print_space ppf () ;
        pr_expr ppf e2 ;
        pp_close_box ppf ()
    | CRef e ->
        pp_open_hovbox ppf indentation ;
        kwd ppf "ref" ;
        space ppf ;
        pr_expr ppf e ;
        pp_close_box ppf ()
    | CDeref e ->
        pp_open_hovbox ppf indentation ;
        kwd ppf "!" ;
        pr_expr ppf e ;
        pp_close_box ppf ()
    | CAssign (e1, e2) ->
        pp_open_hovbox ppf indentation ;
        pr_expr ppf e1 ;
        space ppf ;
        kwd ppf "<-" ;
        pp_print_space ppf () ;
        pr_expr ppf e2 ;
        pp_close_box ppf ()
    | CCast (ty, e) ->
        pp_open_hovbox ppf indentation ;
        kwd ppf "(" ;
        pp_open_hovbox ppf indentation ;
        kwd ppf "<" ;
        Types.pr_ty ppf ty ;
        kwd ppf ">" ;
        pp_print_space ppf () ;
        pr_expr ppf e ;
        pp_close_box ppf () ;
        kwd ppf ")" ;
        pp_close_box ppf ()

  and pr_lambda ppf (x, ty) body =
    pp_open_vbox ppf indentation ;
    kwd ppf "(" ;
    pp_open_vbox ppf indentation ;
    kwd ppf "fun" ;
    space ppf ;
    ident ppf x ;
    space ppf ;
    kwd ppf ":" ;
    space ppf ;
    Types.pr_ty ppf ty ;
    kwd ppf "." ;
    pp_print_space ppf () ;
    pr_expr ppf body ;
    pp_close_box ppf () ;
    kwd ppf ")" ;
    pp_close_box ppf ()

  let string_of_expr = str_fmt pr_expr
end
