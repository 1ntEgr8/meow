open Printf
module Input = Input
module Types = Types
module Constant = Constant

let purr () = print_endline "purrrrr"

let meow cmd program =
  let expr, ty = program in
  let string_of_opt_ty ty =
    match ty with
    | Some ty ->
        Fmt.Types.string_of_ty ty
    | None ->
        Fmt.Types.string_of_ty Types.TUnknown
  in
  match cmd with
  | `DumpCast ->
      let expr', ty' = Input.lower expr |> Expr.lower in
      printf "%s : %s\n"
        (Fmt.Cast.string_of_expr expr')
        (Fmt.Types.string_of_ty ty')
  | `Typecheck -> (
      let expr' = Input.lower expr in
      let ty' = Expr.Tc.typeof expr' Context.TypingContext.empty in
      match ty with
      | Some ty ->
          if Types.consistent ty ty' then
            printf "%s\n" (Fmt.Types.string_of_ty ty')
          else raise (Eval.EvalError Eval.TypeError)
      | None ->
          printf "%s\n" (Fmt.Types.string_of_ty ty') )
  | `Eval ->
      let expr', ty' = Input.lower expr |> Expr.lower in
      let typechecks =
        match ty with Some ty -> Types.consistent ty ty' | None -> true
      in
      if typechecks then
        let result = Eval.eval expr' in
        printf "%s\n" (Fmt.Cast.string_of_expr result)
      else (
        printf "User-provided type '%s' doesn't match program type '%s'\n"
          (string_of_opt_ty ty)
          (Fmt.Types.string_of_ty ty') ;
        raise (Eval.EvalError Eval.TypeError) )

let parse_prog lexbuf = Parser.main Lexer.token lexbuf

let meow_from_string cmd s =
  let lexbuf = Lexing.from_string s in
  let program = parse_prog lexbuf in
  meow cmd program

let meow_from_fd cmd chan =
  let lexbuf = Lexing.from_channel chan in
  let program = parse_prog lexbuf in
  meow cmd program
