open Printf

let usage_msg = "gdmeow <file> [<file>] ..."

let input_files = ref []

let speclist = []

let anon_fun filename = input_files := filename :: !input_files

let () =
  Arg.parse speclist anon_fun usage_msg ;
  if List.length !input_files = 0 then (
    printf "Missing input files\n" ;
    printf "%s\n" usage_msg ;
    exit (-1) )
  else
    List.iter
      (fun file ->
        let fd = open_in file in
        let lexbuf = Lexing.from_channel fd in
        let expr, _ty =
          Gradualmeow.Parser.main Gradualmeow.Lexer.token lexbuf
        in
        let compiled, ty' =
          Gradualmeow.Input.lower expr |> Gradualmeow.Expr.lower
        in
        printf "%s\n" (Gradualmeow.Cast.string_of_expr compiled) ;
        printf "%s\n" (Gradualmeow.Types.string_of_ty ty') ;
        let result = Gradualmeow.Eval.eval compiled in
        printf "%s\n" (Gradualmeow.Cast.string_of_expr result) )
      !input_files
