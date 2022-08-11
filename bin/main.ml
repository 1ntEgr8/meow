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
    exit (-1);
  ) else (
    List.iter (fun file ->
      let fd = open_in file in
      let lexbuf = Lexing.from_channel fd in
      let (expr, _ty) =
        Gradualmeow.Parser.main Gradualmeow.Lexer.token lexbuf
      in
      let ty' = Gradualmeow.typeof expr in
      printf "%s" (Gradualmeow.Types.string_of_ty ty')
    ) !input_files
  )
