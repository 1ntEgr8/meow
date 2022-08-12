open Printf

let usage_msg = "gdmeow <file> [<file>] ..."

let input_files = ref []

let dump_cast = ref false

let typecheck = ref false

let speclist =
  [ ("-dump-cast", Arg.Set dump_cast, "Dump Cast AST")
  ; ("-typecheck", Arg.Set typecheck, "Only run typechecker") ]

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
        let cmd =
          if !dump_cast then `DumpCast
          else if !typecheck then `Typecheck
          else `Eval
        in
        Meow.meow_from_fd cmd fd )
      !input_files
