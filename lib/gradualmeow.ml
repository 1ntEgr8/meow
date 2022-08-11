module Types = Types
module Parser = Parser
module Lexer = Lexer
module Context = Context

let hello () = print_endline "hi there"

let typeof expr =
  let expr' = Input.lower expr in
  Typing.tysynth expr' Context.empty