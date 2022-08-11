module Types = Types
module Parser = Parser
module Lexer = Lexer
module Context = Context

module Input = Input
module Expr = Expr
module Cast = Cast

let hello () = print_endline "hi there"

let typeof expr =
  let expr' = Input.lower expr in
  Expr.Tc.typeof expr' Context.empty

