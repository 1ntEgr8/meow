open Parser

exception Eof

let white = [%sedlex.regexp? ' ' | '\t']

let newline = [%sedlex.regexp? '\n' | '\r' | "\r\n"]

let digit = [%sedlex.regexp? '0' .. '9']

let lower = [%sedlex.regexp? 'a' .. 'z']

let upper = [%sedlex.regexp? 'A' .. 'Z']

let var =
  [%sedlex.regexp? lower | upper | '_', Star lower | upper | digit | '_']

let int = [%sedlex.regexp? Plus digit]

let cats =
  [%sedlex.regexp?
    ( "meow"
    | "😺"
    | "😸"
    | "😹"
    | "😻"
    | "😼"
    | "😽"
    | "🙀"
    | "😿"
    | "😾" )]

let rec token lexbuf =
  match%sedlex lexbuf with
  | white ->
      token lexbuf
  | newline ->
      token lexbuf
  | "true" ->
      TRUE
  | "false" ->
      FALSE
  | "fun" ->
      FUN
  | "int" ->
      TYINT
  | "bool" ->
      TYBOOL
  | "ref" ->
      REF
  | "succ" ->
      SUCC
  | '!' ->
      BANG
  | '(' ->
      LPAREN
  | ')' ->
      RPAREN
  | ':' ->
      COLON
  | '?' ->
      UNKNOWN
  | '.' ->
      DOT
  | "<-" ->
      ASSIGN
  | "->" ->
      ARROW
  | int ->
      INT (int_of_string (Sedlexing.Latin1.lexeme lexbuf))
  | cats ->
      MEOW
  | var ->
      VAR (Sedlexing.Latin1.lexeme lexbuf)
  | eof ->
      EOF
  | _ ->
      assert false
