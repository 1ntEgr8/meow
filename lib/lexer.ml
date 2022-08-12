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
    | 0x1F638 (* 😸 *)
    | 0x1F639 (* 😹 *)
    | 0x1F63A (* 😺 *)
    | 0x1F63B (* 😻 *)
    | 0x1F63C (* 😼 *)
    | 0x1F63D (* 😽 *)
    | 0x1F63E (* 😾 *)
    | 0x1F63F (* 😿 *)
    | 0x1F640 (* 🙀 *)
  )]

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
      INT (int_of_string (Sedlexing.Utf8.lexeme lexbuf))
  | cats ->
      MEOW
  | var ->
      VAR (Sedlexing.Utf8.lexeme lexbuf)
  | eof ->
      EOF
  | _ ->
      Printf.printf "%s\n" (Sedlexing.Utf8.lexeme lexbuf) ;
      assert false
