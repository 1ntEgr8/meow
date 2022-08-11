{
  open Parser

  exception Eof
}

let white = ' ' | '\t'
let newline = '\n' | '\r' | "\r\n"
let var = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit = ['0'-'9']
let int = digit+

rule token = parse
  | white { token lexbuf }
  | newline { token lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "fun" { FUN }
  | "int" { TYINT }
  | "bool" { TYBOOL }
  | "ref" { REF }
  | "succ" { SUCC }
  | '!' { BANG }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ':' { COLON }
  | '?' { UNKNOWN }
  | '.' { DOT }
  | "<-" { ASSIGN }
  | "->" { ARROW }
  | var as lxm { VAR (lxm) }
  | int as lxm { INT (int_of_string lxm) }
  | eof { EOF }
