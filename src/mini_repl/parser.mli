type token =
  | TAB
  | NEWLINE
  | POUND
  | EOF
  | NAME of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.str
