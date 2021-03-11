type token =
  | TAB
  | NEWLINE
  | POUND
  | EOF
  | NAME of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.str
val test :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> token list
