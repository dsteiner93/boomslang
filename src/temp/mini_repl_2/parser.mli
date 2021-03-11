type token =
  | TAB
  | NEWLINE
  | INDENT
  | EOF
  | DEDENT
  | NINDENT
  | NAME of (string)
  | NDEDENTN of (int)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.str
val test :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (token * int) list
