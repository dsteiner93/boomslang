open Ast

let rec eval = function
  Lit(a) -> a
| Concat(a, b) -> (eval a) ^ (eval b)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let str = Parser.program Scanner.tokenize lexbuf in
  let result = eval str in
  print_endline (result)

