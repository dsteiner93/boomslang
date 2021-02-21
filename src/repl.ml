(* How to use this primitive REPL to test:

   Set export OCAMLRUNPARAM='p' in your shell.
   This will show interesting diagnostic info from the shift/reduce
   tables generated by the parser. (See chapter 4 of the dragon
   book for info on how the parser generated by YACC works.)

   Then, run ./repl and type different programs to see how
   they got tokenized. If you enter a valid expression, the
   program should end with "Passed" when you hit ctrl-D. If you
   enter an invalid program, it will give you a parse error.
 *)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let _ = Parser.program Scanner.tokenize lexbuf in
  print_endline "Passed"
