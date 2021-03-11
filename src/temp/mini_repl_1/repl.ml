open Ast

let rec eval = function
  Lit(a) -> a
| Concat(a, b) -> (eval a) ^ (eval b)



let _ =  
  
  let lexbuf = Lexing.from_channel stdin in
  let token_list = ref (Parser.test Scanner.tokenize lexbuf) in

  (* hack to convert a list of tokens to something ocamlyacc can read *) 
  let token lexbuf =
    match !token_list with
      | [] -> Parser.EOF
      | h::t -> token_list := t; h in

  let str = Parser.program token (Lexing.from_string "") in
  let result = eval str in
  print_endline (result)

