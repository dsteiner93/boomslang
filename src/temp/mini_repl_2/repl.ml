open Ast

let rec eval = function
  Lit(a) -> a
| Concat(a, b) -> (eval a) ^ (eval b)



let _ =  
  
  let lexbuf = Lexing.from_channel stdin in
  let tuple_list = (Parser.test Scanner.tokenize lexbuf) in

  (* convert the tuple list into the token list *)
  let get_token_list arr = 
    (* returns a list of n DEDENT tokens *)
    let rec get_dedent_list = function
      | 1            -> [Parser.DEDENT]
      | n when n > 1 -> [Parser.DEDENT] @ get_dedent_list (n - 1)
      | _            -> [] in
    (* helper for List.fold_left later *)
    let helper s e = match e with
      | Parser.DEDENT, n   -> s @ (get_dedent_list n)
      | tok, _             -> s @ [tok] in
    List.fold_left helper [] arr in

  let token_list = ref (get_token_list tuple_list) in

  (* hack to convert a list of tokens to something ocamlyacc can read *) 
  let token lexbuf =
    match !token_list with
      | [] -> Parser.EOF
      | h::t -> token_list := t; h in

  (* run the token list through the parser *) 
  let str = Parser.program token (Lexing.from_string "") in
  let result = eval str in
  print_endline (result)

