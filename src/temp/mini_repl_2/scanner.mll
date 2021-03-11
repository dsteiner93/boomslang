{
open Parser 

let indent_level = ref 0
}

rule tokenize = parse
  [' ' '\r'] { tokenize lexbuf }
| ['\n']['\t']* as lit {
    let _ = print_endline "hello" in
    let ntabs = (Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf) - 1 in
    let temp = !indent_level in
    if ntabs = !indent_level
     then (print_endline "a" ; NEWLINE)
    else if ntabs = !indent_level + 1
     then (print_endline "b" ; incr indent_level ; NINDENT)
    else
     (print_endline "c" ; indent_level := ntabs ; NDEDENTN(temp - ntabs))
  }
| ['a'-'z']+ as lit { NAME(lit) }
| eof { EOF }
