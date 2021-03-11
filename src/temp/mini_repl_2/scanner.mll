{
open Parser 

let indent_level = ref 0
}

rule tokenize = parse
  [' ' '\r'] { tokenize lexbuf }
| ['\n']['\t']* as lit {
    (* ntabs is the number of tabs after the newline, it can be 0 *)
    let ntabs = (Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf) - 1 in
    let temp = !indent_level in
    if ntabs = !indent_level
     then NEWLINE
    else if ntabs = !indent_level + 1
     then ( incr indent_level ; INDENT )
    else
     ( indent_level := ntabs ; NDEDENTN(temp - ntabs) )
  }
| ['a'-'z']+ as lit { NAME(lit) }
| eof { EOF }
