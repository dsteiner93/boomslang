{
open Parser 

let indent_level = ref 0
}

rule tokenize = parse
  [' ' '\n' '\r' '\t']*['\n']['\t']* {
    (* this is the length of the regex *)
    let regex_len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
    (* use this function to find how many tabs there are at the end of regex *)
    let rec get_ntabs_helper n = 
     if (Lexing.lexeme_char lexbuf n) = '\n'
      then regex_len - n - 1
     else if n = 0
      then regex_len (* this case should be unreachable *)
     else
      get_ntabs_helper (n - 1) in
    (* get the number of tabs in the regex *) 
    let ntabs = get_ntabs_helper (regex_len - 1) in
    let temp = !indent_level in
    if ntabs = !indent_level
     then NEWLINE
    else if ntabs = !indent_level + 1
     then ( incr indent_level ; INDENT )
    else
     ( indent_level := ntabs ; NDEDENTN(temp - ntabs) )
  }
| [' ' '\n' '\r' '\t']+ { tokenize lexbuf } 
| ['a'-'z']+ as lit { NAME(lit) }
| eof { EOF }
