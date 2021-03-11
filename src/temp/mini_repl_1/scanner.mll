{
open Parser 

let indent_level = ref 3

}

rule tokenize = parse
  [' ' '\r'] { tokenize lexbuf }
| ['\t'] { TAB }
| ['\n'] { NEWLINE }
| ['a'-'z']+ as lit { NAME(lit) }
| ['#'] { POUND }
| eof { EOF }
