(* Scanner for Python++ Language *)

{

open Parser 

module StringMap = Map.Make(String)

let add_entry map pair = StringMap.add (fst pair) (snd pair) map

let primitive_types = [
  "short"; "int"; "long"; "float"; "double"; "boolean"; "char"; "string";
  "void";
]

let reserved_word_to_token = List.fold_left add_entry StringMap.empty [
  (* Boolean operators *)
  ("not", NOT); ("or", OR); ("and", AND);
  (* Loops and conditionals *)
  ("loop", LOOP); ("while", WHILE); ("if", IF); ("elif", ELIF); ("else", ELSE);
  (* Words related to functions and classes *)
  ("def", DEF); ("class", CLASS); ("construct", CONSTRUCT);
  ("return", RETURN); ("returns", RETURNS); ("self", SELF);
  ("required", REQUIRED); ("optional", OPTIONAL); ("static", STATIC);
]

}


(* Class names in Python++ must start with a capital letter,
   to distinguish them from identifiers, which must begin
   with a lowercase letter *)
let class_name = ['A'-'Z']['a'-'z' 'A'-'Z']*
let int_literal = ['0'-'9']+

rule tokenize = parse
  [' ' '\t' '\r'] { tokenize lexbuf }
(* Mathematical operations *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MODULO }
(* Assignment operators *)
| '=' { EQ }
| "+=" { PLUS_EQ }
| "-=" { MINUS_EQ }
| "*=" { TIMES_EQ }
| "/=" { DIVIDE_EQ }
(* Comparison operators *)
| "==" { DOUBLE_EQ }
| "!=" { NOT_EQ }
| ">" { GT }
| "<" { LT }
| ">=" { GTE }
| "<=" { LTE }
(* Literal definitions *)
| int_literal as lit { INT_LITERAL(int_of_string lit) }
| ['0'-'9']+('.'['0'-'9']+)? as lit { FLOAT_LITERAL(float_of_string lit) }
| "true" { BOOLEAN_LITERAL(true) }
| "false" { BOOLEAN_LITERAL(false) }
(* TODO strings and chars *)
(* TODO syntactically meaningful whitespace - must keep track of INDENT *)
(* TODO comments, single-line and multi-line *)
| '#'  { single_comment lexbuf }
| "/#" { multi_comment lexbuf }
(* Misc. punctuation *)
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACKET }
| ']' { RBRACKET }
| ':' { COLON }
| '.' { PERIOD }
| ',' { COMMA }
| '\n' { NEWLINE }
| "NULL" { NULL }
(* User defined types, i.e. class names *)
| class_name as t { TYPE(t) }
(* Array/List type. Lists can be of primitives or objects *)
| class_name"["int_literal"]" | "short["int_literal"]" |
  "int["int_literal"]" | "long["int_literal"]" |
  "double["int_literal"]" | "float["int_literal"]" |
  "boolean["int_literal"]" | "char["int_literal"]" |
  "string["int_literal"]" as t { TYPE(t) }
(* If we see a lowercase letter followed by any letters or digits,
   it could either be the name of a primitive type (e.g. int), or
   a reserved word (e.g. class) or an identifier for a variable. *)
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as possible_id {
    if List.mem possible_id primitive_types
      then TYPE(possible_id)
    else if StringMap.mem possible_id reserved_word_to_token
      then StringMap.find possible_id reserved_word_to_token
    else
      IDENTIFIER(possible_id)
  }
| eof { EOF }

and multi_comment = parse
  "#/" { tokenize lexbuf }
| _ { multi_comment lexbuf }

and single_comment = parse
  '\n' { tokenize lexbuf }
| _ { single_comment lexbuf }
