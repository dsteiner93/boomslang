(* Scanner for Python++ Language *)

{

open Parser 

module StringMap = Map.Make(String)

let add_entry map pair = StringMap.add (fst pair) (snd pair) map

let reserved_word_to_token = List.fold_left add_entry StringMap.empty [
  (* Boolean operators *)
  ("not", NOT); ("or", OR); ("and", AND);
  (* Loops and conditionals *)
  ("loop", LOOP); ("while", WHILE); ("if", IF); ("elif", ELIF); ("else", ELSE);
  (* Words related to functions and classes *)
  ("def", DEF); ("class", CLASS); ("self", SELF);
  ("return", RETURN); ("returns", RETURNS);
  ("static", STATIC); ("required", REQUIRED); ("optional", OPTIONAL);
  (* Primitive data types *)
  ("int", INT); ("long", LONG); ("float", FLOAT); ("boolean", BOOLEAN);
  ("char", CHAR); ("string", STRING); ("void", VOID);
]

let strip_firstlast str =
  if String.length str <= 2 then ""
  else String.sub str 1 ((String.length str) - 2)

let tab_count_stack = Stack.of_seq (List.to_seq [0])
let token_queue = Queue.create ()

let rec enqueue_dedents n = if n > 0 then (Queue.add DEDENT token_queue; (enqueue_dedents (n-1)))

let rec enqueue_indents n = if n > 0 then (Queue.add INDENT token_queue; (enqueue_indents (n-1)))

let count_tabs str = if String.contains str '\t' then String.length str - String.index str '\t' else 0
}


(* Class names in Python++ must start with a capital letter,
   to distinguish them from identifiers, which must begin
   with a lowercase letter *)
let class_name = ['A'-'Z']['a'-'z' 'A'-'Z']*
let int_literal = ['0'-'9']+

rule tokenize = parse
  [' ' '\r'] { tokenize lexbuf }
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
(* Comments *)
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
| '_' { UNDERSCORE }
| "NULL" { NULL }
(* Literal definitions *)
| int_literal as lit { INT_LITERAL(int_of_string lit) }
| int_literal"L" as lit {
    LONG_LITERAL(Int64.of_string (String.sub lit 0 (String.length lit - 1)))
}
| ['0'-'9']+('.'['0'-'9']+)? | '.'['0'-'9']+ as lit { FLOAT_LITERAL(lit) }
| "true" { BOOLEAN_LITERAL(true) }
| "false" { BOOLEAN_LITERAL(false) }
(* Char literals are single quotes followed by any single character
   followed by a single quote *)
| '\'' [' '-'~'] '\'' as lit { CHAR_LITERAL( (strip_firstlast lit).[0] ) }
(* String literals in Python++ cannot contain double quotes or newlines.
   String literals are a " followed by any non newline or double quote
   followed by " *)
| '"' ([^'"''\n'])* '"' as lit { STRING_LITERAL(strip_firstlast lit) }
(* Syntactically meaningful whitespace - tabs for indentation only *)
| ['\n']+['\t']* as newlines_and_tabs {
  let num_tabs = (count_tabs newlines_and_tabs) in
  if (Stack.top tab_count_stack) == num_tabs then
    NEWLINE
  else if (Stack.top tab_count_stack) > num_tabs then
    ((enqueue_dedents ((Stack.pop tab_count_stack) - num_tabs); Stack.push num_tabs tab_count_stack); NEWLINE)
  else
    ((enqueue_indents (num_tabs - (Stack.top tab_count_stack)); Stack.push num_tabs tab_count_stack); NEWLINE)
}
(* User defined types, i.e. class names *)
| class_name as t { CLASS_NAME(t) }
(* If we see a lowercase letter followed by any letters or digits,
   it could either be the name of a primitive type (e.g. int), or
   a reserved word (e.g. class) or an identifier for a variable. *)
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as possible_id {
    if StringMap.mem possible_id reserved_word_to_token
      then StringMap.find possible_id reserved_word_to_token
    else
      IDENTIFIER(possible_id)
  }
| ['+' '-' '%' '&' '$' '@' '!' '#' '^' '*' '/' '~' '?' '>' '<']+ as lit { OBJ_OPERATOR(lit) }
| eof { EOF }
| _ as char { raise (Failure("Illegal character: " ^ Char.escaped char)) }


and multi_comment = parse
  "#/" { tokenize lexbuf }
| _ { multi_comment lexbuf }

and single_comment = parse
  '\n' { tokenize lexbuf }
| _ { single_comment lexbuf }

{
let read_next_token lexbuf =
  if Queue.is_empty token_queue then tokenize lexbuf else Queue.take token_queue
}

