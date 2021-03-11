type token =
  | TAB
  | NEWLINE
  | POUND
  | EOF
  | NAME of (string)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 13 "parser.ml"
let yytransl_const = [|
  257 (* TAB *);
  258 (* NEWLINE *);
  259 (* POUND *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  260 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\004\000\005\000\003\000\000\000\000\000"

let yydgoto = "\002\000\
\008\000"

let yysindex = "\003\000\
\255\254\000\000\000\000\000\000\000\000\000\000\255\254\255\254"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\000\006\000"

let yygindex = "\000\000\
\007\000"

let yytablesize = 8
let yytable = "\003\000\
\004\000\005\000\006\000\001\000\006\000\001\000\000\000\007\000"

let yycheck = "\001\001\
\002\001\003\001\004\001\001\000\000\000\000\000\255\255\001\000"

let yynames_const = "\
  TAB\000\
  NEWLINE\000\
  POUND\000\
  EOF\000\
  "

let yynames_block = "\
  NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.str) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.str) in
    Obj.repr(
# 14 "parser.mly"
                  ( Concat(_1, _2) )
# 72 "parser.ml"
               : Ast.str))
; (fun __caml_parser_env ->
    Obj.repr(
# 15 "parser.mly"
      ( Lit("TAB") )
# 78 "parser.ml"
               : Ast.str))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 16 "parser.mly"
       ( Lit(_1) )
# 85 "parser.ml"
               : Ast.str))
; (fun __caml_parser_env ->
    Obj.repr(
# 17 "parser.mly"
          ( Lit("NEWLINE") )
# 91 "parser.ml"
               : Ast.str))
; (fun __caml_parser_env ->
    Obj.repr(
# 18 "parser.mly"
        ( Lit("POUND") )
# 97 "parser.ml"
               : Ast.str))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.str)
