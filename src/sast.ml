(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx = 
  SStringLiteral of string
| SCharLiteral of char
| Scall of scall
and scall = 
  SFuncCall of string * sexpr list (* my_func(1, 2, 3) *)

type sstmt =
  SExpr of sexpr

type sp_unit =
  SStmt of sstmt

type sprogram = sp_unit list
(* or maybe just type sprogram = sexpr *)

(* [ (Void, ("println", [(string, "Hello World")]))] *)
