(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx

and sx = 
  SIntLiteral of int
| SStringLiteral of string
| SCall of scall
and scall =
  SFuncCall of string * sexpr list

type sstmt =
  SExpr of sexpr

type sp_unit =
  SStmt of sstmt

type sprogram = sp_unit list

