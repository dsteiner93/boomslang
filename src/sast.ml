(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx

and sx = 
 SLiteral of int
| SCall of scall

and call =
 FuncCall of string * sx list 

type sstmt =
 SExpr of sexpr

type sp_unit =
 SStmt of sstmt

type sprogram = sp_unit list
(* or maybe just type sprogram = sexpr *)

(* [ (Void, ("println", [(string, "Hello World")]))] *)
