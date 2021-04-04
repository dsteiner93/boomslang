(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate sp_units =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Boomslang" in

  (* Get types from the context *)
  let i8_t       = L.i8_type            context
  and i32_t      = L.i32_type           context
  and void_t     = L.void_type          context
  and str_t      = L.pointer_type (L.i8_type context)
  in

  (* Return the LLVM type for a Boomslang type *)
  let ltype_of_typ = function
     A.Int    -> i32_t
  |  _        -> void_t
  in

  (* define the built-in println function *)
  let println_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let println_func : L.llvalue = L.declare_function "printf" println_t the_module in

  (* LLVM requires a 'main' function as an entry point *)
  let main_t : L.lltype =
      L.var_arg_function_type i32_t [| |] in
  let main_func : L.llvalue =
    L.define_function "main" main_t the_module in

  (* define the builder for the main block *) 
  let builder = L.builder_at_end context (L.entry_block main_func) in

  (* format strings for printf style of functions *)
  let int_format_str =
    L.build_global_stringptr "%d\n" "fmt" builder
  and str_format_str =
    L.build_global_stringptr "%s\n" "fmt" builder in

  let rec build_expr (exp : sexpr) = match exp with
    _, SIntLiteral(i)      -> L.const_int i32_t i
  | _, SStringLiteral(str) -> L.build_global_stringptr str "unused" builder
  | typ, SCall(sc) -> match sc with
      SFuncCall(func_name, expr_list) -> match func_name, expr_list with
        "println", [e] -> L.build_call println_func [| str_format_str; (build_expr e) |] 
                                       "" builder
  in

  let build_stmt  (ss : sstmt) = match ss with
    SExpr(se)   -> ignore (build_expr se)
  | _           -> () in

  let build_func  (sf : sfdecl) = () in 

  let build_class (sc : sclassdecl) = () in

  let build_program (spunit : sp_unit) = match spunit with
    SStmt(ss)       -> build_stmt ss
  | SFdecl(sf)      -> build_func sf
  | SClassdecl(sc)  -> build_class sc in
  
  List.iter build_program sp_units; 
  L.build_ret (L.const_int i32_t 0) builder; (* build return for main *)
  the_module
