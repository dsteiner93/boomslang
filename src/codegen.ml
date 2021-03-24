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
  let the_module = L.create_module context "Pythonpp" in

  (* Get types from the context *)
  let i8_t       = L.i8_type            context
  and i32_t      = L.i32_type           context
  and void_t     = L.void_type          context
  and str_t      = L.pointer_type (L.i8_type context)
  in

  (* Return the LLVM type for a python++ type *)
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

  let builder = L.builder_at_end context (L.entry_block main_func) in

  let int_format_str =
    L.build_global_stringptr "%d\n" "fmt" builder
  and str_format_str =
    L.build_global_stringptr "%s\n" "fmt" builder
  in
  let get_the_string = function
    SStmt(SExpr(_, SCall(SFuncCall(fname, (_, SStringLiteral(sliteral))::tl1))))::tl2 -> sliteral
    | _ -> raise (Failure("Unimplemented"))
  in
  let get_the_ll_string = L.build_global_stringptr (get_the_string sp_units) "unused" builder in
  let _ : L.llvalue =
    L.build_call println_func [| str_format_str ; get_the_ll_string |] "" builder in

  (* add the return at the end of main *) 
  let main_return : L.llvalue =
    L.build_ret (L.const_int i32_t 0) builder in
  
  the_module
