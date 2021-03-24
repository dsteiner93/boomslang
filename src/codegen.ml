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
let translate spunits =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Pythonpp" in

  (* Get types from the context *)
  let int_t      = L.i32_type           context
  and void_t     = L.void_type          context in

  (* Return the LLVM type for a python++ type *)
  let ltype_of_typ = function
     A.Int    -> int_t
  |  _        -> void_t
  in

  (* define the println function, this prints ints for now *) 
  let println_t : L.lltype =
      L.var_arg_function_type void_t [| int_t |] in
  let println_func : L.llvalue =
      L.declare_function "println" println_t the_module in

  (* LLVM requires a 'main' function as an entry point *) 
  let main_t : L.lltype = 
      L.var_arg_function_type int_t [| |] in
  let main_func : L.llvalue = 
    L.define_function "main" main_t the_module in

  let builder = L.builder_at_end context (L.entry_block main_func) in

  (* hardcode to print int = 42 *) 
  let _ : L.llvalue =
    L.build_call println_func [| (L.const_int int_t 42) |] "" builder in

  (* add the return at the end of main *) 
  let main_return : L.llvalue =
    L.build_ret (L.const_int int_t 0) builder in
  
  the_module
