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

  let println_t : L.lltype =
      L.var_arg_function_type void_t [| int_t |] in
  let println_func : L.llvalue =
      L.declare_function "println" println_t the_module in
  
  the_module
