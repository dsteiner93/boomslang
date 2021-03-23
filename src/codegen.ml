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
  let char_t     = L.i8_type           context
  and void_t     = L.void_type         context in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
     A.Char   -> char_t
  |  A.Void   -> void_t
  |  _        -> void_t
  in

  let printf_t : L.lltype = 
      L.var_arg_function_type char_t [| char_t |] in
  let println_func : L.llvalue = 
      L.declare_function "println" printf_t the_module in

  (* Fill in the body of the given function *)
  let build_expr_body fdecl =
    (* let (the_function, _) = StringMap.find fdecl.sfname function_decls in *)
    
    let ftype = L.function_type (ltype_of_typ fdecl.styp) in
    let the_function = (L.define_function "println" ftype the_module, fdecl) in

    (* creates an instruction builder positioned at the end of the basic block context *)
    let builder = L.builder_at_end context 
    (* returns the entry basic block of the function the_function *)
                (L.entry_block the_function) in

    (* creates a series of instructions that adds a global 
    string pointer at the position specified by the instruction builder "builder".*)
    let char_format_str = L.build_global_stringptr "%c\n" "fmt" builder in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
    SCharLiteral c -> L.const_string char_t c
      | SCall ("println", [e]) -> 

    (* Construct code for an expression; return its value *)
    (* creates a %name = call %fn(args...) 
    instruction at the position specified by the instruction builder b. 
    i.e. %println = call println_func(i8 104) *)
    L.build_call println_func [| char_format_str ; (expr builder e) |] "println" builder
    in

  (* Add a return if the last block falls off the end *)
  add_terminal builder (match fdecl.styp with
    A.Void -> L.build_ret_void)
in

  List.iter build_expr_body spunits;
  the_module
