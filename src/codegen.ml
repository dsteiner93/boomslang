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
  
  let main_t : L.lltype = 
      L.var_arg_function_type int_t [| |] in
  let main_func : L.llvalue = 
    L.define_function "main" main_t the_module in

  let builder = L.builder_at_end context (L.entry_block main_func) in

  let _ : L.llvalue =
    L.build_call println_func [| (L.const_int int_t 42) |] "" builder in

  let main_return : L.llvalue =
    L.build_ret (L.const_int int_t 0) builder in
  (*
  let main_entry_block = L.entry_block main_func in
  let builder = L.builder_at_end context main_entry_block in
  
  
  let builder = L.builder_at_end context (L.entry_block the_function) in
  
  let main_return : L.llvalue =
    L.build_ret_void the_module *)
(* 
  let builder = L.builder_at_end context (L.entry_block "main") in
  let _ = L.build_call println_func [| 42 |] "println" builder in
*)
  
  the_module

(*  
(* Fill in the body of the given function *)
  let build_expr_body fdecl =
    (* let (the_function, _) = StringMap.find fdecl.sfname function_decls in *)
    
    (* TODO: Char is hard coded for now, don't hardcode this later *) 
    let name = "println"
    and formal_types = [| i32_t |] in
      let ftype = L.function_type void_t formal_types in
    (* let ftype = L.function_type (ltype_of_typ A.Void) char_t in *)
    let the_function = L.define_function "println" void_t the_module in

    (* creates an instruction builder positioned at the end of the basic block context *)
    let builder = L.builder_at_end context 
    (* returns the entry basic block of the function the_function *)
                (L.entry_block the_function) in

    (* creates a series of instructions that adds a global 
    string pointer at the position specified by the instruction builder "builder".*)
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
    SLiteral i  -> L.const_int i32_t i
      | SCall ("println", [e]) -> 
        L.build_call println_func [| int_format_str ; (expr builder e) |] 
                                    "println" builder
      | _ -> L.const_int i32_t 0

    (* Construct code for an expression; return its value *)
    (* creates a %name = call %fn(args...) 
    instruction at the position specified by the instruction builder b. 
    i.e. %println = call println_func(i8 104) *)
    in
  (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
Some _ -> ()
    | None -> ignore (instr builder)
  
  in

  (* Build the code for each statement in the function *)
  let builder = expr builder (SBlock fdecl) in

  (* Add a return if the last block falls off the end *)
  
  add_terminal builder (match fdecl with
    A.Void -> L.build_ret_void)
in

  List.iter build_expr_body spunits;
  the_module
*)
