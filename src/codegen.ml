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
module S = Semant
open Sast 

module StringMap = Map.Make(String)
module SignatureMap = Map.Make(struct type t = S.function_signature let compare = compare end)

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
     A.Primitive(Int)    -> i32_t
  |  A.Primitive(Void)   -> void_t
  |  A.Primitive(String) -> str_t
  |  _                   -> void_t
  in

  (* create a map of all of the built in functions *)
  let built_in_map =
   let built_in_funcs : (string * A.typ * (A.typ list)) list = [
     ("println", A.Primitive(Void), [A.Primitive(String)])
   ] in
   let helper m e = match e with fun_name, ret_t, arg_ts -> 
       let arg_t_arr = Array.of_list (List.fold_left (fun s e -> s @ [ltype_of_typ e]) 
                    [] arg_ts) in
    SignatureMap.add { S.fs_name = fun_name; S.formal_types = arg_ts }
                      (L.var_arg_function_type (ltype_of_typ ret_t) arg_t_arr) m in
   List.fold_left helper SignatureMap.empty built_in_funcs in
    
  (* define the built-in println function *)
  let println_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let println_func : L.llvalue = L.declare_function "printf" println_t the_module in

  (* LLVM requires a 'main' function as an entry point *)
  let main_t : L.lltype =
      L.var_arg_function_type i32_t [| |] in
  let main_func : L.llvalue =
    L.define_function "main" main_t the_module in

  let str_format_str builder =
   L.build_global_stringptr "%s\n" "fmt" builder in

  let rec build_expr builder (exp : sexpr) = match exp with
    _, SIntLiteral(i)      -> L.const_int i32_t i
  | _, SStringLiteral(str) -> L.build_global_stringptr str "unused" builder
  | typ, SCall(sc) -> match sc with
      SFuncCall(func_name, expr_list) -> match func_name, expr_list with
        "println", [e] -> L.build_call println_func [| (str_format_str builder); 
                          (build_expr builder e) |] "" builder
  in

  let build_stmt  builder (ss : sstmt) = match ss with
    SExpr(se)   -> ignore (build_expr builder se)
  | _           -> () in

  let build_func  (sf : sfdecl) = () in 

  let build_class (sc : sclassdecl) = () in
  
  (* define the builder for the main block *) 
  let main_builder = L.builder_at_end context (L.entry_block main_func) in

  let build_program (spunit : sp_unit) = match spunit with
    SStmt(ss)       -> build_stmt main_builder ss
  | SFdecl(sf)      -> build_func sf
  | SClassdecl(sc)  -> build_class sc in
     
  List.iter build_program sp_units; 
  L.build_ret (L.const_int i32_t 0) main_builder; (* build return for main *)
  the_module
