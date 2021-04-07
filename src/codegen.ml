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
  and str_t      = L.pointer_type       (L.i8_type context)
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
   let built_in_funcs : (string * A.typ * (A.typ list)) list = [ (*put funcs here *)
     ("println", A.Primitive(Void), [A.Primitive(String)]);
     ("int_to_string", A.Primitive(String), [A.Primitive(Int)])
   ] in
   let helper m e = match e with fun_name, ret_t, arg_ts -> 
    let arg_t_arr = Array.of_list 
                    (List.fold_left (fun s e -> s @ [ltype_of_typ e]) [] arg_ts) in
    let func = L.declare_function fun_name 
              (L.var_arg_function_type (ltype_of_typ ret_t) arg_t_arr) the_module in
    let signature = { S.fs_name = fun_name; S.formal_types = arg_ts } in
    SignatureMap.add signature func m in
   List.fold_left helper SignatureMap.empty built_in_funcs in

  (* expression builder *)
  let rec build_expr builder (exp : sexpr) = match exp with
    _, SIntLiteral(i)      -> L.const_int i32_t i
  | _, SStringLiteral(str) -> L.build_global_stringptr str "unused" builder
  | _, SBinop(sexpr1, binop, sexpr2) -> 
        let sexpr1' = build_expr builder sexpr1
        and sexpr2' = build_expr builder sexpr2 in
        (match binop with
          A.Plus         -> L.build_add
        | A.Subtract     -> L.build_sub
        | A.Times        -> L.build_mul
        | A.Divide       -> L.build_sdiv (*signed division*)
        | A.BoGT         -> L.build_icmp L.Icmp.Sgt (*ordered and greater than*)
        | A.BoLT         -> L.build_icmp L.Icmp.Slt (*ordered and less than*)
        | A.BoGTE        -> L.build_icmp L.Icmp.Sge (*etc.*)
        | A.BoLTE        -> L.build_icmp L.Icmp.Sle 
        | A.DoubleEq     -> L.build_icmp L.Icmp.Eq (*ordered and equal to*)
        (*A.Modulo       -> TODO*)  ) sexpr1' sexpr2' "tmp" builder 
        (*"L.build sexpr1' sexpr2' "tmp" 'builder'"? I need to understand LLVM syntax*)
  (*| A.Float, SBinop (sexpr1, binop, sexpr) -> 
        let sexpr1' = build_expr builder sexpr1
        and sexpr2' = build_expr builder sexpr2 in
        (match binop with
          A.Plus         -> L.build_fadd
        | A.Subtract     -> L.build_fsub
        | A.Times        -> L.build_fmul
        | A.Divide       -> L.build_fdiv
        | A.BoGT         -> L.build_fcmp L.Fcmp.Ogt (*ordered and greater than*)
        | A.BoLT         -> L.build_fcmp L.Fcmp.Olt (*ordered and less than*)
        | A.BoGTE        -> L.build_fcmp L.Fcmp.Oge (*etc.*)
        | A.BoLTE        -> L.build_fcmp L.Fcmp.Ole 
        | A.DoubleEq     -> L.build_fcmp L.Fcmp.Oeq (*ordered and equal to*)
        (*A.Modulo       -> TODO*)  ) sexpr1' sexpr2' "tmp" builder *)
        (*"L.build sexpr1' sexpr2' "tmp" 'builder'"? I need to understand LLVM syntax*)
  | typ, SCall(sc) -> match sc with
      SFuncCall(func_name, expr_list) -> 
       let expr_typs = List.fold_left (fun s (t, _) -> s @ [t]) [] expr_list in
       let signature = { S.fs_name = func_name; S.formal_types = expr_typs } in
       if SignatureMap.mem signature built_in_map
        then L.build_call (SignatureMap.find signature built_in_map)
             (Array.of_list (List.fold_left (fun s e -> s @ [build_expr builder e]) 
             [] expr_list))
             (if typ = Primitive(Void) then "" else (func_name ^ "_res"))
             builder
       else L.const_int i32_t 0 (* TODO: this is a placeholder! *)
  in

  (* statement builder *) 
  let build_stmt  builder (ss : sstmt) = match ss with
    SExpr(se)   -> ignore (build_expr builder se)
  | _           -> () in

  (* function decleration builder *) 
  let build_func  (sf : sfdecl) = () in 

  (* class decleration builder *) 
  let build_class (sc : sclassdecl) = () in
  
  (* LLVM requires a 'main' function as an entry point *)
  let main_t : L.lltype =
      L.var_arg_function_type i32_t [| |] in
  let main_func : L.llvalue =
    L.define_function "main" main_t the_module in
  let main_builder = L.builder_at_end context (L.entry_block main_func) in

  (* program builder *) 
  let build_program (spunit : sp_unit) = match spunit with
    SStmt(ss)       -> build_stmt main_builder ss
  | SFdecl(sf)      -> build_func sf
  | SClassdecl(sc)  -> build_class sc in
     
  List.iter build_program sp_units; 
  L.build_ret (L.const_int i32_t 0) main_builder; (* build return for main *)
  the_module
