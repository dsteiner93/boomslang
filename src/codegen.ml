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
module StringHash = Hashtbl.Make(struct
  type t = string (* type of keys *)
  let equal x y = x = y (* use structural comparison *)
  let hash = Hashtbl.hash (* generic hash function *)
end)

let rec lookup v_symbol_tables s =
  match v_symbol_tables with
  [] -> raise (Failure ("undeclared identifier " ^ s))
  | hd::tl -> try StringHash.find hd s
              with Not_found -> (lookup tl s)

(* translate : Sast.program -> Llvm.module *)
let translate sp_units =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Boomslang" in

  (* Get types from the context *)
  let i1_t       = L.i1_type            context
  and i8_t       = L.i8_type            context
  and i32_t      = L.i32_type           context
  and i64_t      = L.i64_type           context
  and float_t    = L.double_type        context
  and str_t      = L.pointer_type       (L.i8_type context)
  and void_t     = L.void_type          context
  in

  (* Return the LLVM type for a Boomslang type *)
  let ltype_of_typ = function
     A.Primitive(A.Int)    -> i32_t
  |  A.Primitive(A.Long)   -> i64_t
  |  A.Primitive(A.Float)  -> float_t
  |  A.Primitive(A.Char)   -> i8_t
  |  A.Primitive(A.String) -> str_t
  |  A.Primitive(A.Bool)   -> i1_t
  |  A.Primitive(A.Void)   -> void_t
  |  _                     -> void_t
  in
  let get_lvalue_of_bool = function
    true -> (L.const_int (ltype_of_typ (A.Primitive(A.Bool))) 1)
  | false -> (L.const_int (ltype_of_typ (A.Primitive(A.Bool))) 0)
  in

  (* create a map of all of the built in functions *)
  let built_in_map =
   let built_in_funcs : (string * A.typ * (A.typ list)) list =
     let convert (fs : (S.function_signature * A.typ)) =
       (((fst fs).fs_name), (snd fs), ((fst fs).formal_types)) in
     List.map convert Semant.built_in_funcs in
   let helper m e = match e with fun_name, ret_t, arg_ts -> 
    let arg_t_arr = Array.of_list 
                    (List.fold_left (fun s e -> s @ [ltype_of_typ e]) [] arg_ts) in
    let func = L.declare_function fun_name 
              (L.var_arg_function_type (ltype_of_typ ret_t) arg_t_arr) the_module in
    let signature = { S.fs_name = fun_name; S.formal_types = arg_ts } in
    SignatureMap.add signature func m in
   List.fold_left helper SignatureMap.empty built_in_funcs in

  (* expression builder *)
  let rec build_expr builder v_symbol_tables (exp : sexpr) = match exp with
    _, SIntLiteral(i)      -> L.const_int i32_t i
  | _, SLongLiteral(l)     -> L.const_of_int64 i64_t l true
  | _, SFloatLiteral(f)    -> L.const_float_of_string float_t f
  | _, SCharLiteral(c)     -> L.const_int i8_t (Char.code c)
  | _, SStringLiteral(str) -> L.build_global_stringptr str "unused" builder
  | _, SBoolLiteral(true)  -> L.const_int i1_t 1
  | _, SBoolLiteral(false) -> L.const_int i1_t 0
  | _, SId(id)             -> L.build_load (lookup v_symbol_tables id) id builder
  | _, SNullExpr           -> L.const_pointer_null i32_t
  (* Function calls *)
  | typ, SCall(sc) -> (match sc with
      (* Nulls must be handled with care *)
        SFuncCall("null_to_string", [(A.NullType, _)]) -> build_expr builder v_symbol_tables (A.Primitive(A.String), SStringLiteral("NULL"))
      | SFuncCall(func_name, expr_list) ->
          let expr_typs = List.fold_left (fun s (t, _) -> s @ [t]) [] expr_list in
          let signature = { S.fs_name = func_name; S.formal_types = expr_typs } in
          if SignatureMap.mem signature built_in_map then
            L.build_call (SignatureMap.find signature built_in_map)
            (Array.of_list (List.fold_left (fun s e -> s @ [build_expr builder v_symbol_tables e])
            [] expr_list))
            (if typ = A.Primitive(A.Void) then "" else (func_name ^ "_res"))
            builder
          else L.const_int i32_t 0 (* TODO: this is a placeholder! *)
      )
  (* == is the only binop that can apply to any two types. *)
  | _, SBinop(sexpr1, A.DoubleEq, sexpr2) ->
      let sexpr1' = build_expr builder v_symbol_tables sexpr1
      and sexpr2' = build_expr builder v_symbol_tables sexpr2 in
      let sexpr1typ = (fst sexpr1)
      and sexpr2typ = (fst sexpr2) in
      if ((sexpr1typ = A.NullType) && (sexpr2typ = A.NullType)) then
        (get_lvalue_of_bool true)
      else if (sexpr1typ = A.NullType) then
        (* Only an object (not a primitive nor array) may be Null. *)
        (match sexpr2typ with
             A.Class(_) -> get_lvalue_of_bool (L.is_null sexpr2')
           | _ -> L.const_int (ltype_of_typ (A.Primitive(Bool))) 0)
      else if (sexpr2typ = A.NullType) then
        (match sexpr1typ with
             A.Class(_) -> get_lvalue_of_bool (L.is_null sexpr1')
           | _ -> L.const_int (ltype_of_typ (A.Primitive(Bool))) 0)
      else
        (match sexpr1typ with
          (* Even though these look different, they are all integers internally *)
            A.Primitive(A.Int)
          | A.Primitive(A.Long)
          | A.Primitive(A.Char)
          | A.Primitive(A.Bool) -> L.build_icmp L.Icmp.Eq sexpr1' sexpr2' "tmp" builder
          (* Floats are similar *)
          | A.Primitive(A.Float) -> L.build_fcmp L.Fcmp.Oeq sexpr1' sexpr2' "tmp" builder
          (* Strings are not comparable using an LLVM native function,
             so we call our own C function here. *)
          | A.Primitive(A.String) ->
              let signature = { S.fs_name = "compare_strings"; S.formal_types = [(fst sexpr1); (fst sexpr2)] } in
              L.build_call (SignatureMap.find signature built_in_map)
                (Array.of_list (List.fold_left (fun s e -> s @ [build_expr builder v_symbol_tables e]) [] [sexpr1; sexpr2]))
                (signature.fs_name ^ "_res") builder
          | _ -> raise (Failure("TODO implement equality checks for arrays and classes"))
        )
  (* Integer and long binops *)
  | _, SBinop(((A.Primitive(A.Int), _) as sexpr1), binop, ((A.Primitive(A.Int), _) as sexpr2))
  | _, SBinop(((A.Primitive(A.Long), _) as sexpr1), binop, ((A.Primitive(A.Long), _) as sexpr2)) ->
      let sexpr1' = build_expr builder v_symbol_tables sexpr1
      and sexpr2' = build_expr builder v_symbol_tables sexpr2 in
      (match binop with
          A.Plus         -> L.build_add
        | A.Subtract     -> L.build_sub
        | A.Times        -> L.build_mul
        | A.Divide       -> L.build_sdiv (* signed division*)
        | A.Modulo       -> L.build_srem (* signed remainder *)
        | A.DoubleEq     -> L.build_icmp L.Icmp.Eq (* ordered and equal to *)
        | A.BoGT         -> L.build_icmp L.Icmp.Sgt (* ordered and greater than *)
        | A.BoLT         -> L.build_icmp L.Icmp.Slt (* ordered and less than *)
        | A.BoGTE        -> L.build_icmp L.Icmp.Sge (* etc. *)
        | A.BoLTE        -> L.build_icmp L.Icmp.Sle 
        | _ -> raise (Failure("Found ineligible binop for int/long operands"))
      ) sexpr1' sexpr2' "tmp" builder
  (* Float binops *)
  | _, SBinop(((A.Primitive(A.Float), _) as sexpr1), binop, ((A.Primitive(A.Float), _) as sexpr2)) ->
      let sexpr1' = build_expr builder v_symbol_tables sexpr1
      and sexpr2' = build_expr builder v_symbol_tables sexpr2 in
      (match binop with
          A.Plus         -> L.build_fadd
        | A.Subtract     -> L.build_fsub
        | A.Times        -> L.build_fmul
        | A.Divide       -> L.build_fdiv (* signed division*)
        | A.Modulo       -> L.build_frem (* signed remainder *)
        | A.DoubleEq     -> L.build_fcmp L.Fcmp.Oeq (* ordered and equal to *)
        | A.BoGT         -> L.build_fcmp L.Fcmp.Ogt (* ordered and greater than *)
        | A.BoLT         -> L.build_fcmp L.Fcmp.Olt (* ordered and less than *)
        | A.BoGTE        -> L.build_fcmp L.Fcmp.Oge (* etc. *)
        | A.BoLTE        -> L.build_fcmp L.Fcmp.Ole
        | _ -> raise (Failure("Found ineligible binop for float operands"))
      ) sexpr1' sexpr2' "tmp" builder
  (* String binops (the only one supported is + for concatenate) *)
  | _, SBinop(((A.Primitive(A.String), _) as sexpr1), binop, ((A.Primitive(A.String), _) as sexpr2)) ->
      let sexpr1' = build_expr builder v_symbol_tables sexpr1
      and sexpr2' = build_expr builder v_symbol_tables sexpr2 in
      (match binop with
          A.Plus         ->
            let signature = { S.fs_name = "concat_strings"; S.formal_types = [(fst sexpr1); (fst sexpr2)] } in
            L.build_call (SignatureMap.find signature built_in_map)
              (Array.of_list (List.fold_left (fun s e -> s @ [build_expr builder v_symbol_tables e]) [] [sexpr1; sexpr2]))
              (signature.fs_name ^ "_res") builder
        | _ -> raise (Failure("Found ineligible binop for string operands"))
      )
  (* Boolean binops *)
  | _, SBinop(((A.Primitive(A.Bool), _) as sexpr1), binop, ((A.Primitive(A.Bool), _) as sexpr2)) ->
      let sexpr1' = build_expr builder v_symbol_tables sexpr1
      and sexpr2' = build_expr builder v_symbol_tables sexpr2 in
      (match binop with
          A.BoOr         -> L.build_or
        | A.BoAnd        -> L.build_and
        | _ -> raise (Failure("Found ineligible binop for boolean operands"))
      ) sexpr1' sexpr2' "tmp" builder
  (* Unary operators *)
  | _, SUnop(A.Not, sexpr1) ->
      let sexpr1' = build_expr builder v_symbol_tables sexpr1 in
      L.build_not sexpr1' "tmp" builder
  | _, SUnop(A.Neg, ((A.Primitive(A.Int), _) as sexpr1))
  | _, SUnop(A.Neg, ((A.Primitive(A.Long), _) as sexpr1)) ->
      let sexpr1' = build_expr builder v_symbol_tables sexpr1 in
      L.build_neg sexpr1' "tmp" builder
  | _, SUnop(A.Neg, ((A.Primitive(A.Float), _) as sexpr1)) ->
      let sexpr1' = build_expr builder v_symbol_tables sexpr1 in
      L.build_fneg sexpr1' "tmp" builder
  | _, SAssign(SRegularAssign(typ, name, sexpr)) ->
      (* Variables outside of classes and functions should be globals,
         those inside functions and classes should be locals.
         In Boomslang, we say that only entries going into the first
         (i.e. highest scope) symbol table are globals.
         Consider the following program:
         int x = 5
         if x > 2:
                int y = 1
         else:
                int y = 2
         In the above program, x is treated like a global variable, and
         y is treated like a local variable inside of main().
       *)
      let e' = build_expr builder v_symbol_tables sexpr in
      if List.length v_symbol_tables = 1 then
        (* Build a global *)
        let global_symbol_table = List.hd v_symbol_tables in
        (* This looks a little weird. Basically, we declare a global of the given type,
           then initialize it to the null version of the that type. Then inside main,
           we build a store to put the contents of the RHS into the global variable.
           For some reason this works but L.define_global with the RHS does not. *)
        let declared_global = (L.declare_global (ltype_of_typ typ) name the_module) in
        let _ = L.set_initializer (L.const_null (ltype_of_typ typ)) declared_global in
        ((StringHash.add global_symbol_table name declared_global);
        ignore(L.build_store e' (lookup v_symbol_tables name) builder));
        e'
      else
        (* Build a local. This means allocating space on the stack, and then
           storing the value of the expr there. *)
        let this_scopes_symbol_table = List.hd v_symbol_tables in
        ((StringHash.add this_scopes_symbol_table name (L.build_alloca (ltype_of_typ typ) name builder));
        ignore(L.build_store e' (lookup v_symbol_tables name) builder));
        e'
  | _, SUpdate(SRegularUpdate(name, Eq, sexpr)) ->
      let e' = build_expr builder v_symbol_tables sexpr in
      ignore(L.build_store e' (lookup v_symbol_tables name) builder); e'
  | _ -> raise (Failure("unimplemented expr in codegen"))
  in

  (* statement builder *) 
  let build_stmt builder v_symbol_tables (ss : sstmt) = match ss with
    SExpr(se)   -> ignore (build_expr builder v_symbol_tables se)
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
  let build_program v_symbol_tables (spunit : sp_unit) = match spunit with
    SStmt(ss)       -> build_stmt main_builder v_symbol_tables ss
  | SFdecl(sf)      -> build_func sf
  | SClassdecl(sc)  -> build_class sc in
     
  List.iter (build_program [StringHash.create 10]) sp_units;
  L.build_ret (L.const_int i32_t 0) main_builder; (* build return for main *)
  the_module
