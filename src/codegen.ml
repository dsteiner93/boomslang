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
module SignatureMap = S.SignatureMap
module StringHash = Hashtbl.Make(struct
  type t = string (* type of keys *)
  let equal x y = x = y (* use structural comparison *)
  let hash = Hashtbl.hash (* generic hash function *)
end)

type symbol_table_entry = {
  llvalue: L.llvalue;
  typ: A.typ;
}

let rec lookup v_symbol_tables s =
  match v_symbol_tables with
  [] -> raise (Failure ("undeclared identifier " ^ s))
  | hd::tl -> try (StringHash.find hd s).llvalue
              with Not_found -> (lookup tl s)

let rec lookup_class_name v_symbol_tables s =
  match v_symbol_tables with
  [] -> raise (Failure ("could not find a class name for id " ^ s))
  | hd::tl -> try (match (StringHash.find hd s).typ with
                     A.Class(class_name) -> class_name
                   | _ -> (lookup_class_name tl s))
              with Not_found -> (lookup_class_name tl s)

let get_static_var_name class_name var_name = class_name ^ "." ^ var_name

let get_class_name kind v_symbol_tables = function
  (_, SSelf) -> lookup_class_name v_symbol_tables "self"
| (A.Class(class_name), _) -> class_name
| _ -> raise (Failure("The LHS expression of a " ^ kind ^ " is expected to be a class type."))

let check_not_zero_fname = function
  A.Primitive(A.Int) -> "check_int_not_zero"
| A.Primitive(A.Long) -> "check_long_not_zero"
| A.Primitive(A.Float) -> "check_float_not_zero"
| _ -> raise (Failure("check_not_zero is not supported for the given type."))

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

  (* Need to build a struct type for every class *)
  let class_name_to_named_struct =
    let helper m e = match e with
      SClassdecl(scd) -> (StringMap.add scd.scname (L.named_struct_type context scd.scname) m)
    | _ -> m in
  List.fold_left helper StringMap.empty sp_units
  in
  (* Return the LLVM type for a Boomslang type *)
  let rec ltype_of_typ = function
    A.Primitive(A.Int)    -> i32_t
  | A.Primitive(A.Long)   -> i64_t
  | A.Primitive(A.Float)  -> float_t
  | A.Primitive(A.Char)   -> i8_t
  | A.Primitive(A.String) -> str_t
  | A.Primitive(A.Bool)   -> i1_t
  | A.Primitive(A.Void)   -> void_t
  (* Classes, arrays, and null type *)
  (* Classes always get passed around as pointers to the memory where the full struct is stored *)
  | A.Class(class_name)   -> L.pointer_type (StringMap.find class_name class_name_to_named_struct)
  | A.Array(typ)          -> L.pointer_type (L.array_type (ltype_of_typ typ) 100) (* TODO remove hardcoded 100 *)
  | _                     -> void_t (* TODO remove this and fill in other types *)
  in
  let get_bind_from_assign = function
    SRegularAssign(typ, name, _) -> (typ, name)
  | SStaticAssign(_, typ, name, _) -> (typ, name)
  in
  let helper = function
    SClassdecl(scd) ->
      (* In order to have it work recursively, first you have to use a named struct type.
         Then you have to fill in the body. *)
      let elts = (Array.of_list (List.map ltype_of_typ (List.map (fst) (scd.srequired_vars @ (List.map get_bind_from_assign scd.soptional_vars))))) in
      L.struct_set_body (StringMap.find scd.scname class_name_to_named_struct) elts false
    | _ -> () in
  let _ = List.iter helper sp_units in
  let get_lvalue_of_bool = function
    true -> (L.const_int (ltype_of_typ (A.Primitive(A.Bool))) 1)
  | false -> (L.const_int (ltype_of_typ (A.Primitive(A.Bool))) 0)
  in

  (* define the default values for all the types *)
  let rec default_val_of_typ typ builder = match typ with
    A.Primitive(A.Int)    -> L.const_int i32_t 0
  | A.Primitive(A.Long)   -> L.const_int i64_t 0
  | A.Primitive(A.Float)  -> L.const_float float_t 0.0
  | A.Primitive(A.Char)   -> L.const_int i8_t 0
  | A.Primitive(A.String) -> L.build_global_stringptr "" "" builder
  | A.Primitive(A.Bool)   -> L.const_int i1_t 0
  | A.Class(name)         -> L.const_pointer_null (ltype_of_typ (A.Class(name)))
  | A.Array(typ)          ->
      let size = 100 in (* TODO remove hardcoded 100 *)
      let default = default_val_of_typ typ builder in
      (* always put the array literal in the heap, maybe find a way to free this memory later *)
      let arrp = L.build_malloc (L.array_type (ltype_of_typ typ) size) "arrp" builder  in
      (* for each element of the array, set the value to the default value *)
      let _ = 
        let rec helper i =
         if i = size then ()
         else (
          ignore (L.build_store default (L.build_gep arrp [| L.const_int i64_t 0 ; L.const_int i64_t i |] "" builder) builder);
          helper (i + 1)) in
        helper 0 in arrp
  | _                     -> L.const_null i32_t (* TODO remove this and fill in other types *)
  in

  (* create a map of all of the built in functions *)
  let built_in_map =
   let built_in_funcs : (string * A.typ * (A.typ list)) list =
     let convert (fs : (function_signature * A.typ)) =
       (((fst fs).fs_name), (snd fs), ((fst fs).formal_types)) in
     List.map convert Semant.built_in_funcs in
   let helper m e = match e with fun_name, ret_t, arg_ts -> 
    let arg_t_arr = Array.of_list 
                    (List.fold_left (fun s e -> s @ (if (e = (A.Primitive(A.Void))) then [(L.pointer_type i8_t)] else [ltype_of_typ e])) [] arg_ts) in
    let func = L.declare_function fun_name 
              (L.var_arg_function_type (ltype_of_typ ret_t) arg_t_arr) the_module in
    let signature = { fs_name = fun_name; formal_types = arg_ts } in
    SignatureMap.add signature func m in
   List.fold_left helper SignatureMap.empty built_in_funcs in

  (* create a map of all the user defined functions *)
  let user_func_map =
    let helper m e = match e with 
      SFdecl(sf) ->
        let func_t = L.function_type (ltype_of_typ sf.srtype) (Array.of_list
          (List.fold_left (fun s e -> match e with typ, _ -> s @ [ltype_of_typ typ])
          [] sf.sformals)) in
        let func = L.define_function (sf.sfname ^ "_usr") func_t the_module in
        let signature = { fs_name = sf.sfname; formal_types =
          List.fold_left (fun s (typ, _) -> s @ [typ]) [] sf.sformals } in
        SignatureMap.add signature func m
    | _ -> m in
    List.fold_left helper SignatureMap.empty sp_units in

  (* builds a map from string to map of function signature to LLVM function definition *)
  let class_signature_map =
    let helper1 m1 e1 = match e1 with
      SClassdecl(scd) ->
        let helper2 m2 mdecl =
          let func_t = L.function_type (ltype_of_typ mdecl.srtype) (Array.of_list
            (ltype_of_typ (A.Class(scd.scname))::
            (List.fold_left (fun s e -> match e with typ, _ -> s @ [ltype_of_typ typ]) [] mdecl.sformals))
          ) in
          let func = L.define_function (mdecl.sfname ^ "_classmethod") func_t the_module in
          let signature = { fs_name = mdecl.sfname; formal_types =
            List.fold_left (fun s (typ, _) -> s @ [typ]) [] mdecl.sformals } in
          SignatureMap.add signature func m2 in
        StringMap.add scd.scname (List.fold_left helper2 SignatureMap.empty scd.smethods) m1
    | _ -> m1 in
    List.fold_left helper1 StringMap.empty sp_units in

  (* to get an element from the struct, we have to use its index in the struct, rather
     than the name. this is annoying, but there doesn't seem to be a way to avoid it. *)
  let class_name_to_decl =
    let helper1 m1 e1 = match e1 with
      SClassdecl(scd) -> StringMap.add scd.scname scd m1
    | _ -> m1 in
  List.fold_left helper1 StringMap.empty sp_units in
  let rec find x lst = (* stolen from https://stackoverflow.com/questions/31279920/finding-an-item-in-a-list-and-returning-its-index-ocaml *)
    match lst with
    | [] -> raise (Failure("Index for v_name " ^ x ^ " not found"))
    | h :: t -> if x = h then 0 else 1 + find x t
  in
  let get_index_in_class class_name v_name =
    let scdecl = StringMap.find class_name class_name_to_decl in
    (find v_name (List.map (snd) (scdecl.srequired_vars @ (List.map get_bind_from_assign scdecl.soptional_vars))))
  in
  let get_name_of_assign = function
    SRegularAssign(_, lhs_name, _) -> lhs_name
  | SStaticAssign(_, _, lhs_name, _) -> lhs_name
  in
  let is_static_variable class_name v_name =
    let scdecl = StringMap.find class_name class_name_to_decl in
    List.mem v_name (List.map get_name_of_assign scdecl.sstatic_vars)
  in

  (* expression builder *)
  let rec build_expr builder v_symbol_tables (exp : sexpr) = match exp with
    _, SIntLiteral(i)      -> L.const_int i32_t i
  | _, SLongLiteral(l)     -> L.const_of_int64 i64_t l true
  | _, SFloatLiteral(f)    -> L.const_float_of_string float_t f
  | _, SCharLiteral(c)     -> L.const_int i8_t (Char.code c)
  | _, SStringLiteral(str) -> L.build_global_stringptr str "STRINGLITERAL" builder
  | _, SBoolLiteral(true)  -> L.const_int i1_t 1
  | _, SBoolLiteral(false) -> L.const_int i1_t 0
  | _, SId(id)             -> L.build_load (lookup v_symbol_tables id) id builder
  | _, SSelf               -> L.build_load (lookup v_symbol_tables "self") "self" builder
  | typ, SNullExpr         -> L.const_pointer_null (ltype_of_typ typ)
  (* Function calls *)
  | typ, SCall(sc) -> (match sc with
      (* Nulls must be handled with care *)
        SFuncCall("null_to_string", [(_, SNullExpr)]) -> build_expr builder v_symbol_tables (A.Primitive(A.String), SStringLiteral("NULL"))
      | SFuncCall(func_name, expr_list) ->
          let expr_typs = List.fold_left (fun s (t, _) -> s @ [t]) [] expr_list in
          let signature_with_possible_nulls = { fs_name = func_name; formal_types = expr_typs } in
          if SignatureMap.mem signature_with_possible_nulls built_in_map then (* is a built in func *)
            L.build_call (SignatureMap.find signature_with_possible_nulls built_in_map)
            (Array.of_list (List.fold_left (fun s e -> s @ [build_expr builder v_symbol_tables e])
            [] expr_list))
            (if typ = A.Primitive(A.Void) then "" else (func_name ^ "_res"))
            builder
          else (* is a user defined func *)
            let matching_signature = S.find_matching_signature signature_with_possible_nulls user_func_map in
            L.build_call (SignatureMap.find matching_signature user_func_map)
            (Array.of_list (List.fold_left (fun s e -> s @ [build_expr builder v_symbol_tables e])
            [] expr_list))
            (if typ = A.Primitive(A.Void) then "" else (func_name ^ "_res"))
            builder
      | SMethodCall(expr, method_name, expr_list) ->
          let expr_typs = List.fold_left (fun s (t, _) -> s @ [t]) [] expr_list in
          let signature_with_possible_nulls = { fs_name = method_name; formal_types = expr_typs } in
          let class_name = get_class_name "method call" v_symbol_tables expr in
          let class_signatures = StringMap.find class_name class_signature_map in
          let signature = S.find_matching_signature signature_with_possible_nulls class_signatures in
          if SignatureMap.mem signature class_signatures then (* is a user defined method *)
            let expr' = build_expr builder v_symbol_tables expr in
            L.build_call (SignatureMap.find signature class_signatures)
            (Array.of_list (expr'::(List.fold_left (fun s e -> s @ [build_expr builder v_symbol_tables e])
            [] (expr_list))))
            (if typ = A.Primitive(A.Void) then "" else (class_name ^ "_" ^ method_name ^ "_res"))
            builder
          else raise (Failure ("method " ^ method_name ^ " not found on class " ^ class_name))
      )
  | _, SObjectInstantiation(class_name, expr_list) ->
    let expr_typs = List.fold_left (fun s (t, _) -> s @ [t]) [] expr_list in
    let signature_with_possible_nulls = { fs_name = "construct"; formal_types = expr_typs } in
    let class_signatures = StringMap.find class_name class_signature_map in
    let signature = S.find_matching_signature signature_with_possible_nulls class_signatures in
    if SignatureMap.mem signature class_signatures then (* found a valid constructor *)
      (* first, create an empty struct of the right type. *)
      (* this is the one place where we DON'T use the pointer version of the struct type *)
      let struct_malloc = L.build_malloc (* (ltype_of_typ (A.Class(class_name))) *) (StringMap.find class_name class_name_to_named_struct) "" builder in
      (* then call the constructor to initialize it properly *)
      ignore (L.build_call (SignatureMap.find signature class_signatures)
        (Array.of_list (struct_malloc::(List.fold_left (fun s e -> s @ [build_expr builder v_symbol_tables e])
         [] expr_list)))
        "" builder); struct_malloc  (* We call the constructor function, but return the llvalue for the struct *)
    else raise (Failure ("constructor function for " ^ class_name ^ " not found"))
  | _, SObjectVariableAccess(sova) ->
     let expr = sova.sova_sexpr in
     let class_name = sova.sova_class_name in
     let var_name = sova.sova_var_name in
     let is_static = sova.sova_is_static in
     if is_static then
       (* This is the case where we access static var x like MyClass.x instead of myinstance.x *)
       (let static_var_name = get_static_var_name class_name var_name in
        L.build_load (lookup v_symbol_tables static_var_name) static_var_name builder)
     else
       let class_name = get_class_name "object variable access" v_symbol_tables expr in
       if (is_static_variable class_name var_name) then
         (let static_var_name = get_static_var_name class_name var_name in
          L.build_load (lookup v_symbol_tables static_var_name) static_var_name builder)
       else
         (let index_in_class = (get_index_in_class class_name var_name) in
          (* Check that the object whose variable we are trying to access isn't null *)
          let expr' = build_expr builder v_symbol_tables expr in
          let bitcast = L.build_bitcast expr' (L.pointer_type i8_t) "bcast" builder in
          let _ = L.build_call (SignatureMap.find ({ fs_name = "check_not_null"; formal_types = [A.Primitive(A.Void)] }) built_in_map) (Array.of_list [bitcast]) "" builder in
          let gep = L.build_struct_gep expr' index_in_class var_name builder in
          L.build_load gep "" builder)
  | _, SArrayAccess(sexpr1, sexpr2) ->
      let n = build_expr builder v_symbol_tables sexpr2 in (* the integer (as an llvalue) we are indexing to *)
      let arr = build_expr builder v_symbol_tables sexpr1 in
      let elemp = L.build_gep arr [| L.const_int i64_t 0 ; n |] "gep_of_arr" builder in
      L.build_load elemp ("arr_elem") builder
  | A.Array(typ) , SArrayLiteral(sexpr_list) ->
      (* create list of llvalue from the evaluated sexpr list *)
      let llvalue_arr = List.fold_left (fun s sexpr -> s @ [build_expr builder v_symbol_tables sexpr])
                         [] sexpr_list in
      (* always put the array literal in the heap, maybe find a way to free this memory later *)
      let arrp = L.build_malloc (L.array_type (ltype_of_typ typ) (List.length sexpr_list)) "arrp" builder  in
      (* for each element of the array, gep and store value *)
      let _ = List.fold_left 
              (fun i e ->  ignore (L.build_store e (L.build_gep arrp [| L.const_int i64_t 0 ; L.const_int i64_t i |] 
              "" builder) builder); i + 1) 0 llvalue_arr in
      arrp
  | A.Array(typ1), SDefaultArray(_, _) ->
      default_val_of_typ (A.Array(typ1)) builder
  (* == is the only binop that can apply to any two types. *)
  | _, SBinop(sexpr1, A.DoubleEq, sexpr2) ->
      let sexpr1' = build_expr builder v_symbol_tables sexpr1
      and sexpr2' = build_expr builder v_symbol_tables sexpr2 in
      let sexpr1typ = (fst sexpr1)
      and sexpr2typ = (fst sexpr2)
      and sexpr1sx = (snd sexpr1)
      and sexpr2sx = (snd sexpr2)
      in
      if ((sexpr1typ = A.NullType || sexpr1sx = SNullExpr) && (sexpr2typ = A.NullType || sexpr2sx = SNullExpr)) then
        (get_lvalue_of_bool true)
      else if (sexpr1typ = A.NullType || sexpr1sx = SNullExpr) then
        (* Only an object (not a primitive nor array) may be Null. *)
        (match sexpr2typ with
             A.Class(_) -> L.build_is_null sexpr2' "" builder
           | _ -> L.const_int (ltype_of_typ (A.Primitive(A.Bool))) 0)
      else if (sexpr2typ = A.NullType || sexpr2sx = SNullExpr) then
        (match sexpr1typ with
             A.Class(_) -> L.build_is_null sexpr1' "" builder
           | _ -> L.const_int (ltype_of_typ (A.Primitive(A.Bool))) 0)
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
              let signature = { fs_name = "compare_strings"; formal_types = [(fst sexpr1); (fst sexpr2)] } in
              L.build_call (SignatureMap.find signature built_in_map)
                (Array.of_list (List.fold_left (fun s e -> s @ [build_expr builder v_symbol_tables e]) [] [sexpr1; sexpr2]))
                (signature.fs_name ^ "_res") builder
          | A.Class(_) -> L.build_icmp L.Icmp.Eq (L.const_int i64_t 0) (L.build_ptrdiff sexpr1' sexpr2' "" builder) "" builder (* TODO this is just a placeholder *)
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
        | A.Divide       ->
          (* Check no divide by zero *)
          let typ = (fst sexpr1) in
          let error_message = build_expr builder v_symbol_tables ((A.Primitive(A.String)), SStringLiteral("DivideByZeroException")) in
          let _ = L.build_call (SignatureMap.find ({ fs_name = (check_not_zero_fname typ); formal_types = [typ; A.Primitive(A.String)] }) built_in_map) (Array.of_list [sexpr2'; error_message]) "" builder in
          L.build_sdiv (* signed division*)
        | A.Modulo       ->
          (* Check no mod by zero *)
          let typ = (fst sexpr1) in
          let error_message = build_expr builder v_symbol_tables ((A.Primitive(A.String)), SStringLiteral("ModByZeroException")) in
          let _ = L.build_call (SignatureMap.find ({ fs_name = (check_not_zero_fname typ); formal_types = [typ; A.Primitive(A.String)] }) built_in_map) (Array.of_list [sexpr2'; error_message]) "" builder in
          L.build_srem (* signed remainder *)
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
        | A.Divide       ->
          (* Check no divide by zero *)
          let typ = (fst sexpr1) in
          let error_message = build_expr builder v_symbol_tables ((A.Primitive(A.String)), SStringLiteral("DivideByZeroException")) in
          let _ = L.build_call (SignatureMap.find ({ fs_name = (check_not_zero_fname typ); formal_types = [typ; A.Primitive(A.String)] }) built_in_map) (Array.of_list [sexpr2'; error_message]) "" builder in
          L.build_fdiv (* signed division*)
        | A.Modulo       ->
          (* Check no mod by zero *)
          let typ = (fst sexpr1) in
          let error_message = build_expr builder v_symbol_tables ((A.Primitive(A.String)), SStringLiteral("ModByZeroException")) in
          let _ = L.build_call (SignatureMap.find ({ fs_name = (check_not_zero_fname typ); formal_types = [typ; A.Primitive(A.String)] }) built_in_map) (Array.of_list [sexpr2'; error_message]) "" builder in
          L.build_frem (* signed remainder *)
        | A.DoubleEq     -> L.build_fcmp L.Fcmp.Oeq (* ordered and equal to *)
        | A.BoGT         -> L.build_fcmp L.Fcmp.Ogt (* ordered and greater than *)
        | A.BoLT         -> L.build_fcmp L.Fcmp.Olt (* ordered and less than *)
        | A.BoGTE        -> L.build_fcmp L.Fcmp.Oge (* etc. *)
        | A.BoLTE        -> L.build_fcmp L.Fcmp.Ole
        | _ -> raise (Failure("Found ineligible binop for float operands"))
      ) sexpr1' sexpr2' "tmp" builder
  (* String binops (the only one supported is + for concatenate) *)
  | _, SBinop(((A.Primitive(A.String), _) as sexpr1), binop, ((A.Primitive(A.String), _) as sexpr2)) ->
      (match binop with
          A.Plus         ->
            let signature = { fs_name = "concat_strings"; formal_types = [(fst sexpr1); (fst sexpr2)] } in
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
  | A.Array(arrtyp), SAssign(SRegularAssign(_, name, sexpr)) ->
      let arrp = build_expr builder v_symbol_tables sexpr in (* get pointer for arr literal *)
      let arrp_typ = L.type_of arrp in
      if List.length v_symbol_tables = 1 then
        (* Build a global *)
        let global_symbol_table = List.hd v_symbol_tables in
        let declared_global = (L.declare_global arrp_typ name the_module) in
        let _ = L.set_initializer (L.const_null arrp_typ) declared_global in
        ((StringHash.add global_symbol_table name { llvalue = declared_global; typ = A.Array(arrtyp) });
        ignore(L.build_store arrp (lookup v_symbol_tables name) builder));
        arrp
      else
        (* Build a local. This means allocating space on the stack, and then
           storing the value of the expr there. *)
        let this_scopes_symbol_table = List.hd v_symbol_tables in
        let new_symbol_table_entry = { llvalue = (L.build_alloca arrp_typ name builder); typ = A.Array(arrtyp) } in
        ((StringHash.add this_scopes_symbol_table name new_symbol_table_entry);
        ignore(L.build_store arrp (lookup v_symbol_tables name) builder));
        arrp
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
        ((StringHash.add global_symbol_table name { llvalue = declared_global; typ = typ });
        ignore(L.build_store e' (lookup v_symbol_tables name) builder));
        e'
      else
        (* Build a local. This means allocating space on the stack, and then
           storing the value of the expr there. *)
        let this_scopes_symbol_table = List.hd v_symbol_tables in
        let new_symbol_table_entry = { llvalue = (L.build_alloca (ltype_of_typ typ) name builder); typ = typ } in
        ((StringHash.add this_scopes_symbol_table name new_symbol_table_entry);
        ignore(L.build_store e' (lookup v_symbol_tables name) builder));
        e'
  | typ1, SAssign(SStaticAssign(class_name, typ2, var_name, sexpr)) ->
      (* . can never be part of an identifier in our language, but it can in LLVM.
         Thus we use the LLVM global name to handle static variables, as the class name
         basically gives each static var a unique prefix and unique id that won't
         conflict with other global variables or other class static variables. *)
      build_expr builder v_symbol_tables (typ1, (SAssign(SRegularAssign(typ2, (get_static_var_name class_name var_name), sexpr))))
  | _, SUpdate(SRegularUpdate(name, A.Eq, sexpr)) ->
      let e' = build_expr builder v_symbol_tables sexpr in
      ignore(L.build_store e' (lookup v_symbol_tables name) builder); e'
  | typ, SUpdate(SObjectVariableUpdate(sova, A.Eq, rhs_sexpr)) ->
      let lhs_sexpr = sova.sova_sexpr in
      let class_name = sova.sova_class_name in
      let var_name = sova.sova_var_name in
      let is_static = sova.sova_is_static in
      let rhs_expr' = build_expr builder v_symbol_tables rhs_sexpr in
      let lhs_expr' = build_expr builder v_symbol_tables lhs_sexpr in
      if is_static then
        let static_var_name = get_static_var_name class_name var_name in
        build_expr builder v_symbol_tables (typ, SUpdate(SRegularUpdate(static_var_name, A.Eq, rhs_sexpr)))
      else
        let class_name = get_class_name "object variable update" v_symbol_tables lhs_sexpr in
        if (is_static_variable class_name var_name) then
          let static_var_name = get_static_var_name class_name var_name in
          build_expr builder v_symbol_tables (typ, SUpdate(SRegularUpdate(static_var_name, A.Eq, rhs_sexpr)))
        else
          (* First check the LHS is not null before assigning to it. *)
          (let bitcast = L.build_bitcast lhs_expr' (L.pointer_type i8_t) "bcast" builder in
           let _ = L.build_call (SignatureMap.find ({ fs_name = "check_not_null"; formal_types = [A.Primitive(A.Void)] }) built_in_map) (Array.of_list [bitcast]) "" builder in
           let gep = L.build_struct_gep lhs_expr' (get_index_in_class class_name var_name) var_name builder in
           ignore(L.build_store rhs_expr' gep builder); rhs_expr')
  | _, SUpdate(SArrayAccessUpdate((sexpr_arr, sexpr_index), A.Eq, sexpr)) ->
      let newvalue = build_expr builder v_symbol_tables sexpr in
      let n = build_expr builder v_symbol_tables sexpr_index in (* the integer (as an llvalue) we are indexing to *)
      let arr = build_expr builder v_symbol_tables sexpr_arr in (* load in arr *)
      let elemp = L.build_gep arr [| L.const_int i64_t 0 ; n |] "" builder in
      let _ = L.build_store newvalue elemp builder in
      newvalue
  | _ -> raise (Failure("unimplemented expr in codegen"))
  in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) in
  (* statement builder *) 
  let rec build_stmt the_function v_symbol_tables builder (ss : sstmt) = match ss with
    SExpr(se)   -> ignore (build_expr builder v_symbol_tables se); builder
  | SReturn(sexpr) -> ignore (L.build_ret (build_expr builder v_symbol_tables sexpr) builder);
                      builder
  | SReturnVoid -> ignore (L.build_ret_void builder); builder
  | SIf (predicate, then_stmts, elif_stmts, else_stmts) ->
    let bool_val = build_expr builder v_symbol_tables predicate in
    if ((List.length elif_stmts) = 0 && (List.length else_stmts) = 0) then
      let merge_bb = L.append_block context "merge" the_function in
      let b_br_merge = L.build_br merge_bb in
      let then_bb = L.append_block context "then" the_function in
      let _ = add_terminal (build_stmt_list the_function ((StringHash.create 10)::v_symbol_tables) (L.builder_at_end context then_bb) then_stmts) b_br_merge in
      (ignore(L.build_cond_br bool_val then_bb merge_bb builder));
      L.builder_at_end context merge_bb
    else
      let then_bb = L.append_block context "then" the_function in
      let then_stmts_builder = build_stmt_list the_function ((StringHash.create 10)::v_symbol_tables) (L.builder_at_end context then_bb) then_stmts in
      let else_bb = L.append_block context "else" the_function in
      let else_stmts_builder =
        if (List.length elif_stmts = 0) then
          (build_stmt_list the_function ((StringHash.create 10)::v_symbol_tables) (L.builder_at_end context else_bb) else_stmts)
        else
          let first_elif = List.hd elif_stmts in
          let first_elif_predicate = fst first_elif in
          let first_elif_stmts = snd first_elif in
          (build_stmt the_function ((StringHash.create 10)::v_symbol_tables) (L.builder_at_end context else_bb) (SIf(first_elif_predicate, first_elif_stmts, (List.tl elif_stmts), else_stmts)))
      in
      (match (L.block_terminator (L.insertion_block then_stmts_builder)) with
         Some(_) -> (
                     (match (L.block_terminator (L.insertion_block else_stmts_builder)) with
                        Some(_) -> (ignore(L.build_cond_br bool_val then_bb else_bb builder);
                                    builder
                                   )
                      | None -> (let merge_bb = L.append_block context "merge" the_function in
                                 let b_br_merge = L.build_br merge_bb in
                                 let _ = add_terminal else_stmts_builder b_br_merge in
                                 ignore(L.build_cond_br bool_val then_bb else_bb builder);
                                 L.builder_at_end context merge_bb
                                )
                     )
                    )
       | None -> (let merge_bb = L.append_block context "merge" the_function in
                  let b_br_merge = L.build_br merge_bb in
                  let _ = add_terminal then_stmts_builder b_br_merge in
                  let _ = add_terminal else_stmts_builder b_br_merge in
                  ignore(L.build_cond_br bool_val then_bb else_bb builder);
                  L.builder_at_end context merge_bb
                 )
      )
  | SLoop(update, predicate, body_stmt_list) ->
    let pred_bb = L.append_block context "while" the_function in
    ignore(L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in

    let while_stmts_builder = (build_stmt_list the_function ((StringHash.create 10)::v_symbol_tables) (L.builder_at_end context body_bb) body_stmt_list) in
    let _ = (match (L.block_terminator (L.insertion_block while_stmts_builder)) with
       Some(_) -> () (* The body already returns, so no need to add the update expression to the end, nor do we need to add a terminal *)
     (* The body did not already return. So we need to do 2 things: Add the update expression and a terminal *)
     | None -> add_terminal (build_stmt the_function v_symbol_tables while_stmts_builder (SExpr(update))) (L.build_br pred_bb)
    ) in

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = build_expr pred_builder v_symbol_tables predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb
  and
  build_stmt_list the_function v_symbol_tables builder stmt_list = 
    List.fold_left (build_stmt the_function v_symbol_tables) builder stmt_list
  in

  (* function declaration builder *)
  let build_func v_symbol_tables class_name builder (sf : sfdecl) =
    let signature = { fs_name = sf.sfname; formal_types =
      List.fold_left (fun s (typ, _) -> s @ [typ]) [] sf.sformals } in
    let func =
      if class_name = "" then
        SignatureMap.find signature user_func_map
      else
        SignatureMap.find signature (StringMap.find class_name class_signature_map)
    in
    let func_builder = L.builder_at_end context (L.entry_block func) in
    (* allocs formals in the stack *)
    let alloca_formal s (typ, name) = 
      s @ [{ llvalue = (L.build_alloca (ltype_of_typ typ) name func_builder) ; typ = typ }] in
    let stack_vars =
      if class_name = "" then
        List.fold_left alloca_formal [] sf.sformals
      else
        let class_typ = (A.Class(class_name)) in
        let fst_stmt = { llvalue = (L.build_alloca ((ltype_of_typ class_typ)) "self" func_builder); typ = class_typ} in
        fst_stmt::(List.fold_left alloca_formal [] sf.sformals)
    in
    (* stores pointers to the stack location of the formal args *)
    let rec store_formals param stack_p = match param, stack_p with
      [], [] -> []
    | hd1::[], hd2::[] -> [L.build_store hd1 hd2.llvalue func_builder]
    | hd1::tl1, hd2::tl2 -> let fst_stmt = (L.build_store hd1 hd2.llvalue func_builder) in
                            fst_stmt::(store_formals tl1 tl2)
    | _ -> raise (Failure "store_formals array mismatch!") in
    (* add a new elem in this function's v_symbol_tables and add formals *)
    let this_scopes_symbol_table = StringHash.create 10 in
    let v_symbol_tables = this_scopes_symbol_table::v_symbol_tables in
    let _ =
      ignore (store_formals (Array.to_list (L.params func)) stack_vars);
      List.iter (fun elem -> StringHash.add this_scopes_symbol_table
                                  (L.value_name elem.llvalue) elem) stack_vars in
    let last_builder = List.fold_left (fun builder stmt -> 
           build_stmt func v_symbol_tables builder stmt) func_builder sf.sbody in
   (* if user didn't specify return on void function, then add it ourselves *)
    let _ = if (sf.srtype = A.Primitive(A.Void)) &&
           (not (List.mem SReturnVoid sf.sbody)) then (* TODO does this need to be updated to be more robust for branches? *)
           ignore (L.build_ret_void last_builder) in
   builder in

  (* class declaration builder *)
  let sassign_to_sexpr = function
    SRegularAssign(lhs_typ, _, _) as sra -> (lhs_typ, (SAssign(sra)))
  | SStaticAssign(_, lhs_typ, _, _) as ssa -> (lhs_typ, (SAssign(ssa)))
  in
  let build_class v_symbol_tables builder (sc : sclassdecl) =
    (* First build the struct type in LLVM, this will be important *)
    (* Classes can have other classes as their fields - these are just pointers *)
    (* loop over all the static vars *)
    let _ = List.map (build_expr builder v_symbol_tables) (List.map sassign_to_sexpr sc.sstatic_vars) in
    (* Then loop through all the fdecls, including constructors *)
    let _ = (List.fold_left (build_func v_symbol_tables sc.scname) builder sc.smethods) in
    builder
  in
  
  (* LLVM requires a 'main' function as an entry point *)
  let main_t : L.lltype =
      L.var_arg_function_type i32_t [| |] in
  let main_func : L.llvalue =
    L.define_function "main" main_t the_module in
  let main_builder = L.builder_at_end context (L.entry_block main_func) in

  (* program builder *) 
  let build_program v_symbol_tables builder (spunit : sp_unit) = match spunit with
    SStmt(ss)       -> build_stmt main_func v_symbol_tables builder ss
  | SFdecl(sf)      -> build_func v_symbol_tables "" builder sf
  | SClassdecl(sc)  -> build_class v_symbol_tables builder sc in
     
  let final_builder = List.fold_left (build_program [StringHash.create 10]) main_builder sp_units in
  ignore (L.build_ret (L.const_int i32_t 0) final_builder); (* build return for main *)
  the_module
