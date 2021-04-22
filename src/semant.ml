(* Semantic checking for the Boomslang compiler *)

open Ast
open Sast

type lhsrhs = {
  lhs: typ;
  rhs: typ;
}

module StringMap = Map.Make(String);;
module SignatureMap = Map.Make(struct type t = function_signature let compare = compare end);;
module TypMap = Map.Make(struct type t = typ let compare = compare end);;
module LhsRhsMap = Map.Make(struct type t = lhsrhs let compare = compare end);;
module StringHash = Hashtbl.Make(struct
  type t = string (* type of keys *)
  let equal x y = x = y (* use structural comparison *)
  let hash = Hashtbl.hash (* generic hash function *)
end);;

let built_in_funcs = [
  ({ fs_name = "println"; formal_types = [Primitive(String)] }, Primitive(Void));
  ({ fs_name = "int_to_string"; formal_types = [Primitive(Int)] }, Primitive(String));
  ({ fs_name = "long_to_string"; formal_types = [Primitive(Long)] }, Primitive(String));
  ({ fs_name = "float_to_string"; formal_types = [Primitive(Float)] }, Primitive(String));
  ({ fs_name = "char_to_string"; formal_types = [Primitive(Char)] }, Primitive(String));
  ({ fs_name = "bool_to_string"; formal_types = [Primitive(Bool)] }, Primitive(String));
  ({ fs_name = "int_to_long"; formal_types = [Primitive(Int)] }, Primitive(Long));
  ({ fs_name = "int_to_float"; formal_types = [Primitive(Int)] }, Primitive(Float));
  ({ fs_name = "concat_strings"; formal_types = [Primitive(String); Primitive(String)] }, Primitive(String)); 
  ({ fs_name = "compare_strings"; formal_types = [Primitive(String); Primitive(String)] }, Primitive(Bool));
]

let type_is_nullable = function
  Primitive(_) -> false
| Class(_) -> true
| Array(_) -> false
| NullType -> true

let binop_method_name = function
  Plus  -> "_+"
| Subtract -> "_-"
| Times -> "_star"
| Divide -> "_slash"
| Modulo -> "_pct"
| DoubleEq -> "_eqeq"
| BoGT -> "_gt"
| BoLT -> "_lt"
| BoGTE -> "_gteq"
| BoLTE -> "_lteq"
| _ -> raise (Failure("Attempted to use an incompatible binary operator on an object type."))

let are_types_compatible typ1 typ2 =
  ((typ1 = typ2) || (type_is_nullable(typ1) && typ2 = NullType))

let signature_could_match signature1 signature2 _ =
  if (signature1.fs_name = signature2.fs_name) && ((List.length signature1.formal_types) = (List.length signature2.formal_types)) then
    List.for_all2 are_types_compatible (signature2.formal_types) (signature1.formal_types)
  else false

(* This function is complicated because if the language has nulls, you can't just look up the
   signature directly. However, since null could match multiple types, if a user calls a func like
   myfunc(null, null), it is impossible to tell if they meant myfunc(MyObject a, MyObject b) or
   myfunc(OtherObject a, OtherObject b). *)
let find_matching_signature signature signatures =
  let matching_signatures_map = SignatureMap.filter (signature_could_match signature) signatures in
  let matching_signatures_list = List.map (fst) (SignatureMap.bindings matching_signatures_map) in
  if List.length matching_signatures_list = 0 then
    raise (Failure("No matching signature found for function call " ^ signature.fs_name))
  else if List.length matching_signatures_list > 1 then
    raise (Failure("The call to " ^ signature.fs_name ^ " is ambiguous."))
  else List.hd matching_signatures_list

(* If the user is calling a function with NULLs as parameters, we need to
   convert the type associated with the NullExpr to the type of the formal
   that is expected. This will help LLVM generate the right kind of null
   pointer. *)
let convert_nulls_in_checked_exprs checked_exprs matching_signature =
  let convert checked_expr typ = match (fst checked_expr) with
    NullType -> (typ, (snd checked_expr))
  | _ -> checked_expr in
  List.map2 convert checked_exprs matching_signature.formal_types

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong. *)

(* Add built-in functions *)
let check original_program =

let rec type_of_identifier v_symbol_tables s =
  match v_symbol_tables with
  [] -> raise (Failure ("undeclared identifier " ^ s))
  | hd::tl -> try StringHash.find hd s
              with Not_found -> (type_of_identifier tl s)
in

let rec dups kind = function (* Stolen from microc *)
      [] -> ()
    | (n1 :: n2 :: _) when n1 = n2 ->
       raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
    | _ :: t -> dups kind t
in

let check_type_is_int = function
  Primitive(Int) -> ()
| _ -> raise (Failure("Expected expr of type int"))
in
let check_type_is_bool = function
  Primitive(Bool) -> ()
| _ -> raise (Failure("Expected expr of type bool"))
in

let add_to_hash hash bind = StringHash.add hash (snd bind) (fst bind) in
let get_hash_of_binds bind_list =
  let hash = (StringHash.create (List.length bind_list)) in
  (List.iter (add_to_hash hash) bind_list); hash
in

let add_built_in map built_in = SignatureMap.add (fst built_in) (snd built_in) map
in
let built_in_func_map = List.fold_left add_built_in SignatureMap.empty built_in_funcs
in

(* Functions to coerce one type into another via a built-in function *)
let wrap_to_string checked_exprs = match checked_exprs with
  [(Primitive(Int), _)] -> (Primitive(String), SCall(SFuncCall("int_to_string", checked_exprs)))
| [(Primitive(Long), _)] -> (Primitive(String), SCall(SFuncCall("long_to_string", checked_exprs)))
| [(Primitive(Float), _)] -> (Primitive(String), SCall(SFuncCall("float_to_string", checked_exprs)))
| [(Primitive(Char), _)] -> (Primitive(String), SCall(SFuncCall("char_to_string", checked_exprs)))
| [(Primitive(Bool), _)] -> (Primitive(String), SCall(SFuncCall("bool_to_string", checked_exprs)))
| [(_, SNullExpr)] -> (Primitive(String), SCall(SFuncCall("null_to_string", checked_exprs)))
| [(Class(_), _) as sexpr] -> (Primitive(String), SCall(SMethodCall(sexpr, "to_string", [])))
| _ -> raise (Failure("Expected exactly 1 expression of a non-void primitive, null, or named object type."))
in
let wrap_int_to_long checked_exprs = match checked_exprs with
  [(Primitive(Int), _)] -> (Primitive(Long), SCall(SFuncCall("int_to_long", checked_exprs)))
| _ -> raise (Failure("Expected exactly 1 expression of type int."))
in
let wrap_int_to_float checked_exprs = match checked_exprs with
  [(Primitive(Int), _)] -> (Primitive(Float), SCall(SFuncCall("int_to_float", checked_exprs)))
| _ -> raise (Failure("Expected exactly 1 expression of type int."))
in
let coerceable_types = [
  ({ lhs = Primitive(Long); rhs = Primitive(Int) }, wrap_int_to_long);
  ({ lhs = Primitive(Float); rhs = Primitive(Int) }, wrap_int_to_float);
] in
let add_to_map map coerceable_type = LhsRhsMap.add (fst coerceable_type) (snd coerceable_type) map in
let coerceable_types_map = List.fold_left add_to_map LhsRhsMap.empty coerceable_types
in

(* First, figure out all the defined functions *)
let get_signature fdecl = { fs_name = fdecl.fname; formal_types = List.map fst fdecl.formals }
in
let sget_signature sfdecl = { fs_name = sfdecl.sfname; formal_types = List.map fst sfdecl.sformals }
in
let add_fdecl map fdecl =
  let signature = (get_signature fdecl) in
  if SignatureMap.mem signature map then raise (Failure(("Duplicate function signatures detected for " ^ fdecl.fname)))
  else SignatureMap.add signature fdecl.rtype map
in
let add_signature map = function
  Fdecl(fdecl) -> add_fdecl map fdecl
| _ -> map
in
let function_signatures = List.fold_left add_signature built_in_func_map original_program
in

(* For each class, generate 1-2 constructor signatures, corresponding to the required and optional fields. *)
let get_bind_from_assign = function
  RegularAssign(typ, name, _) -> (typ, name)
in
let get_required_only_signature classdecl =
  { fs_name = "construct"; formal_types = (List.map (fst) classdecl.required_vars) }
in
let get_required_and_optional_signature classdecl =
  let optional_var_binds = (List.map get_bind_from_assign classdecl.optional_vars) in
  { fs_name = "construct"; formal_types = (List.map (fst) (classdecl.required_vars @ optional_var_binds)) }
in
let get_autogenerated_constructor_signatures classdecl =
  if List.length classdecl.required_vars = 0 && List.length classdecl.optional_vars = 0 then [ {fs_name = "construct"; formal_types = [] } ]
  else if List.length classdecl.required_vars > 0 && List.length classdecl.optional_vars = 0 then [ get_required_only_signature classdecl ]
  else [(get_required_only_signature classdecl); (get_required_and_optional_signature classdecl)]
in

let _ = (* Check for duplicate class names *)
let add_classdecl map = function
  Classdecl(classdecl) ->
    if (StringMap.mem classdecl.cname map) then
      raise (Failure("Detected duplicate declarations for class " ^ classdecl.cname))
    else
      StringMap.add classdecl.cname classdecl map
| _ -> map
in
List.fold_left add_classdecl StringMap.empty original_program
in

let get_generic_to_actual actual_class generic_class =
  let generics_length = (List.length generic_class.generics) in
  let actuals_length = (List.length actual_class.generics) in
  if generics_length <> (actuals_length) then
    raise (Failure("Attempted to initialize " ^ generic_class.cname ^ " as " ^ actual_class.cname ^ " but " ^ (string_of_int generics_length) ^ " types were expected and " ^ (string_of_int actuals_length) ^ " were provided."))
  else
    let add_to_map map typ1 typ2 =
      if TypMap.mem typ1 map then (* Duplicate type name found *)
        raise (Failure("Found duplicate generic type " ^ (str_of_typ typ1) ^ " in class " ^ generic_class.cname))
      else TypMap.add typ1 typ2 map in
    List.fold_left2 add_to_map (TypMap.add (Class(generic_class.cname)) (Class(actual_class.cname)) TypMap.empty) generic_class.generics actual_class.generics
in
let generic_map =
let add_classdecl map = function
  Classdecl(classdecl) ->
    if ((List.length classdecl.generics) > 0) && classdecl.source_class_name = "" then
      StringMap.add classdecl.cname classdecl map
    else map
| _ -> map
in
List.fold_left add_classdecl StringMap.empty original_program
in
(* This method finds all class declarations of the form
   class MyClass = MyGenericClass(typ1, typ2) and converts
   them to full class declarations. This means looking up the generic
   type class MyGenericClass[T1, T2] and replacing all instances of T1
   with typ1 and all instances of T2 with typ2.
   This takes the entire program (list of p_units) and returns a new
   program that has all the generic classes removed, and all the
   generic class instantiations converted into fully usable classdecls.
   After this point, the original program should no longer be used.
*)
let rec convert_generic_typ generic_to_actual = function
  Primitive(_) as self -> self
| Class(_) as self -> if TypMap.mem self generic_to_actual then
                        TypMap.find self generic_to_actual
                      else self
| Array(typ) -> Array(convert_generic_typ generic_to_actual typ)
| NullType -> NullType
and
convert_generic_assign generic_to_actual = function
  RegularAssign(typ, id, expr) -> RegularAssign((convert_generic_typ generic_to_actual typ), id, expr)
and
convert_generic_bind generic_to_actual bind =
  let typ = (fst bind) in
  let name = (snd bind) in
  ((convert_generic_typ generic_to_actual typ), name)
and
convert_generic_ova generic_to_actual ova =
  let ce expr = convert_generic_expr generic_to_actual expr in
  if ova.ova_class_name <> "" then
    let new_class = (convert_generic_typ generic_to_actual (Class(ova.ova_class_name))) in
    (match new_class with
       Class(new_name) -> { ova_expr = (ce ova.ova_expr); ova_class_name = new_name; ova_var_name = ova.ova_var_name; ova_is_static = ova.ova_is_static }
     | _ -> raise (Failure("Somehow converted class type to something that was not a class - this should not be possible.")))
  else
    { ova_expr = (ce ova.ova_expr); ova_class_name = ova.ova_class_name; ova_var_name = ova.ova_var_name; ova_is_static = ova.ova_is_static }
and
convert_generic_expr generic_to_actual =
  let ce expr = convert_generic_expr generic_to_actual expr in
  let ces exprs = List.map (convert_generic_expr generic_to_actual) exprs in
  function
  Call(FuncCall(name, exprs)) -> Call(FuncCall(name, (ces exprs)))
| Call(MethodCall(expr1, name, exprs)) -> Call(MethodCall((ce expr1), name, (ces exprs)))
| ObjectInstantiation(old_name, exprs) ->
  let new_class = (convert_generic_typ generic_to_actual (Class(old_name))) in
  (match new_class with
     Class(new_name) -> ObjectInstantiation(new_name, (ces exprs))
   | _ -> raise (Failure("Somehow converted class type to something that was not a class - this should not be possible.")))
| ObjectVariableAccess(ova) -> ObjectVariableAccess((convert_generic_ova generic_to_actual ova))
| ArrayAccess(expr1, expr2) -> ArrayAccess((ce expr1), (ce expr2))
| ArrayLiteral(exprs) -> ArrayLiteral((ces exprs))
| DefaultArray(typ, exprs) -> DefaultArray((convert_generic_typ generic_to_actual typ), (ces exprs))
| Binop(expr1, binop, expr2) -> Binop((ce expr1), binop, (ce expr2))
| Unop(unaryop, expr) -> Unop(unaryop, (ce expr))
| Assign(assign) -> Assign(convert_generic_assign generic_to_actual assign)
| Update(RegularUpdate(name, updateop, expr)) -> Update(RegularUpdate(name, updateop, (ce expr)))
| Update(ObjectVariableUpdate(ova, updateop, expr)) -> Update(ObjectVariableUpdate((convert_generic_ova generic_to_actual ova), updateop, (ce expr)))
| Update(ArrayAccessUpdate((expr1, expr2), updateop, expr3)) -> Update(ArrayAccessUpdate(((ce expr1), (ce expr2)), updateop, (ce expr3)))
| IntLiteral(_)
| LongLiteral(_)
| FloatLiteral(_)
| CharLiteral(_)
| StringLiteral(_)
| BoolLiteral(_)
| Id(_)
| Self
| NullExpr as self -> self
and
convert_generic_elif generic_to_actual elif =
  let expr1 = (fst elif) in
  let stmts = (snd elif) in
  ((convert_generic_expr generic_to_actual expr1), (List.map (convert_generic_stmt generic_to_actual) stmts))
and
convert_generic_stmt generic_to_actual =
  let ce expr = convert_generic_expr generic_to_actual expr in
  let css stmts = (List.map (convert_generic_stmt generic_to_actual) stmts) in
  function
  Expr(expr) -> Expr((convert_generic_expr generic_to_actual expr))
| Return(expr) -> Return((convert_generic_expr generic_to_actual expr))
| ReturnVoid -> ReturnVoid
| If(expr1, stmts1, elifs, stmts2) -> If((ce expr1), (css stmts1), (List.map (convert_generic_elif generic_to_actual) elifs), (css stmts2))
| Loop(expr1, expr2, stmts) -> Loop((ce expr1), (ce expr2), (css stmts))
and
convert_generic_fdecl generic_to_actual fdecl =
  { rtype = (convert_generic_typ generic_to_actual fdecl.rtype);
    fname = fdecl.fname;
    formals = (List.map (convert_generic_bind generic_to_actual) fdecl.formals);
    body = (List.map (convert_generic_stmt generic_to_actual) fdecl.body)
  }
and
convert_instantiation actual_class generic_class =
  let generic_to_actual = get_generic_to_actual actual_class generic_class in
  { cname = actual_class.cname; source_class_name = ""; generics = [];
    static_vars = (List.map (convert_generic_assign generic_to_actual) generic_class.static_vars);
    required_vars = (List.map (convert_generic_bind generic_to_actual) generic_class.required_vars);
    optional_vars = (List.map (convert_generic_assign generic_to_actual) generic_class.optional_vars);
    methods = (List.map (convert_generic_fdecl generic_to_actual) generic_class.methods)
  }
and
get_converted_generic_instantiations = function
  Classdecl(classdecl) as self ->
    if ((List.length classdecl.generics) > 0) && classdecl.source_class_name <> "" then
      if StringMap.mem classdecl.source_class_name generic_map then
        Classdecl((convert_instantiation classdecl (StringMap.find classdecl.source_class_name generic_map)))
      else raise (Failure("Attempted to initialize " ^ classdecl.cname ^ " using generic class " ^ classdecl.source_class_name ^ ", but no such generic class could be found"))
    else if (List.length classdecl.generics) = 0 then
      self
   else Stmt(Expr(NullExpr)) (* We want to ignore these classes. This NullExpr will be filtered below. *)
| _ as self -> self
in
let program = (List.filter (fun a -> a <> Stmt(Expr(NullExpr))) (List.map get_converted_generic_instantiations original_program))
in

(* Next, figure out the type signature map for all functions on all classes.
   This will build a Map<ClassName, Map<FunctionSignature, RtypeOfFunction>> *)
let class_signatures =
(* Adds all the user defined functions, but also the auto-gen constructor signatures *)
let add_void_func_signature map func_signature = SignatureMap.add func_signature (Primitive(Void)) map
in
let func_signatures_for_class classdecl =
  let original_map = (List.fold_left add_fdecl SignatureMap.empty classdecl.methods) in
  let with_constructors = List.fold_left add_void_func_signature original_map (get_autogenerated_constructor_signatures classdecl) in
  SignatureMap.add { fs_name = "to_string"; formal_types = []; } (Primitive(String)) with_constructors
in
let add_class_signature map = function
  Classdecl(classdecl) -> if StringMap.mem classdecl.cname map then raise (Failure(("Duplicate classes detected for " ^ classdecl.cname)))
                          else StringMap.add classdecl.cname (func_signatures_for_class classdecl) map
| _ -> map
in
List.fold_left add_class_signature StringMap.empty program
in
let check_class_exists class_name =
  if StringMap.mem class_name class_signatures then ()
  else raise (Failure("Class name " ^ class_name ^ " was never defined."))
in
let rec check_class_exists_nested = function (* if defining an object array, make sure class is defined *)
    Class(name) -> check_class_exists name
  | Array(typ)  -> check_class_exists_nested typ
  | _           -> ()
in
(* Next, figure out the types for all variables for classes.
   This will build a Map<ClassName, Map<VariableName, TypeOfVariable>> *)
let class_variable_types =
let add_class_variable map tuple = StringMap.add (fst tuple) (snd tuple) map in
let get_tuple_from_assign = function
  RegularAssign(typ, str, _) -> (str, typ)
in
let get_tuple_from_bind bind = (snd bind, fst bind) in
let add_class_variables map = function
  (* Duplicate class names, duplicate variable names within a class,
     and improper assigns/binds are checked in other parts of the code *)
  Classdecl(classdecl) -> StringMap.add classdecl.cname
    (List.fold_left add_class_variable StringMap.empty
      ((List.map get_tuple_from_assign classdecl.static_vars) @
       (List.map get_tuple_from_bind classdecl.required_vars) @
       (List.map get_tuple_from_assign classdecl.optional_vars))) map
| _ -> map
in
List.fold_left add_class_variables StringMap.empty program
in
let class_static_vars =
let add_class_variable map tuple = StringMap.add (fst tuple) (snd tuple) map in
let get_tuple_from_assign = function
  RegularAssign(typ, str, _) -> (str, typ)
in
let add_class_variables map = function
  (* Duplicate class names, duplicate variable names within a class,
     and improper assigns/binds are checked in other parts of the code *)
  Classdecl(classdecl) -> StringMap.add classdecl.cname
    (List.fold_left add_class_variable StringMap.empty
      (List.map get_tuple_from_assign classdecl.static_vars)) map
| _ -> map
in
List.fold_left add_class_variables StringMap.empty program
in

let rec check_fcall fname actuals v_symbol_tables =
  let checked_exprs = List.map (check_expr v_symbol_tables) actuals in
  let signature = { fs_name = fname; formal_types = List.map (fst) checked_exprs } in
  if fname = "println" && (List.length actuals) = 1 then
    (* Special convenience code to wrap all primitives to become a valid print call *)
    let wrapped_checked_exprs = match checked_exprs with
        [(Primitive(Int), _)] -> [wrap_to_string checked_exprs]
      | [(Primitive(Long), _)] -> [wrap_to_string checked_exprs]
      | [(Primitive(Float), _)] -> [wrap_to_string checked_exprs]
      | [(Primitive(Char), _)] -> [wrap_to_string checked_exprs]
      | [(Primitive(Bool), _)] -> [wrap_to_string checked_exprs]
      | [(NullType, _)] -> [wrap_to_string checked_exprs]
      | [(Class(_), _)] -> [wrap_to_string checked_exprs]
      | _ -> checked_exprs in
    let signature = { fs_name = fname; formal_types = List.map (fst) wrapped_checked_exprs } in
    if SignatureMap.mem signature function_signatures then ((SignatureMap.find signature function_signatures), SCall (SFuncCall(fname, wrapped_checked_exprs))) else raise (Failure("No matching signature found for function call " ^ fname))
  else
    let matching_signature = find_matching_signature signature function_signatures in
    let null_safe_checked_exprs = convert_nulls_in_checked_exprs checked_exprs matching_signature in
    ((SignatureMap.find matching_signature function_signatures), SCall (SFuncCall(fname, null_safe_checked_exprs)))
and
check_mcall_prechecked checked_expr fname checked_actuals =
  match ((fst checked_expr)) with
  Class(class_name) ->
    let class_function_signatures = if StringMap.mem class_name class_signatures
                                    then StringMap.find class_name class_signatures
                                    else raise (Failure(("Class name " ^ class_name ^ " not found ")))
    in
    let signature = { fs_name = fname; formal_types = List.map (fst) checked_actuals } in
    let matching_signature = find_matching_signature signature class_function_signatures in
    let null_safe_checked_exprs = convert_nulls_in_checked_exprs checked_actuals matching_signature in
    ((SignatureMap.find matching_signature class_function_signatures), SCall (SMethodCall(checked_expr, fname, null_safe_checked_exprs)))
  | _ -> raise (Failure(("Attempted to call method on something that was not a class")))
and
check_mcall expr fname actuals v_symbol_tables =
  let checked_expr = check_expr v_symbol_tables expr in
  let checked_exprs = List.map (check_expr v_symbol_tables) actuals in
  check_mcall_prechecked checked_expr fname checked_exprs
and

check_call v_symbol_tables = function
  FuncCall(fname, exprs) -> (check_fcall fname exprs v_symbol_tables)
| MethodCall(expr, fname, exprs) -> check_mcall expr fname exprs v_symbol_tables
and

check_object_variable_access v_symbol_tables obj_var_access =
  let expr = obj_var_access.ova_expr in
  let class_name = obj_var_access.ova_class_name in
  let var_name = obj_var_access.ova_var_name in
  let is_static_access = obj_var_access.ova_is_static in
  if is_static_access then
    if StringMap.mem class_name class_static_vars then
      let class_variable_map = StringMap.find class_name class_static_vars in
      if StringMap.mem (var_name) class_variable_map then
        let typ_of_access = StringMap.find (var_name) class_variable_map in
        let sova = { sova_sexpr = (NullType, SNullExpr); sova_class_name = class_name; sova_var_name = var_name; sova_is_static = is_static_access } in
        (typ_of_access, (SObjectVariableAccess sova))
      else raise (Failure("Could not find a static variable named " ^ (var_name) ^ " in class " ^ class_name))
    else raise (Failure("Could not find a definition for class name " ^ class_name))
  else
    let checked_expr = check_expr v_symbol_tables expr in
    let object_type = (fst checked_expr) in
    match object_type with
      Class(class_name) -> (* Then check that the variable being accessed on the class actually exists *)
        if StringMap.mem class_name class_variable_types then
          let class_variable_map = StringMap.find class_name class_variable_types in
          if StringMap.mem (var_name) class_variable_map then
            let typ_of_access = StringMap.find (var_name) class_variable_map in
            let sova = { sova_sexpr = checked_expr; sova_class_name = class_name; sova_var_name = var_name; sova_is_static = is_static_access } in
            (typ_of_access, (SObjectVariableAccess sova))
          else raise (Failure("Could not find a variable named " ^ (var_name) ^ " in class " ^ class_name))
        else raise (Failure("Could not find a definition for class name " ^ class_name))
    | _ -> raise (Failure("Attempted to access variable " ^ (var_name) ^ " on something that isn't an object."))
and

check_lhs_is_not_void = function
  Primitive(Void) -> raise (Failure("LHS of assignment cannot be void"))
| NullType -> raise (Failure("LHS of assignment cannot be null"))
| _ -> ()
and
(* Arrays are painful due to empty array literals and array literals containing NULL *)
check_array_assign lhs_name lhs_element_type this_scopes_v_table checked_expr =
  (* *_type is always like Array(Int). *_element_type is the type of the element
     making up the array, e.g. Int *)
  (* if defining an object array, make sure class is defined *)
  let _ = check_class_exists_nested lhs_element_type in
  let lhs_type = Array(lhs_element_type) in
  let rhs_type = (fst checked_expr) in
  (* Size checks can only be applied to array literals and are used for the empty list case,
     because this is the only case where the type is unknown, other than a list containing
     only NULLs, which is only allowed for objects. *)
  let rhs_size = (match (snd checked_expr) with
    SArrayLiteral(sexprs) -> List.length sexprs
  | _ -> -1) in (* TODO this doesn't work with nested arrays *)
  match rhs_type with
    Array(rhs_element_type) ->
      if rhs_size = 0 then
        (* If the RHS is 0, then the types don't matter, since it can match any type *)
        ((StringHash.add this_scopes_v_table lhs_name lhs_type); (SRegularAssign(lhs_type, lhs_name, checked_expr)))
      else if (type_is_nullable lhs_element_type) && ((lhs_element_type = rhs_element_type) || (rhs_element_type = NullType)) then
        (* If the LHS is a nullable type, the array literal could be like [NULL, NULL, NULL] in which
           case its element type will be NullType, but it should still match an array like MyArray[] *)
        ((StringHash.add this_scopes_v_table lhs_name lhs_type); (SRegularAssign(lhs_type, lhs_name, checked_expr)))
      else if (rhs_type = lhs_type) then
        (* If we reached this point, the element type is not allowed to be null and it is non-zero. So the
           types need to directly match. *)
        ((StringHash.add this_scopes_v_table lhs_name lhs_type); (SRegularAssign(lhs_type, lhs_name, checked_expr)))
      else
        raise (Failure(("Illegal assignment. LHS was type " ^ (str_of_typ (Array(lhs_element_type))) ^ " but RHS type was " ^ (str_of_typ (fst checked_expr)))))
  | _ -> raise (Failure("RHS of array assignment must be an array."))
and
check_regular_assign lhs_type lhs_name rhs_expr v_symbol_tables =
  let this_scopes_v_table = (List.hd v_symbol_tables) in
  let _ = (check_lhs_is_not_void lhs_type) in
  (if StringHash.mem this_scopes_v_table lhs_name then
    let existing_type = StringHash.find this_scopes_v_table lhs_name in
    if existing_type <> lhs_type then raise (Failure(("Variable " ^ lhs_name ^ " has type " ^ (str_of_typ existing_type) ^ " but you attempted to assign it to type " ^ (str_of_typ lhs_type)))));

  let checked_expr = (check_expr v_symbol_tables rhs_expr) in
  let rhs_type = (fst checked_expr) in
  let lhs_rhs = { lhs = lhs_type; rhs = rhs_type } in
  let _ = (match lhs_type with Class(class_name) -> (check_class_exists class_name) | _ -> ()) in
  match lhs_type with
    Array(lhs_element_type) -> check_array_assign lhs_name lhs_element_type this_scopes_v_table checked_expr
  | _ ->
    if (rhs_type = lhs_type) || ((type_is_nullable lhs_type) && (rhs_type = NullType)) then
      let converted_null = (lhs_type, (snd checked_expr)) in
      ((StringHash.add this_scopes_v_table lhs_name lhs_type); (SRegularAssign(lhs_type, lhs_name, converted_null)))
    else if LhsRhsMap.mem lhs_rhs coerceable_types_map then
      let converter = LhsRhsMap.find lhs_rhs coerceable_types_map in
      ((StringHash.add this_scopes_v_table lhs_name lhs_type); (SRegularAssign(lhs_type, lhs_name, (converter [checked_expr]))))
    else
      raise (Failure(("Illegal assignment. LHS was type " ^ (str_of_typ lhs_type) ^ " but RHS type was " ^ (str_of_typ (fst checked_expr)))))
and

check_assign v_symbol_tables = function
  RegularAssign(lhs_type, lhs_name, rhs_expr) -> (check_regular_assign lhs_type lhs_name rhs_expr v_symbol_tables)
and

check_regular_update id updateop rhs_expr v_symbol_tables =
  let lhs_type = (type_of_identifier v_symbol_tables id) in
  let checked_expr = check_expr v_symbol_tables rhs_expr in
  let rhs_type = (fst checked_expr) in
  let lhs_rhs = { lhs = lhs_type; rhs = rhs_type } in
  match updateop with
    Eq ->
          if (lhs_type = rhs_type) || ((type_is_nullable lhs_type) && (rhs_type = NullType)) then
            let converted_null = (lhs_type, (snd checked_expr)) in
            (lhs_type, SUpdate (SRegularUpdate(id, updateop, converted_null)))
          else if LhsRhsMap.mem lhs_rhs coerceable_types_map then
            let converter = LhsRhsMap.find lhs_rhs coerceable_types_map in
            ((fst checked_expr), SUpdate (SRegularUpdate(id, updateop, (converter [checked_expr]))))
          else raise (Failure(("Illegal update. LHS was type " ^ (str_of_typ lhs_type) ^ " but RHS type was " ^ (str_of_typ (fst checked_expr)))))
and
get_sova = function
  SObjectVariableAccess(sova) -> sova
| _ -> raise (Failure("Found something other than an object variable access in an unexpected place."))
and
check_object_variable_update object_variable_access updateop rhs_expr v_symbol_tables =
  let checked_object_variable_access = check_object_variable_access v_symbol_tables object_variable_access in
  let sova = (get_sova (snd checked_object_variable_access)) in
  let lhs_type = (fst checked_object_variable_access) in
  let checked_expr = (check_expr v_symbol_tables rhs_expr) in
  let rhs_type = (fst checked_expr) in
  let lhs_rhs = { lhs = lhs_type; rhs = rhs_type } in
  match updateop with
    Eq ->
          if (lhs_type = rhs_type) || ((type_is_nullable lhs_type) && (rhs_type = NullType)) then
            let converted_null = (lhs_type, (snd checked_expr)) in
            (lhs_type, SUpdate (SObjectVariableUpdate(sova, updateop, converted_null)))
          else if LhsRhsMap.mem lhs_rhs coerceable_types_map then
            let converter = LhsRhsMap.find lhs_rhs coerceable_types_map in
            ((fst checked_expr), SUpdate (SObjectVariableUpdate(sova, updateop, (converter [checked_expr]))))
          else raise (Failure(("Illegal object variable update. LHS was type " ^ (str_of_typ lhs_type) ^ " but RHS type was " ^ (str_of_typ (fst checked_expr)))))
and
check_array_access_update array_access updateop rhs_expr v_symbol_tables =
  let checked_array_access = check_array_access (fst array_access) (snd array_access) v_symbol_tables in
  let sarray_access = match (snd checked_array_access) with SArrayAccess(tuple) -> tuple | _ -> raise (Failure("Found unexpected type while checking array access updates.")) in
  let lhs_type = (fst checked_array_access) in
  let checked_expr = (check_expr v_symbol_tables rhs_expr) in
  let rhs_type = (fst checked_expr) in
  let lhs_rhs = { lhs = lhs_type; rhs = rhs_type } in
  match updateop with
    Eq ->
          if (lhs_type = rhs_type) || ((type_is_nullable lhs_type) && (rhs_type = NullType)) then
            let converted_null = (lhs_type, (snd checked_expr)) in
            (lhs_type, SUpdate (SArrayAccessUpdate(sarray_access, updateop, converted_null)))
          else if LhsRhsMap.mem lhs_rhs coerceable_types_map then
            let converter = LhsRhsMap.find lhs_rhs coerceable_types_map in
            ((fst checked_expr), SUpdate (SArrayAccessUpdate(sarray_access, updateop, (converter [checked_expr]))))
          else raise (Failure(("Illegal array update. LHS was type " ^ (str_of_typ lhs_type) ^ " but RHS type was " ^ (str_of_typ (fst checked_expr)))))
and
check_update v_symbol_tables = function
  RegularUpdate(id, updateop, expr) -> check_regular_update id updateop expr v_symbol_tables
| ObjectVariableUpdate(object_variable_access, updateop, expr) -> check_object_variable_update object_variable_access updateop expr v_symbol_tables
| ArrayAccessUpdate(array_access, updateop, expr) -> check_array_access_update array_access updateop expr v_symbol_tables
and

check_array_access array_expr int_expr v_symbol_tables =
  (* Check the expr used to index into an array is an int.
     Check that the expr being indexed into is actually an array.
     Checking that the index is not out of bounds has to be done in codegen. *)
  let rhs_checked_expr = (check_expr v_symbol_tables int_expr) in
  let _ = (check_type_is_int (fst rhs_checked_expr)) in
  let lhs_checked_expr = (check_expr v_symbol_tables array_expr) in
  match (fst lhs_checked_expr) with
      Array(typ) -> (typ, SArrayAccess(lhs_checked_expr, rhs_checked_expr))
    | _ -> raise (Failure("Attempted to access something like it was an array, but it was not an array."))
and

check_exprs_have_same_type l = match l with
  [] -> ()
| [(_, _)] -> ()
| (NullType, _) :: (typ2, _) :: _ ->
    if (not (type_is_nullable typ2)) then
      raise (Failure ("Array literal had incompatible types " ^ (str_of_typ NullType) ^ " " ^ (str_of_typ typ2)))
    else check_exprs_have_same_type (List.tl l)
| (typ1, _) :: (NullType, _) :: _ ->
    if (not (type_is_nullable typ1)) then
      raise (Failure ("Array literal had incompatible types " ^ (str_of_typ typ1) ^ " " ^ (str_of_typ NullType)))
    else check_exprs_have_same_type (List.tl l)
| (typ1, _) :: (typ2, _) :: _ ->
    if typ1 <> typ2 then
      raise (Failure ("Array literal had incompatible types " ^ (str_of_typ typ1) ^ " " ^ (str_of_typ typ2)))
    else check_exprs_have_same_type (List.tl l)
and
check_array_literal exprs v_symbol_tables =
  if (List.length exprs) = 0 then (Array(NullType), (SArrayLiteral []))
  else
  let checked_exprs = List.map (check_expr v_symbol_tables) exprs in
  let _ = check_exprs_have_same_type checked_exprs in
  let non_null_list = List.filter (function p -> p <> NullType) (List.map fst checked_exprs) in
  let element_typ = if List.length non_null_list > 0 then (List.hd non_null_list) else NullType in
  let convert_null typ checked_expr = match (fst checked_expr) with
    NullType -> (typ, (snd checked_expr))
  | _ -> checked_expr in
  let null_safe_checked_exprs = List.map (convert_null element_typ) checked_exprs in
  let typ = Array(element_typ) in (typ, (SArrayLiteral null_safe_checked_exprs))
and

check_default_array typ exprs v_symbol_tables = match typ with
  Array(_) -> let _ = check_class_exists_nested typ in
              let checked_exprs = List.map (check_expr v_symbol_tables) exprs in
              let _ = List.iter (check_type_is_int) (List.map fst checked_exprs) in
              (typ, SDefaultArray(typ, checked_exprs))
| _        -> raise (Failure ("Attempted to create a default non-array type"))
and

check_unop unaryop expr v_symbol_tables =
  let checked_expr = (check_expr v_symbol_tables expr) in
  let typ = (fst checked_expr) in
  match unaryop with
    Not -> (match typ with
              Primitive(Bool) -> (typ, SUnop(unaryop, checked_expr))
            | _ -> raise (Failure("Attempted to call unary op not on something that wasn't a boolean")))
  | Neg -> (match typ with
              Primitive(Int) | Primitive(Long) | Primitive(Float) -> (typ, SUnop(unaryop, checked_expr))
            | _ -> raise (Failure("Attempted to call unary op - on something that wasn't a number")))
and

coerce_binop_exprs checked_lhs checked_rhs =
  let lhs_type = (fst checked_lhs) in
  let rhs_type = (fst checked_rhs) in
  if lhs_type = Primitive(Int) && rhs_type = Primitive(Long) then
    ((wrap_int_to_long [checked_lhs]), checked_rhs)
  else if lhs_type = Primitive(Long) && rhs_type = Primitive(Int) then
    (checked_lhs, (wrap_int_to_long [checked_rhs]))
  else if lhs_type = Primitive(Int) && rhs_type = Primitive(Float) then
    ((wrap_int_to_float [checked_lhs]), checked_rhs)
  else if lhs_type = Primitive(Float) && rhs_type = Primitive(Int) then
    (checked_lhs, (wrap_int_to_float [checked_rhs]))
  else if lhs_type = Primitive(String) then
    (checked_lhs, (wrap_to_string [checked_rhs]))
  else if rhs_type = Primitive(String) then
    ((wrap_to_string [checked_lhs]), checked_rhs)
  else
    raise (Failure("No coercion rule found for " ^ (str_of_typ lhs_type) ^ " and " ^ (str_of_typ rhs_type)))
and
check_binop_coerced checked_lhs binop checked_rhs =
  let lhs_type = (fst checked_lhs) in
  match binop with
    Plus -> (match lhs_type with
        Primitive(Int) | Primitive(Long) | Primitive(Float) | Primitive(String) ->
          (lhs_type, SBinop(checked_lhs, binop, checked_rhs))
      | Primitive(Char) -> (Primitive(String), SBinop((wrap_to_string [checked_lhs]), binop, (wrap_to_string [checked_rhs])))
      | _ -> raise (Failure("Binop + is not available for type " ^ (str_of_typ lhs_type))))

    | Subtract | Times | Divide | Modulo -> (match lhs_type with
        Primitive(Int) | Primitive(Long) | Primitive(Float) -> (lhs_type, SBinop(checked_lhs, binop, checked_rhs))
      | _ -> raise (Failure("Binops -, *, /, and % are not available for type " ^ (str_of_typ lhs_type))))

    | DoubleEq -> (Primitive(Bool), SBinop(checked_lhs, binop, checked_rhs))

    | BoGT | BoLT | BoGTE | BoLTE -> (match lhs_type with
        Primitive(Int) | Primitive(Long) | Primitive(Float) -> (Primitive(Bool), SBinop(checked_lhs, binop, checked_rhs))
      | _ -> raise (Failure("Operators > < >= <= are not available for type " ^ (str_of_typ lhs_type))))
    | BoOr | BoAnd -> (match lhs_type with
        Primitive(Bool) -> (Primitive(Bool), SBinop(checked_lhs, binop, checked_rhs))
      | _ -> raise (Failure("or and and must take boolean types, not " ^ (str_of_typ lhs_type))))
and
check_binop lhs binop rhs v_symbol_tables =
  let checked_lhs = (check_expr v_symbol_tables lhs) in
  let checked_rhs = (check_expr v_symbol_tables rhs) in
  let lhs_type = (fst checked_lhs) in
  let rhs_type = (fst checked_rhs) in
  (* NULLs are only valid in a ==. As usual, NULLs have to be treated very carefully. *)
  if ((lhs_type = NullType) || (rhs_type = NullType)) && binop = DoubleEq then
    (Primitive(Bool), SBinop(checked_lhs, binop, checked_rhs))
  else (match lhs_type with
     Class(_) -> (try check_mcall_prechecked checked_lhs (binop_method_name binop) [checked_rhs]
                  (* check if the user defined an operator override. if they did , we call it like a method *)
                  with _ -> if binop = DoubleEq then (Primitive(Bool), SBinop(checked_lhs, binop, checked_rhs))
                            else raise (Failure("Attempted to use a binop on an object that is not supported")))
  | _ -> (if lhs_type = rhs_type then
            check_binop_coerced checked_lhs binop checked_rhs
          else
            let lhs_rhs = coerce_binop_exprs checked_lhs checked_rhs in
            check_binop_coerced (fst lhs_rhs) binop (snd lhs_rhs))
  )
and

check_object_instantiation class_name exprs v_symbol_tables =
  let checked_exprs = List.map (check_expr v_symbol_tables) exprs in
  if StringMap.mem class_name class_signatures then
    (* See if there is a constructor matching this signature *)
    let signatures_in_class = StringMap.find class_name class_signatures in
    let signature = { fs_name = "construct"; formal_types = List.map fst checked_exprs } in
    try (let matching_signature = find_matching_signature signature signatures_in_class in
      let null_safe_checked_exprs = convert_nulls_in_checked_exprs checked_exprs matching_signature in
      (Class(class_name), SObjectInstantiation(class_name, null_safe_checked_exprs)))
    with Failure(_) -> (raise (Failure("Attempted to initialize class " ^ class_name ^ " using a type signature that has no associated constructor.")))
  else raise (Failure("Attempted to initialize class " ^ class_name ^ " that does not exist."))
and

check_expr v_symbol_tables = function
  IntLiteral(i) -> (Primitive(Int), SIntLiteral(i))
| LongLiteral(l) -> (Primitive(Long), SLongLiteral(l))
| FloatLiteral(f) -> (Primitive(Float), SFloatLiteral(f))
| CharLiteral(c) -> (Primitive(Char), SCharLiteral(c))
| StringLiteral(s) -> (Primitive(String), SStringLiteral(s))
| BoolLiteral(b) -> (Primitive(Bool), SBoolLiteral(b))
| Id(id_str) -> (type_of_identifier v_symbol_tables id_str, SId(id_str))
| NullExpr -> (NullType, SNullExpr)
| Self -> (type_of_identifier v_symbol_tables "self", SSelf)
| Call(call) -> check_call v_symbol_tables call
| ObjectInstantiation(class_name, exprs) -> check_object_instantiation class_name exprs v_symbol_tables
| ObjectVariableAccess(object_variable_access) -> check_object_variable_access v_symbol_tables object_variable_access
| ArrayAccess(array_name, expr) -> check_array_access array_name expr v_symbol_tables
| ArrayLiteral(exprs) -> check_array_literal exprs v_symbol_tables
| DefaultArray(typ, exprs) -> check_default_array typ exprs v_symbol_tables
| Binop(lhs_expr, binop, rhs_expr) -> check_binop lhs_expr binop rhs_expr v_symbol_tables
| Unop(unaryop, expr) -> check_unop unaryop expr v_symbol_tables
| Assign(assign) ->
    (let checked_assign = (check_assign v_symbol_tables assign) in
     match checked_assign with
       SRegularAssign(lhs_type, _, _) -> (lhs_type, SAssign(checked_assign))
     | _ -> raise (Failure("Found unexpected assign type")))
| Update(update) -> check_update v_symbol_tables update
and

check_elif v_symbol_tables expected_rtype elif =
  let checked_cond = (check_expr v_symbol_tables (fst elif)) in
  let _ = check_type_is_bool (fst checked_cond) in
  let checked_stmt_list = check_stmt_list ((StringHash.create 10)::v_symbol_tables) expected_rtype (snd elif) in
  (checked_cond, checked_stmt_list)
and
check_if if_cond_expr if_stmt_list elif_list else_stmt_list v_symbol_tables expected_rtype =
 (* check that all the conds are boolean *)
  let checked_if_cond = (check_expr v_symbol_tables if_cond_expr) in
  let _ = check_type_is_bool (fst checked_if_cond) in
  let checked_if_stmt_list = check_stmt_list ((StringHash.create 10)::v_symbol_tables) expected_rtype if_stmt_list in
  let checked_elifs = List.map (check_elif v_symbol_tables expected_rtype) elif_list in
  let checked_else_list = check_stmt_list ((StringHash.create 10)::v_symbol_tables) expected_rtype else_stmt_list in
  SIf(checked_if_cond, checked_if_stmt_list, checked_elifs, checked_else_list)
and

check_expr_is_update = function
  Update(_) -> ()
| NullExpr -> ()
| _ -> raise (Failure("Loop was expecting an update expr or a null expr"))
and
check_loop update_expr cond_expr stmt_list v_symbol_tables expected_rtype =
  let checked_cond = (check_expr v_symbol_tables cond_expr) in
  let _ = check_type_is_bool (fst checked_cond) in
  let _ = check_expr_is_update update_expr in
  let checked_update_expr = (check_expr v_symbol_tables update_expr) in
  let checked_stmt_list = check_stmt_list ((StringHash.create 10)::v_symbol_tables) expected_rtype stmt_list in
  SLoop(checked_update_expr, checked_cond, checked_stmt_list)
and

check_stmt v_symbol_tables expected_rtype = function
  Expr(expr) -> SExpr (check_expr v_symbol_tables expr)
| ReturnVoid -> (match expected_rtype with
                   Primitive(Void) -> SReturnVoid
                 | NullType -> raise (Failure("Found return statement in unexpected place (i.e. a statement outside of a function."))
                 | _ -> raise (Failure("Expected a void return but found a " ^ (str_of_typ expected_rtype) ^ " return"))
                )
| Return(expr) -> (match expected_rtype with
                     Primitive(Void) -> raise (Failure("Found a non-void return inside a function that required a " ^ (str_of_typ expected_rtype) ^ " return."))
                   | NullType -> raise (Failure("Found return statement in unexpected place (i.e. a statement outside of a function."))
                   | _ ->
                     let checked_expr = (check_expr v_symbol_tables expr) in
                     let actual_rtype = fst checked_expr in
                     if expected_rtype <> actual_rtype then raise (Failure(("Mismatch between expected and actual return type")))
                     else SReturn (checked_expr)
                   )
| If(if_cond_expr, if_stmt_list, elif_list, else_stmt_list) -> (check_if if_cond_expr if_stmt_list elif_list else_stmt_list v_symbol_tables expected_rtype)
| Loop(update_expr, cond_expr, stmt_list) -> check_loop update_expr cond_expr stmt_list v_symbol_tables expected_rtype
and
elif_always_returns elif =
  let stmt_list = (snd elif) in
  stmts_always_return stmt_list
and
all_are_true l = (* Check if all elements of a list are true *)
  not (List.mem false l)
and
stmt_always_returns = function
  Expr(_) -> false (* expressions are not returns *)
| ReturnVoid -> true
| Return(_) -> true (* Return statements always return *)
| If(_, if_stmt_list, elif_list, else_stmt_list) ->
    (* Checking if ifs always return is more complicated. *)
    if List.length else_stmt_list > 0 then
      (stmts_always_return if_stmt_list) && (all_are_true (List.map elif_always_returns elif_list)) && (stmts_always_return else_stmt_list)
    else false (* Technically, if there is an elseless if, it always returns if the if statement is always true
                  and the if_stmt_list always returns, or if there is an elseless if+elifs, it always returns if the
                  if+elifs are always true and their stmt lists always return. But this is too complicated to check,
                  since we may not know if an if will always return true, as the expr in an if can be generated at
                  runtime. *)
| Loop(_, _, _) -> false (* Similar to the above, a loop always returns if its body always returns, but that requires
                         knowing whether the loop is guaranteed to be entered, which relies on information that may
                         not be known at compile time. *)
and
(* Given a stmt_list, determine if it always returns.
   This is needed for the following semantic checks:
   -If a statement always returns, then any code after it is unreachable.
   -If a function has a non-void return, then its statement list must always return. *)
stmts_always_return = function
  [] -> false
| hd::tl -> let first_statement_returns = (stmt_always_returns hd) in
            if first_statement_returns && (List.length tl > 0) then
              raise (Failure("Found unreachable code."))
            else if first_statement_returns && (List.length tl == 0) then
              true
            else (* First statement did not always return, so check if the next ones did. *)
              stmts_always_return tl
and
check_stmt_list v_symbol_tables expected_rtype = function
  [ReturnVoid as s] -> [check_stmt v_symbol_tables expected_rtype s]
| [Return(_) as s] -> [check_stmt v_symbol_tables expected_rtype s]
| ReturnVoid :: _ -> raise (Failure("Nothing can follow a return statement"))
| Return(_) :: _ -> raise (Failure("Nothing can follow a return statement"))
| s :: stmt_list ->
    (* This let is neeeded to ensure the first statement is evaluated
       before the rest of the statements in the list. Our language is
       sequential from top to bottom but by default OCaml has :: as
       right associative *)
    let fst_stmt = check_stmt v_symbol_tables expected_rtype s in
    fst_stmt :: check_stmt_list v_symbol_tables expected_rtype stmt_list
| [] -> []
and

check_binds (kind : string) (binds : bind list) = (* check_binds was stolen from microc *)
  List.iter (function
      (Primitive(Void), b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
    | (NullType, b) -> raise (Failure ("illegal null " ^ kind ^ " " ^ b))
    | (Class(class_name), _) -> check_class_exists class_name
    | _ -> ()) binds;
  dups kind (List.sort (fun (a) (b) -> compare a b) (List.map (snd) binds))
and
check_fdecl is_in_class v_symbol_tables fdecl = (ignore (check_binds ("fdecl " ^ fdecl.fname) fdecl.formals));
  (let all_branches_return = stmts_always_return fdecl.body in
   if fdecl.rtype <> Primitive(Void) && (not all_branches_return) then
     (* If the function return type is not void, it must return in every branch *)
     raise (Failure("Function " ^ fdecl.fname ^ " had a non-void return but had branches that never returned."))
   (* Check that return type of constructor is always void *)
   else if (is_in_class && fdecl.fname = "construct" && fdecl.rtype <> Primitive(Void)) then
     raise (Failure("Found constructor that had rtype " ^ (str_of_typ fdecl.rtype) ^ " rather than void."))
   else if (is_in_class && fdecl.fname = "to_string" && ((List.length fdecl.formals) = 0) && fdecl.rtype <> Primitive(String)) then
     raise (Failure("Found to_string that had rtype " ^ (str_of_typ fdecl.rtype) ^ " rather than string."))
   else
     { srtype = fdecl.rtype; sfname = fdecl.fname; sformals = fdecl.formals; sbody = check_stmt_list ((get_hash_of_binds fdecl.formals)::v_symbol_tables) fdecl.rtype fdecl.body })
and

get_names_from_assign = function
  RegularAssign(_, name, _) -> name
and
get_all_class_variable_names classdecl = (List.map get_names_from_assign classdecl.static_vars) @ (List.map (snd) classdecl.required_vars) @ (List.map get_names_from_assign classdecl.optional_vars)
and
get_sstmt_from_bind cname bind =
  let typ = (fst bind) in
  let name = (snd bind) in
  let sova = { sova_sexpr = (Class(cname), SSelf); sova_class_name = ""; sova_var_name = name; sova_is_static = false } in
  SExpr (typ, SUpdate(SObjectVariableUpdate(sova, Eq, (typ, SId(name)))))
and
get_sstmt_from_assign_with_default v_symbol_tables cname = function
  RegularAssign(typ, name, expr) ->
    let sova = { sova_sexpr = (Class(cname), SSelf); sova_class_name = ""; sova_var_name = name; sova_is_static = false } in
    let checked_expr = (check_expr v_symbol_tables expr) in
    let lhs_type = typ in
    let converted_null = (lhs_type, (snd checked_expr)) in
    SExpr (typ, SUpdate(SObjectVariableUpdate(sova, Eq, converted_null)))
and
get_required_only_constructor v_symbol_tables classdecl =
  let body = (List.map (get_sstmt_from_bind classdecl.cname) classdecl.required_vars) @ (List.map (get_sstmt_from_assign_with_default v_symbol_tables classdecl.cname) classdecl.optional_vars) in
  { srtype = Primitive(Void); sfname = "construct"; sformals = classdecl.required_vars; sbody = body }
and
get_required_and_optional_constructor classdecl =
  let optional_var_binds = (List.map get_bind_from_assign classdecl.optional_vars) in
  let body = (List.map (get_sstmt_from_bind classdecl.cname) classdecl.required_vars) @ (List.map (get_sstmt_from_bind classdecl.cname) optional_var_binds) in
  { srtype = Primitive(Void); sfname = "construct"; sformals = (classdecl.required_vars @ optional_var_binds); sbody = body }
and
get_autogenerated_constructors v_symbol_tables classdecl =
  if List.length classdecl.required_vars = 0 && List.length classdecl.optional_vars = 0 then [ { srtype = Primitive(Void); sfname = "construct"; sformals = []; sbody = [] } ]
  else if List.length classdecl.required_vars > 0 && List.length classdecl.optional_vars = 0 then [get_required_only_constructor v_symbol_tables classdecl]
  else [(get_required_only_constructor v_symbol_tables classdecl); (get_required_and_optional_constructor classdecl)]
and
signature_is_not_yet_defined map sfdecl =
  let signature = (sget_signature sfdecl) in
  not (SignatureMap.mem signature map)
and
add_autogenerated_constructors v_symbol_tables classdecl checked_fdecls =
  (* This will add the autogenerated constructors to the checked classdecl.
     However, if there is already a constructor defined with the same signature, we don't
     throw an error or overwrite it here. The user defined constructor takes precedence over
     the auto-generated one. *)
  let autogenerated_constructors = get_autogenerated_constructors v_symbol_tables classdecl in
  let original_func_signatures = (List.fold_left add_fdecl SignatureMap.empty classdecl.methods) in
  checked_fdecls @ (List.filter (signature_is_not_yet_defined original_func_signatures) autogenerated_constructors)
and
get_default_to_string classdecl =
  { srtype = Primitive(String); sfname = "to_string"; sformals = []; sbody = [SReturn(Primitive(String), SStringLiteral("User did not define to_string method for class " ^ classdecl.cname))] }
and
add_to_string_if_not_present classdecl checked_fdecls =
  let original_func_signatures = (List.fold_left add_fdecl SignatureMap.empty classdecl.methods) in
  checked_fdecls @ (List.filter (signature_is_not_yet_defined original_func_signatures) [get_default_to_string classdecl])
and
convert_to_static_assign class_name = function
  SRegularAssign(lhs_typ, lhs_name, rhs_expr) -> SStaticAssign(class_name, lhs_typ, lhs_name, rhs_expr)
| _ -> raise (Failure("Found unexpected assignment type in class " ^ class_name))
and
check_classdecl v_symbol_tables classdecl =
  (* First check for duplicates in the class variables *)
  (dups ("classdecl " ^ classdecl.cname) (List.sort (fun (a) (b) -> compare a b) (get_all_class_variable_names classdecl)));
  (* Then check all the assigns and bindings are correct *)
  let new_v_symbol_tables = (StringHash.create 10)::v_symbol_tables in (* class gets a new level of symbols *)
  let checked_static_vars = List.map (convert_to_static_assign classdecl.cname) (List.map (check_assign new_v_symbol_tables) classdecl.static_vars) in
  let checked_required_vars = (ignore (check_binds ("classdecl " ^ classdecl.cname) classdecl.required_vars)); classdecl.required_vars in
  let checked_optional_vars = List.map (check_assign new_v_symbol_tables) classdecl.optional_vars in
  (* Inside this class, make sure "self" by itself points to this class type *)
  let _ = StringHash.add (List.hd new_v_symbol_tables) "self" (Class(classdecl.cname)) in
  let checked_fdecls = List.map (check_fdecl true new_v_symbol_tables) classdecl.methods in
  { scname = classdecl.cname; sstatic_vars = checked_static_vars; srequired_vars = checked_required_vars; soptional_vars = checked_optional_vars; smethods = (add_to_string_if_not_present classdecl (add_autogenerated_constructors v_symbol_tables classdecl checked_fdecls)) }
and

check_p_unit v_symbol_tables = function
  Stmt(stmt) -> SStmt (check_stmt v_symbol_tables (NullType) stmt)
| Fdecl(fdecl) -> SFdecl (check_fdecl false v_symbol_tables fdecl)
| Classdecl(classdecl) -> SClassdecl (check_classdecl v_symbol_tables classdecl)
in

List.map (check_p_unit [StringHash.create 10]) program

