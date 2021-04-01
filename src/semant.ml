(* Semantic checking for the Boomslang compiler *)

open Ast
open Sast

type function_signature = {
  fs_name: string;
  formal_types: typ list;
}

module StringMap = Map.Make(String);;
module SignatureMap = Map.Make(struct type t = function_signature let compare = compare end);;
module StringHash = Hashtbl.Make(struct
  type t = string (* type of keys *)
  let equal x y = x = y (* use structural comparison *)
  let hash = Hashtbl.hash (* generic hash function *)
end);;

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong. *)

(* Add built-in functions *)
let check program =

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

let built_in_funcs = [ ({ fs_name = "println"; formal_types = [Primitive(String)] }, Primitive(Void)) ]
in
let add_built_in map built_in = SignatureMap.add (fst built_in) (snd built_in) map
in
let built_in_func_map = List.fold_left add_built_in SignatureMap.empty built_in_funcs
in

(* First, figure out all the defined functions *)
let get_signature fdecl = { fs_name = fdecl.fname; formal_types = List.map fst fdecl.formals }
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
let function_signatures = List.fold_left add_signature built_in_func_map program
in

(* Next, figure out the type signature map for all functions on all classes.
   This will build a Map<ClassName, Map<FunctionSignature, RtypeOfFunction>> *)
let class_signatures =
let add_class_signature map = function
  Classdecl(classdecl) -> if StringMap.mem classdecl.cname map then raise (Failure(("Duplicate classes detected for " ^ classdecl.cname)))
                          else StringMap.add classdecl.cname (List.fold_left add_fdecl SignatureMap.empty classdecl.methods) map
| _ -> map
in
List.fold_left add_class_signature StringMap.empty program
in
(* Next, figure out the types for all variables for classes.
   This will build a Map<ClassName, Map<VariableName, TypeOfVariable>> *)
let class_variable_types =
let add_class_variable map tuple = StringMap.add (fst tuple) (snd tuple) map in
let get_tuple_from_assign = function
  RegularAssign(typ, str, _) -> (str, typ)
| _ -> raise (Failure("Unexpected assign encountered"))
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

let rec check_fcall fname actuals v_symbol_tables =
  let checked_exprs = List.map (check_expr v_symbol_tables) actuals in
  let signature = { fs_name = fname; formal_types = List.map (fst) checked_exprs } in
  if SignatureMap.mem signature function_signatures then ((SignatureMap.find signature function_signatures), SCall (SFuncCall(fname, checked_exprs))) else raise (Failure("No matching signature found for function call"))
and

check_mcall object_name fname actuals v_symbol_tables =
  match (type_of_identifier v_symbol_tables object_name) with
  Class(class_name) ->
    let class_function_signatures = if StringMap.mem class_name class_signatures
                                    then StringMap.find class_name class_signatures
                                    else raise (Failure(("Class name " ^ class_name ^ " not found ")))
    in
    let checked_exprs = List.map (check_expr v_symbol_tables) actuals in
    let signature = { fs_name = fname; formal_types = List.map (fst) checked_exprs } in
    if SignatureMap.mem signature class_function_signatures
    then ((SignatureMap.find signature class_function_signatures), SCall (SMethodCall(object_name, fname, checked_exprs)))
    else raise (Failure("No matching signature found for method call"))
  | _ -> raise (Failure(("Attempted to call method on " ^ object_name ^ " but " ^ object_name ^ " was not a class")))
and

check_call v_symbol_tables = function
  FuncCall(fname, exprs) -> (check_fcall fname exprs v_symbol_tables)
| MethodCall(object_name, fname, exprs) -> check_mcall object_name fname exprs v_symbol_tables
and

check_object_variable_access v_symbol_tables object_variable_access =
  (* First get the type of the object name. If it is not a class type, that is an error. *)
  let object_type = (type_of_identifier v_symbol_tables (fst object_variable_access)) in
  match object_type with
    Class(class_name) -> (* Then check that the variable being accessed on the class actually exists *)
      if StringMap.mem class_name class_variable_types then
        let class_variable_map = StringMap.find class_name class_variable_types in
        if StringMap.mem (snd object_variable_access) class_variable_map then
          let typ_of_access = StringMap.find (snd object_variable_access) class_variable_map in
          (typ_of_access, (SObjectVariableAccess object_variable_access))
        else raise (Failure("Could not find a variable named " ^ (snd object_variable_access) ^ " in class " ^ class_name))
      else raise (Failure("Could not find a definition for class name " ^ class_name ^ " which corresponded to variable " ^ (fst object_variable_access)))
  | _ -> raise (Failure("Attempted to access variable " ^ (snd object_variable_access) ^ " on " ^ (fst object_variable_access) ^ ", which isn't an object."))
and

check_lhs_is_not_void = function
  Primitive(Void) -> raise (Failure("LHS of assignment cannot be void"))
| NullType -> raise (Failure("LHS of assignment cannot be null"))
| _ -> ()
and
check_regular_assign lhs_type lhs_name rhs_expr v_symbol_tables =
  let this_scopes_v_table = (List.hd v_symbol_tables) in
  let _ = (check_lhs_is_not_void lhs_type) in
  (if StringHash.mem this_scopes_v_table lhs_name then
    let existing_type = StringHash.find this_scopes_v_table lhs_name in
    if existing_type <> lhs_type then raise (Failure(("Variable " ^ lhs_name ^ " has type " ^ (str_of_typ existing_type) ^ " but you attempted to assign it to type " ^ (str_of_typ lhs_type)))));

  let checked_expr = (check_expr v_symbol_tables rhs_expr) in
  let rhs_type = (fst checked_expr) in
  if rhs_type = lhs_type then
    ((StringHash.add this_scopes_v_table lhs_name lhs_type); (SRegularAssign(lhs_type, lhs_name, checked_expr)))
  else
    raise (Failure(("Illegal assignment. LHS was type " ^ (str_of_typ lhs_type) ^ " but RHS type was " ^ (str_of_typ (fst checked_expr)))))
and
check_object_variable_assign object_variable_access rhs_expr v_symbol_tables =
  let checked_object_variable_access = check_object_variable_access v_symbol_tables object_variable_access in
  let lhs_type = (fst checked_object_variable_access) in
  let checked_expr = (check_expr v_symbol_tables rhs_expr) in
  let rhs_type = (fst checked_expr) in
  if rhs_type = lhs_type then SObjectVariableAssign(object_variable_access, checked_expr)
  else raise (Failure(("Illegal assignment. LHS was type " ^ (str_of_typ lhs_type) ^ " but RHS type was " ^ (str_of_typ (fst checked_expr)))))
and

check_assign v_symbol_tables = function
  RegularAssign(lhs_type, lhs_name, rhs_expr) -> (check_regular_assign lhs_type lhs_name rhs_expr v_symbol_tables)
| ObjectVariableAssign(object_variable_access, rhs_expr) -> (check_object_variable_assign object_variable_access rhs_expr v_symbol_tables)
and

check_regular_update id updateop rhs_expr v_symbol_tables =
  let lhs_type = (type_of_identifier v_symbol_tables id) in
  let checked_expr = check_expr v_symbol_tables rhs_expr in
  let rhs_type = (fst checked_expr) in
  match updateop with
    Eq | PlusEq | MinusEq | TimesEq | DivideEq ->
          if lhs_type = rhs_type then ((fst checked_expr), SUpdate (SRegularUpdate(id, updateop, checked_expr)))
          else raise (Failure(("Illegal update. LHS was type " ^ (str_of_typ lhs_type) ^ " but RHS type was " ^ (str_of_typ (fst checked_expr)))))
and

check_object_variable_update object_variable_access updateop rhs_expr v_symbol_tables =
  let checked_object_variable_access = check_object_variable_access v_symbol_tables object_variable_access in
  let lhs_type = (fst checked_object_variable_access) in
  let checked_expr = (check_expr v_symbol_tables rhs_expr) in
  let rhs_type = (fst checked_expr) in
  match updateop with
    Eq | PlusEq | MinusEq | TimesEq | DivideEq ->
          if lhs_type = rhs_type then ((fst checked_expr), SUpdate (SObjectVariableUpdate(object_variable_access, updateop, checked_expr)))
          else raise (Failure(("Illegal update. LHS was type " ^ (str_of_typ lhs_type) ^ " but RHS type was " ^ (str_of_typ (fst checked_expr)))))
and

check_update v_symbol_tables = function
  RegularUpdate(id, updateop, expr) -> check_regular_update id updateop expr v_symbol_tables
| ObjectVariableUpdate(object_variable_access, updateop, expr) -> check_object_variable_update object_variable_access updateop expr v_symbol_tables
and

check_array_access array_name expr v_symbol_tables =
  (* Check the expr used to index into an array is an int.
     Check that the array name is actually referring to an array.
     Checking that the index is not out of bounds has to be done in codegen. *)
  let checked_expr = (check_expr v_symbol_tables expr) in
  let _ = (check_type_is_int (fst checked_expr)) in
  let name_type = (type_of_identifier v_symbol_tables array_name) in
  match name_type with
      Array(typ, _) -> (typ, SArrayAccess(array_name, checked_expr))
    | _ -> raise (Failure("Attempted to access " ^ array_name ^ " like it was an array, but it was not an array."))
and

check_exprs_have_same_type = function
  [] -> ()
| [(_, _)] -> ()
| ((typ1, _) :: (typ2, _) :: _) when typ1 <> typ2 ->
       raise (Failure ("Array literal had incompatible types " ^ (str_of_typ typ1) ^ " " ^ (str_of_typ typ2)))
| _ -> ()
and
check_array_literal exprs v_symbol_tables =
  if (List.length exprs) = 0 then (NullType, (SArrayLiteral []))
  else
  let checked_exprs = List.map (check_expr v_symbol_tables) exprs in
  let _ = check_exprs_have_same_type checked_exprs in
  let typ = Array((fst (List.hd checked_exprs)), (List.length checked_exprs)) in (typ, (SArrayLiteral checked_exprs))
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

check_binop lhs binop rhs v_symbol_tables =
  match binop with
  Plus | Subtract | Times | Divide | Modulo ->
  let checked_lhs = (check_expr v_symbol_tables lhs) in
  let checked_rhs = (check_expr v_symbol_tables rhs) in
  ((fst checked_lhs), SBinop(checked_lhs, binop, checked_rhs))
| DoubleEq | NotEq | BoGT | BoLT | BoGTE | BoLTE | BoOr | BoAnd ->
  let checked_lhs = (check_expr v_symbol_tables lhs) in
  let checked_rhs = (check_expr v_symbol_tables rhs) in
  (Primitive(Bool), SBinop(checked_lhs, binop, checked_rhs))
and

check_object_instantiation class_name exprs v_symbol_tables =
  (* TODO check the exprs match one of its constuctor signatures *)
  let checked_exprs = List.map (check_expr v_symbol_tables) exprs in
  if StringMap.mem class_name class_signatures then (Class(class_name), SObjectInstantiation(class_name, checked_exprs))
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
| Call(call) -> check_call v_symbol_tables call
| ObjectInstantiation(class_name, exprs) -> check_object_instantiation class_name exprs v_symbol_tables
| ObjectVariableAccess(object_variable_access) -> check_object_variable_access v_symbol_tables object_variable_access
| ArrayAccess(array_name, expr) -> check_array_access array_name expr v_symbol_tables
| ArrayLiteral(exprs) -> check_array_literal exprs v_symbol_tables
| Binop(lhs_expr, binop, rhs_expr) -> check_binop lhs_expr binop rhs_expr v_symbol_tables
| Unop(unaryop, expr) -> check_unop unaryop expr v_symbol_tables
| Assign(assign) ->
    (let checked_assign = (check_assign v_symbol_tables assign) in
     match checked_assign with
       SRegularAssign(lhs_type, _, _) -> (lhs_type, SAssign(checked_assign))
     | SObjectVariableAssign(_, checked_expr) -> ((fst checked_expr), SAssign(checked_assign)))
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
  let checked_update_expr = (check_expr v_symbol_tables cond_expr) in
  let checked_stmt_list = check_stmt_list ((StringHash.create 10)::v_symbol_tables) expected_rtype stmt_list in
  SLoop(checked_cond, checked_update_expr, checked_stmt_list)
and

check_stmt v_symbol_tables expected_rtype = function
  Expr(expr) -> SExpr (check_expr v_symbol_tables expr)
| ReturnVoid -> (match expected_rtype with
                   Primitive(Void) -> SReturnVoid
                 | NullType -> raise (Failure("Found return statement in unexpected place (i.e. a statement outside of a function."))
                 | _ -> raise (Failure("Expected a void return but found a " ^ (str_of_typ expected_rtype) ^ " return"))
                )
| Return(expr) -> (match expected_rtype with
                     Primitive(Void) -> raise (Failure("Found a void return inside a function that required a " ^ (str_of_typ expected_rtype) ^ " return."))
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
check_stmt_list v_symbol_tables expected_rtype = function
  [ReturnVoid as s] -> [check_stmt v_symbol_tables expected_rtype s]
| [Return(_) as s] -> [check_stmt v_symbol_tables expected_rtype s]
| ReturnVoid :: _ -> raise (Failure("Nothing can follow a return statement"))
| Return(_) :: _ -> raise (Failure("Nothing can follow a return statement"))
| s :: stmt_list -> check_stmt v_symbol_tables expected_rtype s :: check_stmt_list v_symbol_tables expected_rtype stmt_list
| [] -> []
and

check_binds (kind : string) (binds : bind list) = (* check_binds was stolen from microc *)
  List.iter (function
      (Primitive(Void), b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
    | (NullType, b) -> raise (Failure ("illegal null " ^ kind ^ " " ^ b))
    | _ -> ()) binds;
  dups kind (List.sort (fun (a) (b) -> compare a b) (List.map (snd) binds))
and
check_fdecl v_symbol_tables fdecl = (ignore (check_binds ("fdecl " ^ fdecl.fname) fdecl.formals)); { srtype = fdecl.rtype; sfname = fdecl.fname; sformals = fdecl.formals; sbody = check_stmt_list ((get_hash_of_binds fdecl.formals)::v_symbol_tables) fdecl.rtype fdecl.body }
and

get_names_from_assign = function
  RegularAssign(_, name, _) -> name
| _ -> raise (Failure("Found non-regular assign inside class variable initializations"))
and
get_all_class_variable_names classdecl = (List.map get_names_from_assign classdecl.static_vars) @ (List.map (snd) classdecl.required_vars) @ (List.map get_names_from_assign classdecl.optional_vars)
and
check_classdecl v_symbol_tables classdecl =
  (* First check for duplicates in the class variables *)
  (dups ("classdecl " ^ classdecl.cname) (List.sort (fun (a) (b) -> compare a b) (get_all_class_variable_names classdecl)));
  (* Then check all the assigns and bindings are correct *)
  let new_v_symbol_tables = (StringHash.create 10)::v_symbol_tables in (* class gets a new level of symbols *)
  let checked_static_vars = List.map (check_assign new_v_symbol_tables) classdecl.static_vars in
  let checked_required_vars = (ignore (check_binds ("classdecl " ^ classdecl.cname) classdecl.required_vars)); classdecl.required_vars in
  let checked_optional_vars = List.map (check_assign new_v_symbol_tables) classdecl.optional_vars in
  let checked_fdecls = List.map (check_fdecl new_v_symbol_tables) classdecl.methods in
  { scname = classdecl.cname; sstatic_vars = checked_static_vars; srequired_vars = checked_required_vars; soptional_vars = checked_optional_vars; smethods = checked_fdecls }
and

check_p_unit v_symbol_tables = function
  Stmt(stmt) -> SStmt (check_stmt v_symbol_tables (NullType) stmt)
| Fdecl(fdecl) -> SFdecl (check_fdecl v_symbol_tables fdecl)
| Classdecl(classdecl) -> SClassdecl (check_classdecl v_symbol_tables classdecl)
in

List.map (check_p_unit [StringHash.create 10]) program

