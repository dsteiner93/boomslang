(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
  SIntLiteral of int
| SLongLiteral of int64
| SFloatLiteral of string
| SCharLiteral of char
| SStringLiteral of string
| SBoolLiteral of bool
| SId of string
| SSelf
| SNullExpr
| SCall of scall
| SObjectInstantiation of string * sexpr list
| SObjectVariableAccess of object_variable_access
| SArrayAccess of sarray_access
| SArrayLiteral of sexpr list
| SBinop of sexpr * binop * sexpr
| SUnop of unaryop * sexpr
| SAssign of sassign
| SUpdate of supdate
and sarray_access = string * sexpr
and scall =
  SFuncCall of string * sexpr list
| SMethodCall of string * string * sexpr list
and sassign =
  SRegularAssign of typ * string * sexpr
and supdate =
  SRegularUpdate of string * updateop * sexpr
| SObjectVariableUpdate of object_variable_access * updateop * sexpr
| SArrayAccessUpdate of sarray_access * updateop * sexpr

type sstmt =
  SExpr of sexpr
| SReturn of sexpr
| SReturnVoid
| SIf of sexpr * sstmt list * selif list * sstmt list
| SLoop of sexpr * sexpr * sstmt list
and selif = sexpr * sstmt list

type sfdecl = {
  srtype: typ;
  sfname: string;
  sformals: bind list;
  sbody: sstmt list;
}

type sclassdecl = {
  scname: string;
  sstatic_vars: sassign list;
  srequired_vars: bind list;
  soptional_vars: sassign list;
  smethods: sfdecl list;
}

type sp_unit =
  SStmt of sstmt
| SFdecl of sfdecl
| SClassdecl of sclassdecl

type sprogram = sp_unit list

type function_signature = {
  fs_name: string;
  formal_types: typ list;
}

(* Begin visualization functions *)
let rec str_of_typ = function
  Primitive(Int) -> "int"
| Primitive(Long) -> "long"
| Primitive(Float) -> "float"
| Primitive(Char) -> "char"
| Primitive(String) -> "string"
| Primitive(Bool) -> "boolean"
| Primitive(Void) -> "void"
| Class(str) -> "Class(" ^ str ^ ")"
| Array(typ, size) -> (str_of_typ typ) ^ "[" ^ (string_of_int size) ^ "]"
| NullType -> "NULL"

let get_label_with_type suffixed_name unsuffixed_name typ = suffixed_name ^ " [label=\"" ^ unsuffixed_name ^ " (" ^ (str_of_typ typ) ^ ")\"]"

let get_multi_node_generator_typ node_name suffix subconverter input_list typ =
  let start_node = node_name ^ suffix in
  let subgraphs = (List.mapi (subconverter suffix) input_list) in (* List<Tuple<StartNodeString, List<String>> *)
  (start_node, (get_label_with_type start_node node_name typ)::List.concat (List.map (get_combine_function start_node) subgraphs))

let combine_list_typ node_name suffix input_list typ =
  let start_node = node_name ^ suffix in
  (start_node, (get_label_with_type start_node node_name typ)::List.concat (List.map (get_combine_function start_node) input_list))

let string_of_id_typ existing_suffix new_index id_string typ =
  let suffix = new_suffix existing_suffix new_index in
  ("id" ^ suffix, ["id" ^ suffix ^ " [label=\"id: " ^ id_string ^ " (" ^ (str_of_typ typ) ^ ")\" fontcolor=red]"])

let string_of_object_variable_access existing_suffix new_index object_variable_access =
  let suffix = new_suffix existing_suffix new_index in
  ("obj_var_access" ^ suffix, ["obj_var_access" ^ suffix ^ " [label=\"" ^ (fst object_variable_access) ^ "." ^ (snd object_variable_access) ^"\"]"])

let rec string_of_sexpr existing_suffix new_index sexpr =
  let suffix = new_suffix existing_suffix new_index in
  let typ = (fst sexpr) in
  let sx = (snd sexpr) in
  match sx with
  SIntLiteral(integer) -> ("intlit" ^ suffix, [get_literal_node "intlit" suffix (string_of_int integer)])
| SLongLiteral(long) -> ("longlit" ^ suffix, [get_literal_node "longlit" suffix (Int64.to_string long)])
| SFloatLiteral(f) -> ("floatlit" ^ suffix, [get_literal_node "floatlit" suffix f])
| SCharLiteral(c) -> ("charlit" ^ suffix, [get_literal_node "charlit" suffix (String.make 1 c)])
| SStringLiteral(s) -> ("stringlit" ^ suffix, [get_literal_node "stringlit" suffix s])
| SBoolLiteral(b) -> ("boollit" ^ suffix, [get_literal_node "boollit" suffix (string_of_bool b)])
| SId(id_string) -> string_of_id_typ suffix 0 id_string typ
| SNullExpr -> ("nullexpr" ^ suffix, [get_literal_node "nullexpr" suffix "NULL"])
| SSelf -> string_of_id_typ suffix 0 "self" typ
| SCall(scall) -> string_of_scall typ suffix 0 scall
| SObjectInstantiation(id_string, sexprs) -> combine_list_typ "object_instantiation" suffix ([string_of_id_typ suffix 0 id_string typ] @ (mapiplus 1 (string_of_sexpr suffix) sexprs)) typ
| SObjectVariableAccess(object_variable_access) -> string_of_object_variable_access suffix 0 object_variable_access
| SArrayAccess(sarray_access) -> string_of_sarray_access suffix 0 sarray_access typ
| SArrayLiteral(sexprs) -> get_multi_node_generator_typ "array_literal" suffix string_of_sexpr sexprs typ
| SBinop(sexpr1, binop, sexpr2) -> combine_list_typ "binop" suffix ([string_of_sexpr suffix 0 sexpr1] @ [string_of_binoperator suffix 1 binop] @ [string_of_sexpr suffix 2 sexpr2]) typ
| SUnop(unaryop, sexpr) -> combine_list_typ "unaryop" suffix ([string_of_unaryop suffix 0 unaryop] @ [string_of_sexpr suffix 1 sexpr]) typ
| SAssign(sassign) -> string_of_sassign suffix 0 sassign
| SUpdate(supdate) -> string_of_supdate typ suffix 0 supdate
and string_of_sarray_access existing_suffix new_index sarray_access typ =
  let suffix = new_suffix existing_suffix new_index in
  let id_string = (fst sarray_access) in
  let sexpr = (snd sarray_access) in
  combine_list_typ "array_access" suffix ([string_of_id suffix 0 id_string] @ [string_of_sexpr suffix 1 sexpr]) typ
and string_of_scall typ existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  SFuncCall(id_string, sexprs) -> combine_list_typ "func_call" suffix ([string_of_id suffix 0 id_string] @ (mapiplus 1 (string_of_sexpr suffix) sexprs)) typ
| SMethodCall(id1, id2, sexprs) -> combine_list_typ "method_call" suffix ([string_of_id suffix 0 id1] @ [string_of_id suffix 1 id2] @ (mapiplus 2 (string_of_sexpr suffix) sexprs)) typ
and string_of_sassign existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  SRegularAssign(typ, id_string, sexpr) -> combine_list_typ "assign" suffix ([string_of_typ suffix 0 typ] @ [string_of_id suffix 1 id_string] @ [string_of_updateop suffix 2 Eq] @ [string_of_sexpr suffix 3 sexpr]) typ
and string_of_supdate typ existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  SRegularUpdate(id_string, updateop, sexpr) -> combine_list_typ "update" suffix ([string_of_id_typ suffix 0 id_string typ] @ [string_of_updateop suffix 1 updateop] @ [string_of_sexpr suffix 2 sexpr]) typ
| SObjectVariableUpdate(object_variable_access, updateop, sexpr) -> combine_list_typ "update" suffix ([string_of_object_variable_access suffix 0 object_variable_access] @ [string_of_updateop suffix 1 updateop] @ [string_of_sexpr suffix 2 sexpr]) typ
| SArrayAccessUpdate(sarray_access, updateop, sexpr) -> combine_list_typ "update" suffix ([string_of_sarray_access suffix 0 sarray_access typ] @ [string_of_updateop suffix 1 updateop] @ [string_of_sexpr suffix 2 sexpr]) typ

let rec string_of_sstmt existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  SExpr(sexpr) -> get_single_node_generator "expr" suffix string_of_sexpr sexpr
| SReturn(sexpr) -> get_single_node_generator "return" suffix string_of_sexpr sexpr
| SReturnVoid -> combine_list "return" suffix [("returnvoid" ^ suffix, [get_literal_node "returnvoid" suffix ("void")])]
| SIf(sexpr, sl1, selifs, sl2) -> combine_list "if" suffix ([string_of_sexpr suffix 0 sexpr] @ (mapiplus 1 (string_of_sstmt suffix) sl1) @ (mapiplus (1 + List.length sl1) (string_of_selif suffix) selifs) @ (mapiplus (1 + (List.length sl1) + (List.length selifs)) (string_of_sstmt suffix) sl2))
| SLoop(sexpr1, sexpr2, sl1) -> combine_list "loop" suffix ([string_of_sexpr suffix 0 sexpr1] @ [string_of_sexpr suffix 1 sexpr2] @ (mapiplus 2 (string_of_sstmt suffix) sl1))
and
string_of_selif existing_suffix new_index selif_tuple =
 let suffix = new_suffix existing_suffix new_index in
 combine_list "elif" suffix ([string_of_sexpr suffix 0 (fst selif_tuple)] @ (mapiplus 1 (string_of_sstmt suffix) (snd selif_tuple)))

let string_of_sfdecl existing_suffix new_index sfdecl =
  let suffix = new_suffix existing_suffix new_index in
  combine_list "fdecl" suffix ([string_of_id suffix 0 sfdecl.sfname] @ (mapiplus 1 (string_of_bind suffix) sfdecl.sformals) @ [string_of_typ suffix (1+List.length sfdecl.sformals) sfdecl.srtype] @ (mapiplus (2+List.length sfdecl.sformals) (string_of_sstmt suffix) sfdecl.sbody))

let string_of_sclassdecl existing_suffix new_index sclassdecl =
  let suffix = new_suffix existing_suffix new_index in
  combine_list "classdecl" suffix ([string_of_id suffix 0 sclassdecl.scname] @ (mapiplus 1 (string_of_sassign suffix) sclassdecl.sstatic_vars) @ (mapiplus (1+List.length sclassdecl.sstatic_vars) (string_of_bind suffix) sclassdecl.srequired_vars) @ (mapiplus (1+(List.length sclassdecl.sstatic_vars)+(List.length sclassdecl.srequired_vars)) (string_of_sassign suffix) sclassdecl.soptional_vars) @ (mapiplus (1+(List.length sclassdecl.sstatic_vars)+(List.length sclassdecl.srequired_vars)+(List.length sclassdecl.soptional_vars)) (string_of_sfdecl suffix) sclassdecl.smethods))

let string_of_sp_unit existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function (* Takes a program unit and returns a Tuple<StartNodeString, List<String>> *)
  SStmt(sstmt) -> get_single_node_generator "stmt" suffix string_of_sstmt sstmt
| SFdecl(sfdecl) -> string_of_sfdecl suffix 0 sfdecl
| SClassdecl(sclassdecl) -> string_of_sclassdecl suffix 0 sclassdecl

let string_of_sprogram sprogram = (* Takes a program object and returns a Tuple<StartNodeString, List<String>> *)
  let suffix = "0" in
  get_multi_node_generator "program" suffix string_of_sp_unit sprogram

let graphviz_string_of_sprogram sprogram =
  "digraph G { \n" ^ (String.concat "\n" (snd (string_of_sprogram sprogram))) ^ "\n}"

