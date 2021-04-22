type primitive = Int | Long | Float | Char | String | Bool | Void

type binop = Plus | Subtract | Times | Divide | Modulo | DoubleEq | BoGT | BoLT | BoGTE | BoLTE | BoOr | BoAnd

type unaryop = Not | Neg

type updateop = Eq

type typ = 
  Primitive of primitive
| Class of string
| Array of typ
| NullType

type bind = typ * string

type expr =
  IntLiteral of int
| LongLiteral of int64
| FloatLiteral of string
| CharLiteral of char
| StringLiteral of string
| BoolLiteral of bool
| Id of string
| Self
| NullExpr
| Call of call
| ObjectInstantiation of string * expr list
| ObjectVariableAccess of object_variable_access
| ArrayAccess of array_access
| ArrayLiteral of expr list
(* e.g. int[2+2][5][6] would be parsed as
   (Array(int), [2+2]) then
   (Array(Array(int)), [5, 2+2]) then
   (Array(Array(Array(int))), [6, 5, 2+2])
   Telling us this is a default triple array that is an array of length 6
   containing arrays of length 5 containing arrays of length 4. *)
| DefaultArray of typ * expr list
| Binop of expr * binop * expr
| Unop of unaryop * expr
| Assign of assign
| Update of update
and array_access = expr * expr (* First expr must be an array, second must be int *)
and call =
  FuncCall of string * expr list (* my_func(1, 2, 3) *)
| MethodCall of expr * string * expr list (* expr.identifier(params) *)
and assign =
  RegularAssign of typ * string * expr
and update =
  RegularUpdate of string * updateop * expr
| ObjectVariableUpdate of object_variable_access * updateop * expr
| ArrayAccessUpdate of array_access * updateop * expr
and object_variable_access = {
  ova_expr: expr;
  ova_class_name: string;
  ova_var_name: string;
  ova_is_static: bool;
}

type stmt =
  Expr of expr
| Return of expr
| ReturnVoid
| If of expr * stmt list * elif list * stmt list
| Loop of expr * expr * stmt list
and elif = expr * stmt list

type fdecl = {
  rtype: typ;
  fname: string;
  formals: bind list;
  body: stmt list;
}

type classdecl = {
  (* The name of the class being defined. *)
  cname: string;
  (* This is only used for the case of instantiating a generic class.
     e.g. in class StringToIntMap = HashMap(string, int), this will
     be set to HashMap, as this is a real valued instantiation of
     the generic HashMap. *)
  source_class_name: string;
  (* If this is the generic class definition, these are the names
     of the generic types. e.g. in class HashMap[K, V]: the generics
     are [K, V]. If this is the real class instantiation of the
     generic class, these are the types that fill in the generics.
     e.g. class StringToIntMap = HashMap(string, int), generics
     becomes [string, int]. If this is a regular class def, e.g.
     class MyClass:, then this list is empty. *)
  generics: typ list;
  static_vars: assign list;
  required_vars: bind list;
  optional_vars: assign list;
  methods: fdecl list;
}

type p_unit =
  Stmt of stmt
| Fdecl of fdecl
| Classdecl of classdecl

type program = p_unit list

(* Begin visualization functions *)
let rec mapiplus i f = function
    [] -> []
  | a::l -> let r = f i a in r :: mapiplus (i + 1) f l

let mapiplus plus f l = mapiplus plus f l

let get_label_without_suffix suffixed_name unsuffixed_name = suffixed_name ^ " [label=\"" ^ unsuffixed_name ^ "\"]"

let new_suffix existing_suffix new_index = existing_suffix ^ (string_of_int new_index)

let get_combine_function start_node subtuple = (start_node ^ " -> " ^ fst subtuple) :: snd subtuple

let get_single_node_generator node_name suffix subconverter subelement =
  let start_node = node_name ^ suffix in
  let subgraphs = (subconverter suffix 0 subelement) in
  (start_node, (get_label_without_suffix start_node node_name)::(get_combine_function start_node subgraphs))

let get_multi_node_generator node_name suffix subconverter input_list =
  let start_node = node_name ^ suffix in
  let subgraphs = (List.mapi (subconverter suffix) input_list) in (* List<Tuple<StartNodeString, List<String>> *)
  (start_node, (get_label_without_suffix start_node node_name)::List.concat (List.map (get_combine_function start_node) subgraphs))

let combine_list node_name suffix input_list =
  let start_node = node_name ^ suffix in
  (start_node, (get_label_without_suffix start_node node_name)::List.concat (List.map (get_combine_function start_node) input_list))

let get_op_node_label name suffix symbol = name ^ suffix ^ " [label=\"" ^ symbol ^ "\", color=transparent]"
let get_prim_node_label name suffix = name ^ suffix ^ " [label=\"" ^ name ^ "\", color=aliceblue, fillcolor=aliceblue, style=filled]"
let get_class_node_label name suffix classname = name ^ suffix ^ " [label=\"" ^ classname ^ "\", color=aliceblue, fillcolor=aliceblue, style=filled]"
let get_literal_node name suffix value = name ^ suffix ^ " [label=\"" ^ value ^ "\", color=green, fillcolor=green, style=filled]"

let string_of_id existing_suffix new_index id_string =
  let suffix = new_suffix existing_suffix new_index in
  ("id" ^ suffix, ["id" ^ suffix ^ " [label=\"id: " ^ id_string ^ "\" fontcolor=red]"])

let rec string_of_typ existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  Primitive(primitive) -> string_of_primitive suffix 0 primitive
| Class(class_string) -> ("class" ^ suffix, [get_class_node_label "class" suffix class_string])
| Array(typ) -> combine_list "array_type" suffix ([string_of_typ suffix 0 typ])
| NullType -> ("NULL" ^ suffix, [get_class_node_label "NULL" suffix "NULL"])
and string_of_primitive existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
| Int -> ("int" ^ suffix, [get_prim_node_label "int" suffix])
| Long -> ("long" ^ suffix, [get_prim_node_label "long" suffix])
| Float -> ("float" ^ suffix, [get_prim_node_label "float" suffix])
| Char -> ("char" ^ suffix, [get_prim_node_label "char" suffix])
| String -> ("string" ^ suffix, [get_prim_node_label "string" suffix])
| Bool -> ("boolean" ^ suffix, [get_prim_node_label "boolean" suffix])
| Void -> ("void" ^ suffix, [get_prim_node_label "void" suffix])
and string_of_expr existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  IntLiteral(integer) -> ("intlit" ^ suffix, [get_literal_node "intlit" suffix (string_of_int integer)])
| LongLiteral(long) -> ("longlit" ^ suffix, [get_literal_node "longlit" suffix (Int64.to_string long)])
| FloatLiteral(f) -> ("floatlit" ^ suffix, [get_literal_node "floatlit" suffix f])
| CharLiteral(c) -> ("charlit" ^ suffix, [get_literal_node "charlit" suffix (String.make 1 c)])
| StringLiteral(s) -> ("stringlit" ^ suffix, [get_literal_node "stringlit" suffix s])
| BoolLiteral(b) -> ("boollit" ^ suffix, [get_literal_node "boollit" suffix (string_of_bool b)])
| Id(id_string) -> string_of_id suffix 0 id_string
| NullExpr -> ("nullexpr" ^ suffix, [get_literal_node "nullexpr" suffix "NULL"])
| Self -> string_of_id suffix 0 "self"
| Call(call) -> string_of_call suffix 0 call
| ObjectInstantiation(id_string, exprs) -> combine_list "object_instantiation" suffix ([string_of_id suffix 0 id_string] @ (mapiplus 1 (string_of_expr suffix) exprs))
| ObjectVariableAccess(object_variable_access) -> string_of_object_variable_access suffix 0 object_variable_access
| ArrayAccess(array_access) -> string_of_array_access suffix 0 array_access
| ArrayLiteral(exprs) -> get_multi_node_generator "array_literal" suffix string_of_expr exprs
| DefaultArray(typ, exprs) -> combine_list "default_array" suffix ([string_of_typ suffix 0 typ] @ (mapiplus 1 (string_of_expr suffix) exprs))
| Binop(expr1, binop, expr2) -> combine_list "binop" suffix ([string_of_expr suffix 0 expr1] @ [string_of_binoperator suffix 1 binop] @ [string_of_expr suffix 2 expr2])
| Unop(unaryop, expr) -> combine_list "unaryop" suffix ([string_of_unaryop suffix 0 unaryop] @ [string_of_expr suffix 1 expr])
| Assign(assign) -> string_of_assign suffix 0 assign
| Update(update) -> string_of_update suffix 0 update
and string_of_array_access existing_suffix new_index array_access =
  let suffix = new_suffix existing_suffix new_index in
  let expr1 = (fst array_access) in
  let expr2 = (snd array_access) in
  combine_list "array_access" suffix ([string_of_expr suffix 0 expr1] @ [string_of_expr suffix 1 expr2])
and string_of_call existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  FuncCall(id_string, exprs) -> combine_list "func_call" suffix ([string_of_id suffix 0 id_string] @ (mapiplus 1 (string_of_expr suffix) exprs))
| MethodCall(expr1, id2, exprs) -> combine_list "method_call" suffix ([string_of_expr suffix 0 expr1] @ [string_of_id suffix 1 id2] @ (mapiplus 2 (string_of_expr suffix) exprs))
and string_of_assign existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  RegularAssign(typ, id_string, expr) -> combine_list "assign" suffix ([string_of_typ suffix 0 typ] @ [string_of_id suffix 1 id_string] @ [string_of_updateop suffix 2 Eq] @ [string_of_expr suffix 3 expr])
and string_of_update existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  RegularUpdate(id_string, updateop, expr) -> combine_list "update" suffix ([string_of_id suffix 0 id_string] @ [string_of_updateop suffix 1 updateop] @ [string_of_expr suffix 2 expr])
| ObjectVariableUpdate(object_variable_access, updateop, expr) -> combine_list "update" suffix ([string_of_object_variable_access suffix 0 object_variable_access] @ [string_of_updateop suffix 1 updateop] @ [string_of_expr suffix 2 expr])
| ArrayAccessUpdate(array_access, updateop, expr) -> combine_list "update" suffix ([string_of_array_access suffix 0 array_access] @ [string_of_updateop suffix 1 updateop] @ [string_of_expr suffix 2 expr])
and string_of_binoperator existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  Plus -> ("plus" ^ suffix, [get_op_node_label "plus" suffix "+"])
| Subtract -> ("subtract" ^ suffix, [get_op_node_label "subtract" suffix "-"])
| Times -> ("times" ^ suffix, [get_op_node_label "times" suffix "*"])
| Divide -> ("divide" ^ suffix, [get_op_node_label "divide" suffix "÷"])
| Modulo -> ("modulo" ^ suffix, [get_op_node_label "modulo" suffix "%"])
| DoubleEq -> ("doubleeq" ^ suffix, [get_op_node_label "doubleeq" suffix "=="])
| BoGT -> ("gt" ^ suffix, [get_op_node_label "gt" suffix ">"])
| BoLT -> ("lt" ^ suffix, [get_op_node_label "lt" suffix "<"])
| BoGTE -> ("gte" ^ suffix, [get_op_node_label "gte" suffix ">="])
| BoLTE -> ("lte" ^ suffix, [get_op_node_label "lte" suffix "<="])
| BoOr -> ("or" ^ suffix, [get_op_node_label "or" suffix "or"])
| BoAnd -> ("and" ^ suffix, [get_op_node_label "and" suffix "and"])
and string_of_updateop existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
| Eq -> ("eq" ^ suffix, [get_op_node_label "eq" suffix "="])
and string_of_unaryop existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  Not -> ("not" ^ suffix, [get_op_node_label "not" suffix "not"])
| Neg -> ("neg" ^ suffix, [get_op_node_label "neg" suffix "-"])
and string_of_object_variable_access existing_suffix new_index = function
  { ova_class_name = class_name; ova_var_name = var_name; ova_is_static = true; _ } ->
    let suffix = new_suffix existing_suffix new_index in
    ("static_var_access" ^ suffix, ["static_var_access" ^ suffix ^ " [label=\"" ^ (class_name) ^ "." ^ (var_name) ^"\"]"])
| { ova_expr = expr; ova_var_name = var_name; ova_is_static = false; _ } ->
    let suffix = new_suffix existing_suffix new_index in
    combine_list "obj_var_access" suffix ([string_of_expr suffix 0 expr] @ [string_of_id suffix 1 var_name])

let rec string_of_stmt existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function
  Expr(expr) -> get_single_node_generator "expr" suffix string_of_expr expr
| Return(expr) -> get_single_node_generator "return" suffix string_of_expr expr
| ReturnVoid -> combine_list "return" suffix [("returnvoid" ^ suffix, [get_literal_node "returnvoid" suffix ("void")])]
| If(expr, sl1, elifs, sl2) -> combine_list "if" suffix ([string_of_expr suffix 0 expr] @ (mapiplus 1 (string_of_stmt suffix) sl1) @ (mapiplus (1 + List.length sl1) (string_of_elif suffix) elifs) @ (mapiplus (1 + (List.length sl1) + (List.length elifs)) (string_of_stmt suffix) sl2))
| Loop(expr1, expr2, sl1) -> combine_list "loop" suffix ([string_of_expr suffix 0 expr1] @ [string_of_expr suffix 1 expr2] @ (mapiplus 2 (string_of_stmt suffix) sl1))
and
string_of_elif existing_suffix new_index elif_tuple =
 let suffix = new_suffix existing_suffix new_index in
 combine_list "elif" suffix ([string_of_expr suffix 0 (fst elif_tuple)] @ (mapiplus 1 (string_of_stmt suffix) (snd elif_tuple)))

let string_of_bind existing_suffix new_index bind =
  let suffix = new_suffix existing_suffix new_index in
  combine_list "bind" suffix ([string_of_typ suffix 0 (fst bind)] @ [string_of_id suffix 1 (snd bind)])

let string_of_fdecl existing_suffix new_index fdecl =
  let suffix = new_suffix existing_suffix new_index in
  combine_list "fdecl" suffix ([string_of_id suffix 0 fdecl.fname] @ (mapiplus 1 (string_of_bind suffix) fdecl.formals) @ [string_of_typ suffix (1+List.length fdecl.formals) fdecl.rtype] @ (mapiplus (2+List.length fdecl.formals) (string_of_stmt suffix) fdecl.body))

let string_of_classdecl existing_suffix new_index classdecl =
  let suffix = new_suffix existing_suffix new_index in
  combine_list "classdecl" suffix ([string_of_id suffix 0 classdecl.cname] @ (mapiplus 1 (string_of_assign suffix) classdecl.static_vars) @ (mapiplus (1+List.length classdecl.static_vars) (string_of_bind suffix) classdecl.required_vars) @ (mapiplus (1+(List.length classdecl.static_vars)+(List.length classdecl.required_vars)) (string_of_assign suffix) classdecl.optional_vars) @ (mapiplus (1+(List.length classdecl.static_vars)+(List.length classdecl.required_vars)+(List.length classdecl.optional_vars)) (string_of_fdecl suffix) classdecl.methods) @ [string_of_id suffix (1+(List.length classdecl.static_vars)+(List.length classdecl.required_vars)+(List.length classdecl.optional_vars)+(List.length classdecl.methods)) classdecl.source_class_name] @ (mapiplus (2+(List.length classdecl.static_vars)+(List.length classdecl.required_vars)+(List.length classdecl.optional_vars)+(List.length classdecl.methods)) (string_of_typ suffix) classdecl.generics))

let string_of_p_unit existing_suffix new_index =
  let suffix = new_suffix existing_suffix new_index in
  function (* Takes a program unit and returns a Tuple<StartNodeString, List<String>> *)
  Stmt(stmt) -> get_single_node_generator "stmt" suffix string_of_stmt stmt
| Fdecl(fdecl) -> string_of_fdecl suffix 0 fdecl
| Classdecl(classdecl) -> string_of_classdecl suffix 0 classdecl

let string_of_program program = (* Takes a program object and returns a Tuple<StartNodeString, List<String>> *)
  let suffix = "0" in
  get_multi_node_generator "program" suffix string_of_p_unit program

let graphviz_string_of_program program =
  "digraph G { \n" ^ (String.concat "\n" (snd (string_of_program program))) ^ "\n}"
