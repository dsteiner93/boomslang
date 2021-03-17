type primitive = Int | Long | Float | Char | String | Bool | Void

type binop = Plus | Subtract | Times | Divide | Modulo | DoubleEq | NotEq | BoGT | BoLT | BoGTE | BoLTE | BoOr | BoAnd

type unaryop = Not | Neg

type updateop = Eq | PlusEq | MinusEq | TimesEq | DivideEq

type typ = 
  Primitive of primitive
| Class of string
| Array of typ * int

type bind = typ * string

type object_variable_access = string * string

type expr =
  IntLiteral of int
| LongLiteral of int64
| FloatLiteral of string
| CharLiteral of char
| StringLiteral of string
| BoolLiteral of bool
| Id of string
| NullExpr
| Call of call
| Paren of expr
| ObjectInstantiation of string * expr list
| ObjectVariableAccess of object_variable_access
| ArrayAccess of string * expr
| ArrayLiteral of expr list
| Binop of expr * binop * expr
| Unop of  unaryop * expr
| Assign of assign
| Update of update
and call =
  FuncCall of string * expr list (* my_func(1, 2, 3) *)
| MethodCall of string * string * expr list (* object.identifier(params) *)
and assign =
  RegularAssign of typ * string * expr
| ObjectVariableAssign of object_variable_access * expr
and update =
  RegularUpdate of string * updateop * expr
| ObjectVariableUpdate of object_variable_access * updateop * expr

type stmt =
  Expr of expr
| Return of expr
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
  cname: string;
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

let get_combine_function start_node subtuple = (start_node ^ " -> " ^ fst subtuple) :: snd subtuple

let get_single_node_generator node_name suffix subconverter subelement =
  let start_node = node_name ^ suffix in
  let subgraphs = (subconverter suffix 0 subelement) in
  (start_node, (get_combine_function start_node subgraphs))

let get_multi_node_generator node_name suffix subconverter input_list =
  let start_node = node_name ^ suffix in
  let subgraphs = (List.mapi (subconverter suffix) input_list) in (* List<Tuple<StartNodeString, List<String>> *)
  (start_node, List.concat (List.map (get_combine_function start_node) subgraphs))

let combine_list node_name suffix input_list =
  let start_node = node_name ^ suffix in
  (start_node, List.concat (List.map (get_combine_function start_node) input_list))

let get_op_node_label name suffix symbol = name ^ suffix ^ " [label=\"" ^ symbol ^ "\", color=transparent]"
let get_prim_node_label name suffix = name ^ suffix ^ " [label=\"" ^ name ^ "\", color=aliceblue, fillcolor=aliceblue, style=filled]"
let get_class_node_label name suffix classname = name ^ suffix ^ " [label=\"" ^ classname ^ "\", color=aliceblue, fillcolor=aliceblue, style=filled]"
let get_literal_node name suffix value = name ^ suffix ^ " [label=\"" ^ value ^ "\", color=green, fillcolor=green, style=filled]"

let string_of_id existing_suffix new_index id_string =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  ("id" ^ suffix, ["id" ^ suffix ^ " [label=\"id: " ^ id_string ^ "\" fontcolor=red]"])

let string_of_object_variable_access existing_suffix new_index object_variable_access =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  ("obj_var_access" ^ suffix, ["obj_var_access" ^ suffix ^ " [label=\"" ^ (fst object_variable_access) ^ "." ^ (snd object_variable_access) ^"\"]"])

let rec string_of_typ existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
  Primitive(primitive) -> string_of_primitive suffix 0 primitive
| Class(class_string) -> ("class" ^ suffix, [get_class_node_label "class" suffix class_string])
| Array(typ, integer) -> combine_list "array_type" suffix ([string_of_typ suffix 0 typ] @ [string_of_expr suffix 1 (IntLiteral integer)])
and string_of_primitive existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
| Int -> ("int" ^ suffix, [get_prim_node_label "int" suffix])
| Long -> ("long" ^ suffix, [get_prim_node_label "long" suffix])
| Float -> ("float" ^ suffix, [get_prim_node_label "float" suffix])
| Char -> ("char" ^ suffix, [get_prim_node_label "char" suffix])
| String -> ("string" ^ suffix, [get_prim_node_label "string" suffix])
| Bool -> ("boolean" ^ suffix, [get_prim_node_label "boolean" suffix])
| Void -> ("void" ^ suffix, [get_prim_node_label "void" suffix])
and string_of_expr existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
  IntLiteral(integer) -> ("intlit" ^ suffix, [get_literal_node "intlit" suffix (Int.to_string integer)])
| LongLiteral(long) -> ("longlit" ^ suffix, [get_literal_node "longlit" suffix (Int64.to_string long)])
| FloatLiteral(f) -> ("floatlit" ^ suffix, [get_literal_node "floatlit" suffix f])
| CharLiteral(c) -> ("charlit" ^ suffix, [get_literal_node "charlit" suffix (String.make 1 c)])
| StringLiteral(s) -> ("stringlit" ^ suffix, [get_literal_node "stringlit" suffix s])
| BoolLiteral(b) -> ("boollit" ^ suffix, [get_literal_node "boollit" suffix (Bool.to_string b)])
| Id(id_string) -> string_of_id suffix 0 id_string
| NullExpr -> ("nullexpr" ^ suffix, [get_literal_node "nullexpr" suffix "NULL"])
| Call(call) -> string_of_call suffix 0 call
| Paren(expr) -> get_single_node_generator "paren" suffix string_of_expr expr
| ObjectInstantiation(id_string, exprs) -> combine_list "object_instantiation" suffix ([string_of_id suffix 0 id_string] @ (mapiplus 1 (string_of_expr suffix) exprs))
| ObjectVariableAccess(object_variable_access) -> string_of_object_variable_access suffix 0 object_variable_access
| ArrayAccess(id_string, expr) -> combine_list "array_access" suffix ([string_of_id suffix 0 id_string] @ [string_of_expr suffix 1 expr])
| ArrayLiteral(exprs) -> get_multi_node_generator "array_literal" suffix string_of_expr exprs
| Binop(expr1, binop, expr2) -> combine_list "binop" suffix ([string_of_expr suffix 0 expr1] @ [string_of_binoperator suffix 1 binop] @ [string_of_expr suffix 2 expr2])
| Unop(unaryop, expr) -> combine_list "unaryop" suffix ([string_of_unaryop suffix 0 unaryop] @ [string_of_expr suffix 1 expr])
| Assign(assign) -> string_of_assign suffix 0 assign
| Update(update) -> string_of_update suffix 0 update
and string_of_call existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
  FuncCall(id_string, exprs) -> combine_list "func_call" suffix ([string_of_id suffix 0 id_string] @ (mapiplus 1 (string_of_expr suffix) exprs))
| MethodCall(id1, id2, exprs) -> combine_list "method_call" suffix ([string_of_id suffix 0 id1] @ [string_of_id suffix 1 id2] @ (mapiplus 2 (string_of_expr suffix) exprs))
and string_of_assign existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
  RegularAssign(typ, id_string, expr) -> combine_list "assign" suffix ([string_of_typ suffix 0 typ] @ [string_of_id suffix 1 id_string] @ [string_of_updateop suffix 2 Eq] @ [string_of_expr suffix 3 expr])
| ObjectVariableAssign(object_variable_access, expr) -> combine_list "assign" suffix ([string_of_object_variable_access suffix 1 object_variable_access] @ [string_of_expr suffix 1 expr])
and string_of_update existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
  RegularUpdate(id_string, updateop, expr) -> combine_list "update" suffix ([string_of_id suffix 0 id_string] @ [string_of_updateop suffix 1 updateop] @ [string_of_expr suffix 2 expr])
| ObjectVariableUpdate(object_variable_access, updateop, expr) -> combine_list "update" suffix ([string_of_object_variable_access suffix 0 object_variable_access] @ [string_of_updateop suffix 1 updateop] @ [string_of_expr suffix 2 expr])
and string_of_binoperator existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
  Plus -> ("plus" ^ suffix, [get_op_node_label "plus" suffix "+"])
| Subtract -> ("subtract" ^ suffix, [get_op_node_label "subtract" suffix "-"])
| Times -> ("times" ^ suffix, [get_op_node_label "times" suffix "*"])
| Divide -> ("divide" ^ suffix, [get_op_node_label "divide" suffix "÷"])
| Modulo -> ("modulo" ^ suffix, [get_op_node_label "modulo" suffix "%"])
| DoubleEq -> ("doubleeq" ^ suffix, [get_op_node_label "doubleeq" suffix "=="])
| NotEq -> ("noteq" ^ suffix, [get_op_node_label "noteq" suffix "!="])
| BoGT -> ("gt" ^ suffix, [get_op_node_label "gt" suffix ">"])
| BoLT -> ("lt" ^ suffix, [get_op_node_label "lt" suffix "<"])
| BoGTE -> ("gte" ^ suffix, [get_op_node_label "gte" suffix ">="])
| BoLTE -> ("lte" ^ suffix, [get_op_node_label "lte" suffix "<="])
| BoOr -> ("or" ^ suffix, [get_op_node_label "or" suffix "or"])
| BoAnd -> ("and" ^ suffix, [get_op_node_label "and" suffix "and"])
and string_of_updateop existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
| Eq -> ("eq" ^ suffix, [get_op_node_label "eq" suffix "="])
| PlusEq -> ("pluseq" ^ suffix, [get_op_node_label "pluseq" suffix "+="])
| MinusEq -> ("minuseq" ^ suffix, [get_op_node_label "minuseq" suffix "-="])
| TimesEq -> ("timeseq" ^ suffix, [get_op_node_label "timeseq" suffix "*="])
| DivideEq -> ("divideeq" ^ suffix, [get_op_node_label "divideeq" suffix "/="])
and string_of_unaryop existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
  Not -> ("not" ^ suffix, [get_op_node_label "not" suffix "not"])
| Neg -> ("neg" ^ suffix, [get_op_node_label "neg" suffix "-"])

let rec string_of_stmt existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function
  Expr(expr) -> get_single_node_generator "expr" suffix string_of_expr expr
| Return(expr) -> get_single_node_generator "return" suffix string_of_expr expr
| If(expr, sl1, elifs, sl2) -> combine_list "if" suffix ([string_of_expr suffix 0 expr] @ (mapiplus 1 (string_of_stmt suffix) sl1) @ (mapiplus (1 + List.length sl1) (string_of_elif suffix) elifs) @ (mapiplus (1 + (List.length sl1) + (List.length elifs)) (string_of_stmt suffix) sl2))
| Loop(expr1, expr2, sl1) -> combine_list "loop" suffix ([string_of_expr suffix 0 expr1] @ [string_of_expr suffix 1 expr2] @ (mapiplus 2 (string_of_stmt suffix) sl1))
and
string_of_elif existing_suffix new_index elif_tuple =
 let suffix = existing_suffix ^ (Int.to_string new_index) in
 combine_list "elif" suffix ([string_of_expr suffix 0 (fst elif_tuple)] @ (mapiplus 1 (string_of_stmt suffix) (snd elif_tuple)))

let string_of_bind existing_suffix new_index bind =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  combine_list "bind" suffix ([string_of_typ suffix 0 (fst bind)] @ [string_of_id suffix 1 (snd bind)])

let string_of_fdecl existing_suffix new_index fdecl =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  combine_list "fdecl" suffix ([string_of_id suffix 0 fdecl.fname] @ (mapiplus 1 (string_of_bind suffix) fdecl.formals) @ [string_of_typ suffix (1+List.length fdecl.formals) fdecl.rtype] @ (mapiplus (2+List.length fdecl.formals) (string_of_stmt suffix) fdecl.body))

let string_of_classdecl existing_suffix new_index classdecl =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  combine_list "classdecl" suffix ([string_of_id suffix 0 classdecl.cname] @ (mapiplus 1 (string_of_assign suffix) classdecl.static_vars) @ (mapiplus (1+List.length classdecl.static_vars) (string_of_bind suffix) classdecl.required_vars) @ (mapiplus (1+(List.length classdecl.static_vars)+(List.length classdecl.required_vars)) (string_of_assign suffix) classdecl.optional_vars) @ (mapiplus (1+(List.length classdecl.static_vars)+(List.length classdecl.required_vars)+(List.length classdecl.optional_vars)) (string_of_fdecl suffix) classdecl.methods))

let string_of_p_unit existing_suffix new_index =
  let suffix = existing_suffix ^ (Int.to_string new_index) in
  function (* Takes a program unit and returns a Tuple<StartNodeString, List<String>> *)
  Stmt(stmt) -> get_single_node_generator "stmt" suffix string_of_stmt stmt
| Fdecl(fdecl) -> string_of_fdecl suffix 0 fdecl
| Classdecl(classdecl) -> string_of_classdecl suffix 0 classdecl

let string_of_program program = (* Takes a program object and returns a Tuple<StartNodeString, List<String>> *)
  let suffix = "0" in
  get_multi_node_generator "program" suffix string_of_p_unit program

let graphviz_string_of_program program =
  "digraph G { \n" ^ (String.concat "\n" (snd (string_of_program program))) ^ "\n}"
