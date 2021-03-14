type primitive = Int | Long | Float | Char | String | Bool | Void

type binop = Plus | Subtract | Times | Divide | Modulo | ObjOperator | DoubleEq | NotEq | BoGT | BoLT | BoGTE | BoLTE | BoOr | BoAnd

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

type program = {
  p_stmts: stmt list;
  p_fdecls: fdecl list;
  p_classdecls: classdecl list;
}

