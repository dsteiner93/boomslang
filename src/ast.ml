type program = {
  p_stmts: stmt list;
  p_fdecls: fdecl list;
  p_classdecls: classdecl list;
}

type primitive = Int | Long | Float | Char | String | Bool | Void

type typ = 
  Primitive of primitive
| Class of string
| Array of typ

type bind = typ * string

// def my_func(int x, Object[] myarray, Object my_object) returns MyOtherObject
// my_func(1, [1,2], Object())
// parser will see DEF IDENTIFIER LPAREN TYPE IDENTIFIER COMMA TYPE IDENTIFIER COMMA TYPE IDENTIFIER RPAREN RETURNS TYPE

type fdecl = {
  return_type: typ;
  fname: string;
  formals: bind list;
  body: stmt list;
}

type stmt =
  Expr of expr
| Return of expr
| If of expr * stmt list * elif list * stmt list
| Loop of expr * expr * stmt list

//LOOP expr WHILE expr:NEWLINE INDENT stmts DEDENT

type elif = expr * stmt list

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
| Binop of expr * binop * expr
| Unop of  uop * expr
| Paren of expr

type binop = Plus | Subtract | Times | Divide | Modulo | ObjOperator | DoubleEq | NotEq | GT | LT | GTE | LTE | OR | AND

type uop = Not | Neg

type call = 
  string * (expr list) (* my_func(1, 2, 3) *)
| string * string * expr list (* object.identifier(params) *)