%{
open Ast
%}

/* Primitive types */
%token INT LONG FLOAT BOOLEAN CHAR STRING VOID
/* Boolean operators */
%token NOT OR AND
/* Loops and conditionals */
%token LOOP WHILE IF ELIF ELSE
/* Named literals */
%token NULL
/* Words related to functions and classes */
%token DEF CLASS SELF RETURN RETURNS STATIC REQUIRED OPTIONAL
/* Mathematical operators */
%token PLUS MINUS TIMES DIVIDE MODULO
/* Assignment operators */
%token EQ PLUS_EQ MINUS_EQ TIMES_EQ DIVIDE_EQ
/* Comparison operators */
%token DOUBLE_EQ NOT_EQ GT LT GTE LTE
/* Misc. punctuation */
%token LPAREN RPAREN LBRACKET RBRACKET COLON PERIOD COMMA UNDERSCORE
/* Syntactically significant whitespace */
%token NEWLINE INDENT DEDENT EOF
/* Misc. Keywords */
%token DEFAULT
/* Parameterized tokens */
%token <int> INT_LITERAL
%token <int64> LONG_LITERAL
%token <string> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOLEAN_LITERAL
%token <string> CLASS_NAME
%token <string> IDENTIFIER
%token <string> OBJ_OPERATOR

/* Set precedence and associativity rules */
/* https://docs.python.org/3/reference/expressions.html#operator-precedence */
%right EQ PLUS_EQ MINUS_EQ TIMES_EQ DIVIDE_EQ
%left OR
%left AND
%left NOT
%left DOUBLE_EQ NOT_EQ GT LT GTE LTE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left OBJ_OPERATOR
%left PERIOD
%nonassoc UNARY_MINUS
%nonassoc FIELD

%start program /* the entry point */
%type <Ast.program> program

%%

program:
  program_without_eof EOF { List.rev $1 }

program_without_eof:
  program_without_eof stmt { (Stmt $2)::$1 }
| program_without_eof fdecl { (Fdecl $2)::$1 }
| program_without_eof classdecl { (Classdecl $2)::$1 }
| program_without_eof NEWLINE { $1 }
| /* nothing */ { [] }

stmts:
  { [] }
| stmts stmt { $2 :: $1 }

stmt:
  expr NEWLINE { Expr $1 }
| RETURN expr NEWLINE { Return $2 }
| RETURN NEWLINE { ReturnVoid }
| RETURN VOID NEWLINE { ReturnVoid }
| if_stmt  { $1 }
| loop { $1 }

if_stmt:
  IF expr COLON NEWLINE INDENT stmts DEDENT { If ($2, List.rev $6, [], []) }
| IF expr COLON NEWLINE INDENT stmts DEDENT ELSE COLON NEWLINE INDENT stmts DEDENT { If ($2, List.rev $6, [], List.rev $12) }
| IF expr COLON NEWLINE INDENT stmts DEDENT elif ELSE COLON NEWLINE INDENT stmts DEDENT { If ($2, List.rev $6, List.rev $8, List.rev $13) }
| IF expr COLON NEWLINE INDENT stmts DEDENT elif { If ($2, List.rev $6, List.rev $8, []) }

fdecl:
  DEF IDENTIFIER LPAREN type_params RPAREN RETURNS typ COLON NEWLINE INDENT stmts DEDENT { {rtype = $7; fname = $2; formals = List.rev $4; body = List.rev $11} }
| DEF IDENTIFIER LPAREN type_params RPAREN COLON NEWLINE INDENT stmts DEDENT { {rtype = Primitive Void; fname = $2; formals = List.rev $4; body = List.rev $9} }
| DEF IDENTIFIER LPAREN RPAREN RETURNS typ COLON NEWLINE INDENT stmts DEDENT { {rtype = $6; fname = $2; formals = []; body = List.rev $10} }
| DEF IDENTIFIER LPAREN RPAREN COLON NEWLINE INDENT stmts DEDENT { {rtype = Primitive Void; fname = $2; formals = []; body = List.rev $8} }
| DEF UNDERSCORE OBJ_OPERATOR LPAREN typ IDENTIFIER RPAREN RETURNS typ COLON NEWLINE INDENT stmts DEDENT { {rtype = $9; fname = "_" ^ $3; formals = [($5, $6)]; body = List.rev $13} }

elif:
  ELIF expr COLON NEWLINE INDENT stmts DEDENT { [($2, List.rev $6)] }
| elif ELIF expr COLON NEWLINE INDENT stmts DEDENT { ($3, List.rev $7) :: $1 }

loop:
  LOOP expr WHILE expr COLON NEWLINE INDENT stmts DEDENT { Loop ($2, $4, List.rev $8) }
| LOOP WHILE expr COLON NEWLINE INDENT stmts DEDENT { Loop (NullExpr, $3, List.rev $7) }

type_params:  /* these are the method signature type */
  typ IDENTIFIER { [($1, $2)] }
| type_params COMMA typ IDENTIFIER { ($3, $4) :: $1 }

params: /* these are the params used to invoke a function */
  expr { [$1] }
| params COMMA expr { $3 :: $1 }

classdecl:
  CLASS CLASS_NAME COLON NEWLINE
    INDENT STATIC COLON NEWLINE INDENT assigns NEWLINE
    DEDENT REQUIRED COLON NEWLINE INDENT vdecls NEWLINE
    DEDENT OPTIONAL COLON NEWLINE INDENT assigns NEWLINE
    DEDENT optional_fdecls DEDENT { {cname = $2; static_vars = List.rev $10; required_vars = List.rev $17; optional_vars = List.rev $24; methods = List.rev $27} }
| CLASS CLASS_NAME COLON NEWLINE
    INDENT optional_fdecls DEDENT { {cname = $2; static_vars = []; required_vars = []; optional_vars = []; methods = List.rev $6} }
| CLASS CLASS_NAME COLON NEWLINE
    INDENT STATIC COLON NEWLINE INDENT assigns NEWLINE
    DEDENT optional_fdecls DEDENT { {cname = $2; static_vars = List.rev $10; required_vars = []; optional_vars = []; methods = List.rev $13} }
| CLASS CLASS_NAME COLON NEWLINE
    INDENT REQUIRED COLON NEWLINE INDENT vdecls NEWLINE
    DEDENT optional_fdecls DEDENT { {cname = $2; static_vars = []; required_vars = List.rev $10; optional_vars = []; methods = List.rev $13} }
| CLASS CLASS_NAME COLON NEWLINE
    INDENT OPTIONAL COLON NEWLINE INDENT assigns NEWLINE
    DEDENT optional_fdecls DEDENT { {cname = $2; static_vars = []; required_vars = []; optional_vars = List.rev $10; methods = List.rev $13} }
| CLASS CLASS_NAME COLON NEWLINE
    INDENT STATIC COLON NEWLINE INDENT assigns NEWLINE
    DEDENT REQUIRED COLON NEWLINE INDENT vdecls NEWLINE
    DEDENT optional_fdecls DEDENT { {cname = $2; static_vars = List.rev $10; required_vars = List.rev $17; optional_vars = []; methods = List.rev $20} }
| CLASS CLASS_NAME COLON NEWLINE
    INDENT STATIC COLON NEWLINE INDENT assigns NEWLINE
    DEDENT OPTIONAL COLON NEWLINE INDENT assigns NEWLINE
    DEDENT optional_fdecls DEDENT { {cname = $2; static_vars = List.rev $10; required_vars = []; optional_vars = List.rev $17; methods = List.rev $20} }
| CLASS CLASS_NAME COLON NEWLINE
    INDENT REQUIRED COLON NEWLINE INDENT vdecls NEWLINE
    DEDENT OPTIONAL COLON NEWLINE INDENT assigns NEWLINE
    DEDENT optional_fdecls DEDENT { {cname = $2; static_vars = []; required_vars = List.rev $10; optional_vars = List.rev $17; methods = List.rev $20} }

optional_fdecls:
  fdecls { $1 }
| /* nothing */ { [] }

fdecls:
  fdecl { [$1] }
| fdecls fdecl { $2::$1 }

vdecls:
  vdecl { [$1] }
| vdecls NEWLINE vdecl { $3::$1 }

vdecl:
  typ IDENTIFIER { ($1, $2) }

assigns:
  assign { [$1] }
| assigns NEWLINE assign { $3::$1 }

assign:
  typ IDENTIFIER EQ expr { RegularAssign ($1, $2, $4) }

assign_update:
  IDENTIFIER EQ expr { RegularUpdate ($1, Eq, $3) }
| IDENTIFIER PLUS_EQ expr { RegularUpdate ($1, Eq, Binop(Id($1), Plus, $3)) }
| IDENTIFIER MINUS_EQ expr { RegularUpdate ($1, Eq, Binop(Id($1), Subtract, $3)) }
| IDENTIFIER TIMES_EQ expr { RegularUpdate ($1, Eq, Binop(Id($1), Times, $3)) }
| IDENTIFIER DIVIDE_EQ expr { RegularUpdate ($1, Eq, Binop(Id($1), Divide, $3)) }
| object_variable_access EQ expr { ObjectVariableUpdate ($1, Eq, $3) }
| object_variable_access PLUS_EQ expr { ObjectVariableUpdate ($1, Eq, Binop(ObjectVariableAccess($1), Plus, $3)) }
| object_variable_access MINUS_EQ expr { ObjectVariableUpdate ($1, Eq, Binop(ObjectVariableAccess($1), Subtract, $3)) }
| object_variable_access TIMES_EQ expr { ObjectVariableUpdate ($1, Eq, Binop(ObjectVariableAccess($1), Times, $3)) }
| object_variable_access DIVIDE_EQ expr { ObjectVariableUpdate ($1, Eq, Binop(ObjectVariableAccess($1), Divide, $3)) }
| array_access EQ expr { ArrayAccessUpdate ($1, Eq, $3) }
| array_access PLUS_EQ expr { ArrayAccessUpdate ($1, Eq, Binop(ArrayAccess($1), Plus, $3)) }
| array_access MINUS_EQ expr { ArrayAccessUpdate ($1, Eq, Binop(ArrayAccess($1), Subtract, $3)) }
| array_access TIMES_EQ expr { ArrayAccessUpdate ($1, Eq, Binop(ArrayAccess($1), Times, $3)) }
| array_access DIVIDE_EQ expr { ArrayAccessUpdate ($1, Eq, Binop(ArrayAccess($1), Divide, $3)) }

func_call:
  expr PERIOD IDENTIFIER LPAREN params RPAREN { MethodCall ($1, $3, List.rev $5) }
| IDENTIFIER LPAREN params RPAREN { FuncCall ($1, List.rev $3) }
| expr PERIOD IDENTIFIER LPAREN RPAREN { MethodCall ($1, $3, []) }
| IDENTIFIER LPAREN RPAREN { FuncCall ($1, []) }
| expr OBJ_OPERATOR expr { MethodCall ($1, "_" ^ $2, [$3]) }

object_instantiation:
  CLASS_NAME LPAREN params RPAREN { ObjectInstantiation ($1, List.rev $3) }
| CLASS_NAME LPAREN RPAREN { ObjectInstantiation ($1, []) }

object_variable_access:
  expr PERIOD IDENTIFIER { { ova_expr = $1; ova_class_name = ""; ova_var_name = $3; ova_is_static = false; } }
| CLASS_NAME PERIOD IDENTIFIER { { ova_expr = NullExpr; ova_class_name = $1; ova_var_name =  $3; ova_is_static = true } }

array_access:
  IDENTIFIER LBRACKET expr RBRACKET { ($1, $3) }

array_literal:
  LBRACKET params RBRACKET { ArrayLiteral (List.rev $2) }
| LBRACKET RBRACKET { ArrayLiteral ([]) }

array_default:
  DEFAULT typ { DefaultArray ($2) }

typ:
  INT { Primitive Int }
| LONG { Primitive Long }
| FLOAT { Primitive Float }
| CHAR { Primitive Char }
| STRING { Primitive String }
| BOOLEAN { Primitive Bool }
| VOID { Primitive Void }
| CLASS_NAME { Class $1 }
| typ LBRACKET INT_LITERAL RBRACKET { Array ($1, $3) }

expr:
  INT_LITERAL { IntLiteral $1 }
| LONG_LITERAL { LongLiteral $1 }
| FLOAT_LITERAL { FloatLiteral $1 }
| CHAR_LITERAL { CharLiteral $1 }
| STRING_LITERAL { StringLiteral $1 }
| BOOLEAN_LITERAL { BoolLiteral $1 }
| IDENTIFIER { Id $1 }
| SELF { Self }
| NULL { NullExpr }
| func_call { Call $1 }
| object_instantiation { $1 }
| object_variable_access { ObjectVariableAccess $1 }
| array_access { ArrayAccess $1 }
| array_literal { $1 }
| array_default { $1 }
| LPAREN expr RPAREN { $2 }
| expr PLUS expr { Binop ($1, Plus, $3) }
| expr MINUS expr { Binop ($1, Subtract, $3) }
| expr TIMES expr { Binop ($1, Times, $3) }
| expr DIVIDE expr { Binop ($1, Divide, $3) }
| expr MODULO expr { Binop ($1, Modulo, $3) }
| MINUS expr %prec UNARY_MINUS { Unop (Neg, $2) }
| assign { Assign $1 }
| assign_update { Update $1 }
| expr DOUBLE_EQ expr { Binop ($1, DoubleEq, $3) }
| expr NOT_EQ expr { Unop (Not, Binop ($1, DoubleEq, $3)) }
| expr GT expr { Binop ($1, BoGT, $3) }
| expr LT expr { Binop ($1, BoLT, $3) }
| expr GTE expr { Binop ($1, BoGTE, $3) }
| expr LTE expr { Binop ($1, BoLTE, $3) }
| NOT expr { Unop (Not, $2) }
| expr OR expr { Binop ($1, BoOr, $3) }
| expr AND expr { Binop ($1, BoAnd, $3) }

