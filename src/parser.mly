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
%nonassoc UNARY_MINUS

%start program /* the entry point */
%type <Ast.program> program

%%

program:
  program_without_eof EOF { { p_stmts = List.rev $1.p_stmts; p_fdecls = List.rev $1.p_fdecls; p_classdecls = List.rev $1.p_classdecls; } }

program_without_eof:
  program_without_eof stmt { { p_stmts = $2::$1.p_stmts; p_fdecls = $1.p_fdecls; p_classdecls = $1.p_classdecls; } }
| program_without_eof fdecl { { p_stmts = $1.p_stmts; p_fdecls = $2::$1.p_fdecls; p_classdecls = $1.p_classdecls; } }
| program_without_eof classdecl { { p_stmts = $1.p_stmts; p_fdecls = $1.p_fdecls; p_classdecls = $2::$1.p_classdecls; } }
| program_without_eof NEWLINE { $1 }
| /* nothing */ { { p_stmts = []; p_fdecls = []; p_classdecls = []; } }

stmts:
  { [] }
| stmts stmt { $2 :: $1 }

stmt:
  expr NEWLINE { Expr $1 }
| RETURN expr NEWLINE { Return $2 }
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
| DEF UNDERSCORE OBJ_OPERATOR LPAREN typ IDENTIFIER RPAREN RETURNS typ COLON NEWLINE INDENT stmts DEDENT { {rtype = $9; fname = $3; formals = [($5, $6)]; body = List.rev $13} }

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
| object_variable_access EQ expr { ObjectVariableAssign ($1, $3) }

assign_update:
  IDENTIFIER EQ expr { RegularUpdate ($1, Eq, $3) }
| IDENTIFIER PLUS_EQ expr { RegularUpdate ($1, PlusEq, $3) }
| IDENTIFIER MINUS_EQ expr { RegularUpdate ($1, MinusEq, $3) }
| IDENTIFIER TIMES_EQ expr { RegularUpdate ($1, TimesEq, $3) }
| IDENTIFIER DIVIDE_EQ expr { RegularUpdate ($1, DivideEq, $3) }
| object_variable_access PLUS_EQ expr { ObjectVariableUpdate ($1, PlusEq, $3) }
| object_variable_access MINUS_EQ expr { ObjectVariableUpdate ($1, MinusEq, $3) }
| object_variable_access TIMES_EQ expr { ObjectVariableUpdate ($1, TimesEq, $3) }
| object_variable_access DIVIDE_EQ expr { ObjectVariableUpdate ($1, DivideEq, $3) }

func_call:
  IDENTIFIER PERIOD IDENTIFIER LPAREN params RPAREN { MethodCall ($1, $3, List.rev $5) }
| SELF PERIOD IDENTIFIER LPAREN params RPAREN { MethodCall ("self", $3, List.rev $5) }
| IDENTIFIER LPAREN params RPAREN { FuncCall ($1, List.rev $3) }
| IDENTIFIER PERIOD IDENTIFIER LPAREN RPAREN { MethodCall ($1, $3, []) }
| SELF PERIOD IDENTIFIER LPAREN RPAREN { MethodCall ("self", $3, []) }
| IDENTIFIER LPAREN RPAREN { FuncCall ($1, []) }

object_instantiation:
  CLASS_NAME LPAREN params RPAREN { ObjectInstantiation ($1, List.rev $3) }
| CLASS_NAME LPAREN RPAREN { ObjectInstantiation ($1, []) }

object_variable_access:
  IDENTIFIER PERIOD IDENTIFIER { ($1, $3) }
| SELF PERIOD IDENTIFIER { ("self", $3) }

array_access:
  IDENTIFIER LBRACKET expr RBRACKET { ArrayAccess ($1, $3)}

array_literal:
  LBRACKET params RBRACKET { ArrayLiteral (List.rev $2) }
| LBRACKET RBRACKET { ArrayLiteral ([]) }

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
| typ LBRACKET RBRACKET { Array ($1, -1) }

expr:
  INT_LITERAL { IntLiteral $1 }
| LONG_LITERAL { LongLiteral $1 }
| FLOAT_LITERAL { FloatLiteral $1 }
| CHAR_LITERAL { CharLiteral $1 }
| STRING_LITERAL { StringLiteral $1 }
| BOOLEAN_LITERAL { BoolLiteral $1 }
| IDENTIFIER { Id $1 }
| NULL { NullExpr }
| func_call { Call $1 }
| object_instantiation { $1 }
| object_variable_access { ObjectVariableAccess $1 }
| array_access { $1 }
| array_literal { $1 }
| LPAREN expr RPAREN { Paren $2 }
| expr PLUS expr { Binop ($1, Plus, $3) }
| expr MINUS expr { Binop ($1, Subtract, $3) }
| expr TIMES expr { Binop ($1, Times, $3) }
| expr DIVIDE expr { Binop ($1, Divide, $3) }
| expr MODULO expr { Binop ($1, Modulo, $3) }
| expr OBJ_OPERATOR expr { Binop ($1, ObjOperator, $3) }
| MINUS expr %prec UNARY_MINUS { Unop (Neg, $2) }
| assign { Assign $1 }
| assign_update { Update $1 }
| expr DOUBLE_EQ expr { Binop ($1, DoubleEq, $3) }
| expr NOT_EQ expr { Binop ($1, NotEq, $3) }
| expr GT expr { Binop ($1, BoGT, $3) }
| expr LT expr { Binop ($1, BoLT, $3) }
| expr GTE expr { Binop ($1, BoGTE, $3) }
| expr LTE expr { Binop ($1, BoLTE, $3) }
| NOT expr { Unop (Not, $2) }
| expr OR expr { Binop ($1, BoOr, $3) }
| expr AND expr { Binop ($1, BoAnd, $3) }

