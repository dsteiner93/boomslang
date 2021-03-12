%{ %}

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
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOLEAN_LITERAL
%token <string> TYPE
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
  program_without_eof EOF { $1 }

program_without_eof:
  program_without_eof stmt { { p_stmts=$2 :: $1.p_stmts; p_fdecls = $1.p_fdecls; p_classdecls=$1.p_classdecls } } 
| program_without_eof fdecl { { p_stmts=$1.p_stmts; p_fdecls = $2 :: $1.p_fdecls; p_classdecls=$1.p_classdecls } }
| program_without_eof classdecl { { p_stmts=$1.p_stmts; p_fdecls = $1.p_fdecls; p_classdecls=$2 :: $1.p_classdecls } }
| program_without_eof NEWLINE {}
| /* nothing */ { { p_stmts=[]; p_fdecls=[]; p_classdecls=[]; } }

optional_fdecls:
  optional_fdecls fdecl {}
| /* nothing */ {}

fdecl:
  DEF IDENTIFIER LPAREN type_params RPAREN RETURNS TYPE COLON NEWLINE INDENT stmts DEDENT {
    { fname = $2; formals = List.rev $4; typ = $8 ; body = List.rev $11} }
| DEF IDENTIFIER LPAREN type_params RPAREN COLON NEWLINE INDENT stmts DEDENT {}
| DEF IDENTIFIER LPAREN RPAREN RETURNS TYPE COLON NEWLINE INDENT stmts DEDENT {}
| DEF IDENTIFIER LPAREN RPAREN COLON NEWLINE INDENT stmts DEDENT {}
| DEF UNDERSCORE OBJ_OPERATOR LPAREN TYPE IDENTIFIER RPAREN RETURNS TYPE COLON NEWLINE INDENT stmts DEDENT {}

stmts:
  stmt { [$1] }
| stmts stmt { $2 :: $1 }

stmt:
  expr NEWLINE { Expr $1 }
| RETURN expr NEWLINE { Return $2 }
| if_stmt  { $1 }
| loop { $1 }

if_stmt:
  IF expr COLON NEWLINE INDENT stmts DEDENT { If($2, List.rev $6, [], []) }
| IF expr COLON NEWLINE INDENT stmts DEDENT ELSE COLON NEWLINE INDENT stmts DEDENT { If($2, List.rev $6, [], List.rev $12) }
| IF expr COLON NEWLINE INDENT stmts DEDENT elif ELSE COLON NEWLINE INDENT stmts DEDENT { If($2, List.rev $6, List.rev $8, List.rev $13) }
| IF expr COLON NEWLINE INDENT stmts DEDENT elif { If($2, List.rev $6, List.rev $8, []) }

elif:
  ELIF expr COLON NEWLINE INDENT stmts DEDENT { [($2, List.rev $6)] }
| elif ELIF expr COLON NEWLINE INDENT stmts DEDENT { ($3, List.rev $7) :: $1 }

loop:
  LOOP expr WHILE expr COLON NEWLINE INDENT stmts DEDENT { Loop($2, $4, List.rev $8) }
| LOOP WHILE expr COLON NEWLINE INDENT stmts DEDENT { Loop(NullExpr, $3, List.rev $7) }

type_params:  /* these are the method signature type */
  TYPE IDENTIFIER { $1 is the type, which is just a string. then we need to convert that to ast.ml typ object. $2 }
| type_params COMMA TYPE IDENTIFIER {}

params: /* these are the params used to invoke a function */
  expr { [$1] }
| params COMMA expr { $3 :: $1 }

classdecl:
  CLASS TYPE COLON NEWLINE
    INDENT STATIC COLON NEWLINE INDENT assigns NEWLINE
    DEDENT REQUIRED COLON NEWLINE INDENT vdecls NEWLINE
    DEDENT OPTIONAL COLON NEWLINE INDENT assigns NEWLINE
    DEDENT optional_fdecls NEWLINE {}

vdecls:
  vdecl {}
| vdecls NEWLINE vdecl {}

vdecl:
  TYPE IDENTIFIER {}

assigns:
  assign {}
| assigns NEWLINE assign {}

assign:
  TYPE IDENTIFIER EQ expr {}
| IDENTIFIER EQ expr {}
| object_variable_access EQ expr {}

assign_update:
  IDENTIFIER PLUS_EQ expr {}
| IDENTIFIER MINUS_EQ expr {}
| IDENTIFIER TIMES_EQ expr {}
| IDENTIFIER DIVIDE_EQ expr {}
| object_variable_access PLUS_EQ expr {}
| object_variable_access MINUS_EQ expr {}
| object_variable_access TIMES_EQ expr {}
| object_variable_access DIVIDE_EQ expr {}

func_call:
  IDENTIFIER PERIOD IDENTIFIER LPAREN params RPAREN { Call($1, $3, List.rev $5) }
| SELF PERIOD IDENTIFIER LPAREN params RPAREN { Call($3, List.rev $5) }
| IDENTIFIER LPAREN params RPAREN { Call($1, List.rev $3) }
| IDENTIFIER PERIOD IDENTIFIER LPAREN RPAREN { Call($1, $3) }
| SELF PERIOD IDENTIFIER LPAREN RPAREN { Call($3) }
| IDENTIFIER LPAREN RPAREN { Call($1) }

object_instantiation:
  TYPE LPAREN params RPAREN {}
| TYPE LPAREN RPAREN {}

object_variable_access:
  IDENTIFIER PERIOD IDENTIFIER {}
| SELF PERIOD IDENTIFIER {}

array_access:
  IDENTIFIER LBRACKET expr RBRACKET {}

array_literal:
  LBRACKET params RBRACKET {}
| LBRACKET RBRACKET {}

expr:
  INT_LITERAL { IntLiteral($1) }
| LONG_LITERAL { LongLiteral($1) }
| FLOAT_LITERAL { FloatLiteral($1) }
| CHAR_LITERAL { CharLiteral}
| STRING_LITERAL {}
| BOOLEAN_LITERAL {}
| IDENTIFIER {}
| NULL { NullExpr }
| func_call {}
| object_instantiation {}
| object_variable_access {}
| array_access {}
| array_literal {}
| LPAREN expr RPAREN {}
| expr PLUS expr {}
| expr MINUS expr {}
| expr TIMES expr {}
| expr DIVIDE expr {}
| expr MODULO expr {}
| expr OBJ_OPERATOR expr {}
| MINUS expr %prec UNARY_MINUS { Unop(Neg, $2) }
| assign {}
| assign_update {}
| expr DOUBLE_EQ expr {}
| expr NOT_EQ expr {}
| expr GT expr {}
| expr LT expr {}
| expr GTE expr {}
| expr LTE expr {}
| NOT expr {}
| expr OR expr {}
| expr AND expr {}
