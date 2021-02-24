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
%token LPAREN RPAREN LBRACKET RBRACKET COLON PERIOD COMMA
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

/* Set precedence and associativity rules */
/* https://docs.python.org/3/reference/expressions.html#operator-precedence */
%right EQ PLUS_EQ MINUS_EQ TIMES_EQ DIVIDE_EQ
%left OR
%left AND
%left NOT
%left DOUBLE_EQ NOT_EQ GT LT GTE LTE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc UNARY_MINUS

%start program /* the entry point */
%type <Ast.program> program

%%

program:
  program_without_eof EOF {}

program_without_eof:
  program_without_eof stmt {}
| program_without_eof fdecl {}
| program_without_eof classdecl {}
| program_without_eof NEWLINE {}
| /* nothing */ {}

optional_fdecls:
  optional_fdecls fdecl {}
| /* nothing */ {}

fdecl:
  DEF IDENTIFIER LPAREN type_params RPAREN RETURNS TYPE COLON NEWLINE INDENT stmts DEDENT {}
| DEF IDENTIFIER LPAREN type_params RPAREN COLON NEWLINE INDENT stmts DEDENT {}
| DEF IDENTIFIER LPAREN RPAREN RETURNS TYPE COLON NEWLINE INDENT stmts DEDENT {}
| DEF IDENTIFIER LPAREN RPAREN COLON NEWLINE INDENT stmts DEDENT {}

stmts:
  stmt {}
| stmts stmt {}

stmt:
  expr NEWLINE {}
| RETURN expr NEWLINE {}
| if_stmt  {}
| loop {}

if_stmt:
  IF expr COLON NEWLINE INDENT stmts DEDENT {}
| IF expr COLON NEWLINE INDENT stmts DEDENT ELSE COLON NEWLINE INDENT stmts DEDENT {}
| IF expr COLON NEWLINE INDENT stmts DEDENT elif ELSE COLON NEWLINE INDENT stmts DEDENT {}
| IF expr COLON NEWLINE INDENT stmts DEDENT elif {}

elif:
  ELIF expr COLON NEWLINE INDENT stmts DEDENT {}
| elif ELIF expr COLON NEWLINE INDENT stmts DEDENT {}

loop:
  LOOP expr WHILE expr COLON NEWLINE INDENT stmts DEDENT {}
| LOOP WHILE expr COLON NEWLINE INDENT stmts DEDENT {}

type_params:  /* these are the method signature type */
  TYPE IDENTIFIER {}
| type_params COMMA TYPE IDENTIFIER {}

params: /* these are the params used to invoke a function */
  expr {}
| params COMMA expr {}

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
  IDENTIFIER PERIOD IDENTIFIER LPAREN params RPAREN {}
| SELF PERIOD IDENTIFIER LPAREN params RPAREN {}
| IDENTIFIER LPAREN params RPAREN {}
| IDENTIFIER PERIOD IDENTIFIER LPAREN RPAREN {}
| SELF PERIOD IDENTIFIER LPAREN RPAREN {}
| IDENTIFIER LPAREN RPAREN {}

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
  INT_LITERAL {}
| LONG_LITERAL {}
| FLOAT_LITERAL {}
| CHAR_LITERAL {}
| STRING_LITERAL {}
| BOOLEAN_LITERAL {}
| IDENTIFIER {}
| NULL {}
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
| MINUS expr %prec UNARY_MINUS {}
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

