%{ open Ast %}

/* Boolean operators */
%token NOT OR AND
/* Loops and conditionals */
%token LOOP WHILE IF ELIF ELSE
/* Named literals */
%token NULL TRUE FALSE
/* Words related to functions and classes */
%token DEF CLASS CONSTRUCT RETURN RETURNS SELF REQUIRED OPTIONAL STATIC
/* Mathematical operations */
%token PLUS MINUS TIMES DIVIDE MODULO
/* Assignment operators */
%token EQ PLUS_EQ MINUS_EQ TIMES_EQ DIVIDE_EQ
/* Comparison operators */
%token DOUBLE_EQ NOT_EQ GT LT GTE LTE
/* Misc. */
%token LPAREN RPAREN LBRACKET RBRACKET COLON EOF
/* Parameterized tokens */
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <string> TYPE
%token <string> IDENTIFIER

/* TODO define complete grammar below */
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
