%{ open Ast %}

%token TAB NEWLINE INDENT EOF DEDENT NINDENT
%token <string> NAME
%token <int> NDEDENTN

%left TAB NEWLINE NAME INDENT NINDENT NDEDENTN DEDENT

%start program
%type <Ast.str> program

%start test
%type <(token * int) list> test

%%

test:
| seq EOF { $1 @ [(EOF, 0)] }
| EOF { [(EOF, 0)] }

seq:
| token { [($1, 0)] }
| NDEDENTN { [(DEDENT, $1)] }
| token seq { ($1, 0)::$2 }

token:
| TAB { TAB }
| NEWLINE { NEWLINE }
| NINDENT { NINDENT }
| NAME { NAME($1) }
| DEDENT { DEDENT }



program:
| program base { Concat($1, $2) }
| base { $1 } 

base:
| TAB { Lit("TAB") }
| NAME { Lit($1) }
| NEWLINE { Lit("NEWLINE") }
| DEDENT { Lit("DEDENT") }
| INDENT { Lit("INDENT") }
