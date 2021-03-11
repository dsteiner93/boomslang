%{ open Ast %}

%token TAB NEWLINE INDENT EOF DEDENT
%token <string> NAME
%token <int> NDEDENTN

%left TAB NEWLINE NAME INDENT NDEDENTN DEDENT

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
| NDEDENTN seq { (DEDENT, $1)::$2 }
| token seq { ($1, 0)::$2 }

token:
| TAB { TAB }
| NEWLINE { NEWLINE }
| NAME { NAME($1) }
| DEDENT { DEDENT }
| INDENT { INDENT }



program:
| program base { Concat($1, $2) }
| base { $1 } 

base:
| TAB { Lit("TAB") }
| NAME { Lit($1) }
| NEWLINE { Lit("NEWLINE") }
| DEDENT { Lit("DEDENT") }
| INDENT { Lit("INDENT") }
