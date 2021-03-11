%{ open Ast %}

%token TAB NEWLINE POUND EOF
%token <string> NAME

%left TAB NEWLINE NAME POUND

%start program
%type <Ast.str> program

%start test
%type <token list> test

%%

test:
| seq EOF { $1 @ [EOF] }
| EOF { [EOF] }

seq:
| token { [$1] }
| token seq { $1::$2 }

token:
| TAB { TAB }
| NEWLINE { NEWLINE }
| POUND { POUND }
| NAME { NAME($1) }



program:
| program base { Concat($1, $2) }
| base { $1 } 

base:
| TAB { Lit("TAB") }
| NAME { Lit($1) }
| NEWLINE { Lit("NEWLINE") }
| POUND { Lit("POUND") }
