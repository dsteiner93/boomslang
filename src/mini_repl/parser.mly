%{ open Ast %}

%token TAB NEWLINE POUND EOF
%token <string> NAME

%left TAB NEWLINE NAME POUND

%start program
%type <Ast.str> program

%%

program:
  program program { Concat($1, $2) }
| TAB { Lit("TAB") }
| NAME { Lit($1) }
| NEWLINE { Lit("NEWLINE") }
| POUND { Lit("POUND") }
