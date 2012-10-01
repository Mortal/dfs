%{
open Ast
%}

%token NEWLINE COMMA RANGE ARROW EOF
%token <string> NODE
%start goal
%type <Ast.decl list> goal
%%

goal:
source_file EOF { $1 }

source_file:
  { [] }
| decl COMMA source_file { $1 :: $3 }
| decl NEWLINE NEWLINE* source_file { $1 :: $4 }

decl:
node RANGE node { NodeRange ($1, $3) }
| node ARROW node { Edge ($1, $3) }
| node { NodeRange ($1, $1) }

node:
NODE { int_of_string $1 }
