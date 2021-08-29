%{
open Infer

let rec expr_app e = function
  | [e'] -> App (e, e')
  | h :: ((_ :: _) as t) -> expr_app (App (e, h)) t
  | _ -> failwith "Unreachable"
%}

%token <string> VAR
%token <int> INT
%token <bool> BOOL

%token RPAREN LPAREN
%token ARROW BACKSLASH EQUAL
%token LET IN
%token EOF

%start main
%type <Infer.expr> main
%%

main:
  | expr; EOF { $1 }
  ;

expr:
  | expr2 { $1 }
  | expr2; expr3 { expr_app $1 $2 }
  | BACKSLASH; VAR; ARROW; expr { Abs ($2, $4) }
  | LET; VAR; EQUAL; expr; IN; expr { Let ($2, $4, $6) }
  ;

expr2:
  | VAR { Var $1 }
  | lit { Lit $1 }
  | RPAREN; expr; LPAREN { $2 }
  ;

expr3:
  | expr2 { [$1] }
  | expr2; expr3 { $1 :: $2 }
  ;

lit:
  | INT { Int $1 }
  | BOOL { Bool $1 }
  ;
