%{
  open Asts.Ast
  module Pos = Asts.Position
%}


%token EOF

%token <int64> INT_LIT
%token <string> IDENT
%token <bool> BOOL_LIT

%token EQ

%token LPAREN "(" RPAREN ")"

%token ARROW "->"

%token LET IN


%start <program> prog
%type <expr> app atomic expr
%type <ident list> list_ident


%%

%inline ident: IDENT {Ident {id = $1}}

list_ident:
  | /* empty */ {[]}
  | id = ident ids = list_ident {id :: ids}

atomic:
| "(" e = expr ")" {e}
| i = INT_LIT {Int {int = i}}
| b = BOOL_LIT {Bool {bool = b}}
| var = ident {Var (var)}

app:
| e1 = app e2 = atomic {App {e1; e2}}
| atomic {$1}

expr: 
| x = ident "->" e = expr {Abs {x; e}}
| LET id = ident vs = list_ident EQ e1 = expr IN e2 = expr {Let {id; vs; e1; e2}}
| app {$1}

prog:
| e = expr EOF {e}
