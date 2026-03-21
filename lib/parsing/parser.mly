%{
  open Asts.Ast
  module Pos = Asts.Position
%}


%token EOF

%token <int64> INT_LIT
%token <string> IDENT
%token <bool> BOOL_LIT

%token PLUS MINUS MUL DIV REM
%token NOT OR AND 
%token EQ NEQ LT LE GT GE


%token LPAREN "(" RPAREN ")"

%token ARROW "->"

%token LET IN


%start <program> prog
%type <expr> app atomic expr 
%type <ident list> list_ident

%nonassoc LET
%right ARROW
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left MUL DIV REM
%right UNOP


%%

%inline ident: IDENT {Ident {id = $1}}

%inline binop:
| PLUS {Plus}
| MINUS {Minus}
| MUL {Mul}
| DIV {Div}
| REM {Rem}
| NEQ {Neq} 
| EQ {Eq}
| LT {Lt}
| LE {Le}
| GT {Gt}
| GE {Ge}
| OR {Or}
| AND {And}

%inline unop:
| MINUS {Neg}
| NOT {Not}

list_ident:
  | {[]}
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
| LET id = ident vs = list_ident EQ e1 = expr IN e2 = expr %prec LET {Let {id; vs; e1; e2}}
| l = expr op = binop r = expr {BinOp {l; op; r}}
| op = unop expr = expr %prec UNOP {UnOp {op; expr}}
| app {$1}

prog:
| e = expr EOF {e}
