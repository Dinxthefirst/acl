{
  exception Err of string
  open Parser
}

let boolean = "true" | "false"
let digit = ['0'-'9']
let integer = ('0' | ['1'-'9']digit*)
let letter = ['a'-'z' 'A'-'Z']
let id = (letter | '_') (letter | digit | '_')* 

rule token = parse
| eof {EOF}
| [' ' '\t'] {token lexbuf}
| '\n' | "\r\n" {Lexing.new_line lexbuf; token lexbuf}
| '+'   {PLUS}
| '-'   {MINUS}
| '*'   {MUL}
| '/'   {DIV}
| "mod" {REM}
| "<>" {NEQ}
| "=" {EQ}
| '<' {LT}
| "<=" {LE}
| '>' {GT}
| ">=" {GE}
| "not" {NOT}
| "or" {OR}
| "and" {AND}
| '(' {LPAREN}
| ')' {RPAREN}
| "let" {LET}
| "in" {IN}
(* | "rec" {REC} *)
| "\\" {LAMBDA}
| "->" {ARROW}
| "::" {CONS}
| "[" {LBRACKET}
| "]" {RBRACKET}
| ";" {SEQ}
| boolean as b {BOOL_LIT (bool_of_string b)}
| integer as i {INT_LIT (Int64.of_string i) }
| id as i {IDENT i} 
| _ as e {raise (Err (Format.sprintf ("Unexpected symbol '%c' found.") e))}
