module Tast = Tast

let string_of_tyvar (tyvar : Tast.tyVar) = tyvar

let rec string_of_type (tp : Tast.tau) : string =
  match tp with
  | TyCon tyCon -> (match tyCon with Int -> "int" | Bool -> "bool")
  | TyVar tyVar -> string_of_tyvar tyVar
  | TFunc { intp; outtp } ->
    (match intp with
     | TFunc _ -> "(" ^ string_of_type intp ^ ")"
     | _ -> string_of_type intp)
    ^ " -> "
    ^ string_of_type outtp
;;

let rec string_of_scheme (scheme : Tast.typeScheme) : string =
  match scheme with
  | Tau t -> string_of_type t
  | TPoly { var; tps } -> string_of_tyvar var ^ "." ^ string_of_scheme tps
;;

let string_of_binop = function
  | Tast.Plus -> "+"
  | Tast.Minus -> "-"
  | Tast.Mul -> "*"
  | Tast.Div -> "-"
  | Tast.Rem -> "%"
  | Tast.Or -> "or"
  | Tast.And -> "and"
  | Tast.Eq -> "=="
  | Tast.Neq -> "!="
  | Tast.Lt -> "<"
  | Tast.Le -> "<="
  | Tast.Gt -> ">"
  | Tast.Ge -> ">="
;;

let string_of_unop = function Tast.Neg -> "-" | Tast.Not -> "not "

let rec string_of_expr (texpr : Tast.expr) : string =
  match texpr with
  | Int { int } -> Int64.to_string int
  | Bool { bool } -> string_of_bool bool
  | Var { id = Ident { id }; _ } -> id
  | Abs { x = Ident { id }; e; tp } ->
    "\\" ^ id ^ " : " ^ string_of_type tp ^ " -> " ^ string_of_expr e
  | App { e1; e2; tp } ->
    "("
    ^ string_of_expr e1
    ^ " "
    ^ string_of_expr e2
    ^ " : "
    ^ string_of_type tp
    ^ ")"
  | Let { id = Ident { id }; e1; e2; tp } ->
    "("
    ^ "let "
    ^ id
    ^ " : "
    ^ string_of_type tp
    ^ " = "
    ^ string_of_expr e1
    ^ " in "
    ^ string_of_expr e2
    ^ ")"
  | BinOp { l; op; r; _ } ->
    "("
    ^ string_of_expr l
    ^ " "
    ^ string_of_binop op
    ^ " "
    ^ string_of_expr r
    ^ ")"
  | UnOp { op; expr; _ } ->
    "(" ^ string_of_unop op ^ string_of_expr expr ^ ")"
;;
