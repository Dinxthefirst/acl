let string_of_ident (Ident { id } : Tast.ident) : string =
  "Ident { id = " ^ id ^ " }"
;;

let string_of_tycon : Tast.tyCon -> string = function
  | Int -> "Int"
  | Bool -> "Bool"
;;

let rec string_of_tau : Tast.tau -> string = function
  | TyCon tp -> "TyCon ( " ^ string_of_tycon tp ^ " )"
  | TyVar tp -> "TyVar ( " ^ tp ^ " )"
  | TFunc { intp; outtp } ->
    "TFunc { intp = "
    ^ string_of_tau intp
    ^ " ; outtp = "
    ^ string_of_tau outtp
    ^ " }"
  | TList { tp } -> "TList { tp = " ^ string_of_tau tp ^ " }"
;;

let rec string_of_typescheme : Tast.typeScheme -> string = function
  | Tau tp -> "Tau ( " ^ string_of_tau tp ^ " )"
  | TPoly { var; tps } ->
    "TPoly { var = " ^ var ^ " ; tps = " ^ string_of_typescheme tps ^ " }"
;;

let string_of_binop : Tast.binop -> string = function
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Mul -> "Mul"
  | Div -> "Div"
  | Rem -> "Rem"
  | And -> "And"
  | Or -> "Or"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
;;

let string_of_unop : Tast.unop -> string = function
  | Neg -> "Neg"
  | Not -> "Not"
;;

let rec string_of_expr : Tast.expr -> string = function
  | Var { id; tp } ->
    "Var { id = " ^ string_of_ident id ^ " ; tp = " ^ string_of_tau tp ^ " }"
  | Int { int } -> "Int { int = " ^ Int64.to_string int ^ " }"
  | Bool { bool } -> "Bool { bool = " ^ string_of_bool bool ^ " }"
  | Abs { x; e; tp } ->
    "Abs { x = "
    ^ string_of_ident x
    ^ " ; e = "
    ^ string_of_expr e
    ^ " ; tp = "
    ^ string_of_tau tp
    ^ " }"
  | App { e1; e2; tp } ->
    "App { e1 = "
    ^ string_of_expr e1
    ^ " ; e2 = "
    ^ string_of_expr e2
    ^ " ; tp = "
    ^ string_of_tau tp
    ^ " }"
  | Let { id; e1; e2; tp } ->
    "Let { id = "
    ^ string_of_ident id
    ^ " ; e1 = "
    ^ string_of_expr e1
    ^ " ; e2 = "
    ^ string_of_expr e2
    ^ " ; tp = "
    ^ string_of_tau tp
    ^ " }"
  | BinOp { l; op; r; tp } ->
    "BinOp { l = "
    ^ string_of_expr l
    ^ " ; op = "
    ^ string_of_binop op
    ^ " ; r = "
    ^ string_of_expr r
    ^ " ; tp = "
    ^ string_of_tau tp
    ^ " }"
  | UnOp { op; expr; tp } ->
    "UnOp { op = "
    ^ string_of_unop op
    ^ " ; expr = "
    ^ string_of_expr expr
    ^ " ; tp = "
    ^ string_of_tau tp
;;

let string_of_program : Tast.program -> string = string_of_expr
