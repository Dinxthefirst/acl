module PBox = PrintBox

let prog_header_style = PBox.Style.fg_color PBox.Style.Green
let binop_style = PBox.Style.fg_color PBox.Style.Magenta
let expr_keyword_style = PBox.Style.fg_color PBox.Style.Green
let make_binop op = PBox.line_with_style binop_style op
let make_expr e = PBox.line_with_style expr_keyword_style e

let make_type t =
  PBox.line_with_style (PBox.Style.fg_color PBox.Style.Cyan) t
;;

let ident_info (Tast.Ident { id }) =
  PBox.line_with_style (PBox.Style.fg_color PBox.Style.Red) id
;;

let string_of_type (tp : Tast.tau) : string = Unparser.string_of_type tp

let type_info tp =
  PBox.line_with_style
    (PBox.Style.fg_color PBox.Style.Yellow)
    (string_of_type tp)
;;

let rec expr_derivation_node (e : Tast.program) =
  match e with
  | Int { int } ->
    PBox.hlist
      ~bars:false
      [ make_expr "IntLit: "; PBox.line (Int64.to_string int) ]
  | Bool { bool } ->
    PBox.hlist
      ~bars:false
      [ make_expr "BoolLit: "; PBox.line (string_of_bool bool) ]
  | Var { id; tp } ->
    PBox.tree
      (make_expr "Lval")
      [ lval_derivation_node id; type_derivation_node tp ]
  | Abs { x; e; tp } ->
    PBox.tree
      (make_expr "Abs")
      [ ident_info x; expr_derivation_node e; type_derivation_node tp ]
  | App { e1; e2; tp } ->
    PBox.tree
      (make_expr "App")
      [ expr_derivation_node e1
      ; expr_derivation_node e2
      ; type_derivation_node tp
      ]
  | Let { id; e1; e2; tp; _ } ->
    PBox.tree
      (make_expr "Let")
      [ ident_info id
      ; expr_derivation_node e1
      ; expr_derivation_node e2
      ; type_derivation_node tp
      ]

and lval_derivation_node ident =
  PBox.hlist ~bars:false [ make_expr "Var: "; ident_info ident ]

and type_derivation_node tp =
  PBox.hlist ~bars:false [ make_type "Type: "; type_info tp ]
;;

let program_to_tree prog : string =
  PrintBox_text.to_string
    (PBox.tree
       (PBox.line_with_style prog_header_style "Program")
       [ expr_derivation_node prog ])
;;

let program_to_tree_testing prog : string option =
  match prog with
  | Ok p ->
    Some
      (PrintBox_text.to_string
         (PBox.tree
            (PBox.line_with_style prog_header_style "Program")
            [ expr_derivation_node p ]))
  | _ ->
    Printf.printf "Cannot convert this program to a tree";
    None
;;
