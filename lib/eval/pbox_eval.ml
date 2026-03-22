module PBox = PrintBox
module SO = Semobj
module Tast = Asts.Tast
module Pbox = Asts.Pbox_ast
module E = Util.Env

let wrong_style = PBox.Style.fg_color PBox.Style.Red
let prog_header_style = PBox.Style.fg_color PBox.Style.Green
let binop_style = PBox.Style.fg_color PBox.Style.Magenta
let value_keyword_style = PBox.Style.fg_color PBox.Style.Green
let make_wrong e = PBox.line_with_style wrong_style e
let make_binop op = PBox.line_with_style binop_style op
let make_value e = PBox.line_with_style value_keyword_style e

let make_type t =
  PBox.line_with_style (PBox.Style.fg_color PBox.Style.Cyan) t
;;

let make_binding s =
  PBox.line_with_style (PBox.Style.fg_color PBox.Style.Magenta) s
;;

let ident_info (Tast.Ident { id }) =
  PBox.line_with_style (PBox.Style.fg_color PBox.Style.Red) id
;;

let valve_info v =
  PBox.line_with_style (PBox.Style.fg_color PBox.Style.Yellow) v
;;

let basval_to_node (b : SO.basval) =
  match b with
  | Int int ->
    PBox.hlist
      ~bars:false
      [ make_type "Int: "
      ; valve_info (Printf.sprintf "%s" (Int64.to_string int))
      ]
  | Bool bool ->
    PBox.hlist
      ~bars:false
      [ make_type "Bool: "
      ; valve_info (Printf.sprintf "%s" (string_of_bool bool))
      ]
  | Done -> PBox.hlist ~bars:false [ make_type "Done" ]
;;

let rec value_derivation_node (v : SO.value) =
  match v with
  | Base b -> PBox.tree (make_value "Baseval") [ basval_to_node b ]
  | Closure { x; e; env } ->
    PBox.tree
      (make_value "Closure")
      [ ident_info x; tast_env_derivation_node e; env_derivation_node env ]
  | List { vs } ->
    PBox.tree (make_value "List") (List.map value_derivation_node vs)

and binding s (v : SO.value) =
  PBox.hlist
    ~bars:false
    [ make_binding (Printf.sprintf "%s |-> " s); value_derivation_node v ]

and env_derivation_node (env : SO.value E.environment) =
  let env_tree = E.pbox_of_env (fun k v acc -> acc @ [ binding k v ]) env in
  PBox.tree (make_value "Env") env_tree

and tast_env_derivation_node (t : Tast.expr) = Pbox.expr_derivation_node t

let value_to_tree (v : SO.value) : string =
  match v with
  | Base (Int c) -> Int64.to_string c
  | Base (Bool b) -> string_of_bool b
  | _ ->
    PrintBox_text.to_string
      (PBox.tree
         (PBox.line_with_style prog_header_style "Value")
         [ value_derivation_node v ])
;;

let result_to_tree (res : SO.result) : string =
  match res with
  | Value v ->
    PrintBox_text.to_string
      (PBox.tree
         (PBox.line_with_style prog_header_style "Result")
         [ value_derivation_node v ])
  | Wrong e ->
    PrintBox_text.to_string
      (PBox.hlist ~bars:false [ make_wrong ("Wrong: \"" ^ e ^ "\"") ])
;;

let result_to_tree_testing (v : (SO.result, 'a) result) : string option =
  match v with
  | Ok v ->
    (match v with
     | SO.Value (Base (Int c)) -> Some (Int64.to_string c)
     | SO.Value (Base (Bool b)) -> Some (string_of_bool b)
     | Wrong e ->
       Some
         (PrintBox_text.to_string
            (PBox.hlist ~bars:false [ make_wrong ("Wrong: \"" ^ e ^ "\"") ]))
     | SO.Value v ->
       Some
         (PrintBox_text.to_string
            (PBox.tree
               (PBox.line_with_style prog_header_style "Result")
               [ value_derivation_node v ])))
  | _ ->
    Printf.printf "Cannot convert this result to a tree";
    None
;;
