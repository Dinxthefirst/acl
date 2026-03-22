module Tast = Asts.Tast
module Unparser = Asts.Unparser
module E = Util.Env
module StringMap = Map.Make (String)

(* Sematic Objects (Dynamic Semantics) *)
type basval =
  | Int of int64
  | Bool of bool
  | Done

type value =
  | Base of basval
  | Closure of
      { x : Tast.ident
      ; e : Tast.expr
      ; env : value E.environment
      }
  | List of { vs : value list }

type result =
  | Value of value
  | Wrong of string

let string_of_basval = function
  | Int int -> Int64.to_string int
  | Bool bool -> string_of_bool bool
  | Done -> "done"
;;

let rec string_of_value = function
  | Base b -> string_of_basval b
  | Closure { x = Ident { id = x }; e; env } ->
    "Closure { x = "
    ^ x
    ^ "; e = "
    ^ Unparser.string_of_expr e
    ^ "; "
    ^ E.string_of_env string_of_value env
    ^ " }"
  | List { vs } ->
    "[" ^ String.concat " ; " (List.map string_of_value vs) ^ "]"
;;

let string_of_result = function
  | Value v -> string_of_value v
  | Wrong s -> "Wrong: " ^ s
;;
