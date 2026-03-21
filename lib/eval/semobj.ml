module Tast = Asts.Tast
module Unparser = Asts.Unparser
module E = Util.Env
module StringMap = Map.Make (String)

(* Sematic Objects (Dynamic Semantics) *)
type basval =
  | Int of int64
  | Bool of bool
  | Done

and value =
  | Base of basval
  | Closure of closure

and closure =
  { x : Tast.ident
  ; e : Tast.expr
  ; env : value E.environment
  }

and result =
  | Value of value
  | Wrong of string

let rec string_of_value = function
  | Base (Int int) -> Int64.to_string int
  | Base (Bool bool) -> string_of_bool bool
  | Closure { x = Ident { id = x }; e; env } ->
    "Closure { x = "
    ^ x
    ^ "; e = "
    ^ Unparser.string_of_expr e
    ^ "; "
    ^ E.string_of_env string_of_value env
    ^ " }"
  | Base Done -> "done"
;;

let string_of_result = function
  | Value v -> string_of_value v
  | Wrong s -> "Wrong: " ^ s
;;
