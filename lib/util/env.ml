module StringMap = Map.Make (String)

type 'a environment = { vars : 'a StringMap.t }

let init_env () = { vars = StringMap.empty }
let lookup_opt s env = StringMap.find_opt s env.vars
let insert s tp env = { vars = StringMap.add s tp env.vars }
let fold_env f env = StringMap.fold f env (init_env ())

let lookup s env =
  match lookup_opt s env with
  | None ->
    failwith (Printf.sprintf "Could not find %s in the environment." s)
  | Some f -> f
;;

let string_of_env f { vars } : string =
  "Env = {"
  ^ StringMap.fold
      (fun k v acc -> acc ^ Printf.sprintf "%s |-> %s; " k (f v))
      vars
      ""
  ^ "}\n"
;;

let pbox_of_env f { vars } = StringMap.fold f vars []
