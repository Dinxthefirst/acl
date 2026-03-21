module Tast = Asts.Tast
module SO = Semobj
module E = Util.Env

let eval_binop (op : Tast.binop) (v1 : SO.value) (v2 : SO.value) : SO.result =
  match v1, v2 with
  | Base (Int l), Base (Int r) ->
    (match op with
     | Tast.Plus -> Value (Base (Int (Int64.add l r)))
     | Tast.Minus -> Value (Base (Int (Int64.sub l r)))
     | Tast.Mul -> Value (Base (Int (Int64.mul l r)))
     | Tast.Div ->
       if r = 0L
       then Wrong "Division by zero"
       else Value (Base (Int (Int64.div l r)))
     | Tast.Rem ->
       if r = 0L
       then Wrong "Division by zero"
       else Value (Base (Int (Int64.rem l r)))
     | Tast.Eq -> Value (Base (Bool (l = r)))
     | Tast.Neq -> Value (Base (Bool (l <> r)))
     | Tast.Lt -> Value (Base (Bool (l < r)))
     | Tast.Le -> Value (Base (Bool (l <= r)))
     | Tast.Gt -> Value (Base (Bool (l > r)))
     | Tast.Ge -> Value (Base (Bool (l >= r)))
     | _ -> failwith "Unreachable")
  | Base (Bool l), Base (Bool r) ->
    (match op with
     | Tast.Or -> Value (Base (Bool (l || r)))
     | Tast.And -> Value (Base (Bool (l && r)))
     | Tast.Eq -> Value (Base (Bool (l = r)))
     | Tast.Neq -> Value (Base (Bool (l <> r)))
     | _ -> failwith "Unreachable")
  | _ ->
    let fail_text =
      Printf.sprintf
        "Both values is not of the same type: v1 = %s and v2 = %s"
        (Semobj.string_of_value v1)
        (Semobj.string_of_value v2)
    in
    Wrong fail_text
;;

let eval_unop (op : Tast.unop) (v : SO.value) : SO.result =
  match v with
  | Base (Int i) ->
    (match op with
     | Neg -> Value (Base (Int (Int64.neg i)))
     | Not -> Wrong "Expected a boolean when using not")
  | Base (Bool b) ->
    (match op with
     | Not -> Value (Base (Bool (not b)))
     | Neg -> Wrong "Expected a integer when using negation")
  | _ -> Wrong "wrong expr val"
;;

let rec eval (env : SO.value E.environment) (expr : Tast.expr) : SO.result =
  match expr with
  | Var { id = Ident { id }; _ } ->
    let v = env |> E.lookup_opt id in
    (match v with
     | None ->
       Wrong (Printf.sprintf "Cannot find variable %s in the environment" id)
     | Some id -> Value id)
  | Int { int } -> Value (Base (Int int))
  | Bool { bool } -> Value (Base (Bool bool))
  | Abs { x; e; _ } -> Value (Closure { x; e; env })
  | App { e1; e2; _ } ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match v1, v2 with
     | Value (Closure { x = Ident { id }; e; env = env' }), Value v ->
       let env'' = E.insert id v env' in
       eval env'' e
     | _ ->
       Printf.printf "t1 %s\n" (SO.string_of_result v1);
       Printf.printf "t2 %s\n" (SO.string_of_result v2);
       Wrong "Expected a closure, and the argument must not be wrong")
  | Let { id = Ident { id }; e1; e2; _ } ->
    (match eval env e1 with
     | Value v ->
       let env' = E.insert id v env in
       eval env' e2
     | _ ->
       Wrong (Printf.sprintf "Expected value from binding of let expression"))
  | BinOp { l; op; r; _ } ->
    let v1 = eval env l in
    let v2 = eval env r in
    (match v1, v2 with
     | Value v1, Value v2 -> eval_binop op v1 v2
     | _ -> Wrong "Expected values from binary operation expressions")
  | UnOp { op; expr; _ } ->
    let v = eval env expr in
    (match v with
     | Value v -> eval_unop op v
     | _ -> Wrong "Expected a value from unary operation expression")
;;

let eval_program (prog : Tast.program) : SO.result =
  let res = eval (E.init_env ()) prog in
  res
;;
