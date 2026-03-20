module Tast = Asts.Tast
module Ast = Asts.Ast
module TE = Util.Env
module Sub = Substitution
module Unparser = Asts.Unparser

let ( @@@ ) s2 s1 = Sub.apply_subst s2 s1
let c = ref 0

let fresh_tyVar () =
  let t = !c in
  let () = c := t + 1 in
  Tast.TyVar ("t" ^ string_of_int (t + 1))
;;

(** 
    https://en.wikipedia.org/wiki/Unification_(computer_science)#Examples_of_syntactic_unification_of_first-order_terms
*)
let rec unify (tau1 : Tast.tau) (tau2 : Tast.tau) : Sub.subst =
  match tau1, tau2 with
  | _ when tau1 = tau2 -> Sub.id ()
  | Tast.TyVar alpha, _ when occurs tau2 alpha ->
    failwith "infinite type checking"
  | (Tast.TyVar _ as alpha), _ -> Sub.add_subst (Sub.id ()) alpha tau2
  | _, Tast.TyVar _ -> unify tau2 tau1
  | ( Tast.TFunc { intp = intp1; outtp = outtp1 }
    , Tast.TFunc { intp = intp2; outtp = outtp2 } ) ->
    let s1 = unify intp1 intp2 in
    let s2 = unify (Sub.apply s1 outtp1) (Sub.apply s1 outtp2) in
    s2 @@@ s1
  | _ ->
    failwith
      (Printf.sprintf
         "Error unifying %s with %s"
         (Unparser.string_of_type tau1)
         (Unparser.string_of_type tau2))

and occurs (tau : Tast.tau) (alpha : Tast.tyVar) : bool =
  match tau with
  | TyCon _ -> false
  | TyVar a -> a = alpha
  | TFunc { intp; outtp } -> occurs intp alpha || occurs outtp alpha
;;

let instantiate (scheme : Tast.typeScheme) : Tast.tau =
  let rec aux : Tast.typeScheme -> Sub.subst * Tast.tau = function
    | Tast.Tau tp -> Sub.id (), tp
    | Tast.TPoly { var; tps } ->
      let alpha = Tast.TyVar var in
      let beta = fresh_tyVar () in
      let s, tau = aux tps in
      let sub = Sub.add_subst (Sub.id ()) alpha beta in
      s @@@ sub, tau
  in
  let sub, tau = aux scheme in
  Sub.apply sub tau
;;

module StringMap = Map.Make (String)

module TyVarSet = Set.Make (struct
    type t = Tast.tyVar

    let compare = compare
  end)

let free_tyVar_of_tau (tau : Tast.tau) : TyVarSet.t =
  let rec aux acc t =
    match t with
    | Tast.TyCon _ -> acc
    | Tast.TyVar var -> TyVarSet.add var acc
    | Tast.TFunc { intp; outtp } ->
      TyVarSet.union (aux acc intp) (aux acc outtp)
  in
  aux TyVarSet.empty tau
;;

let free_tyVar_of_scheme (scheme : Tast.typeScheme) : TyVarSet.t =
  let rec aux acc scheme =
    match scheme with
    | Tast.Tau t -> free_tyVar_of_tau t
    | Tast.TPoly { var; tps } ->
      TyVarSet.union (TyVarSet.add var acc) (aux acc tps)
  in
  aux TyVarSet.empty scheme
;;

let tyVar_of_env (env : Tast.typeScheme TE.environment) =
  let { TE.vars } = env in
  StringMap.fold
    (fun _ scheme acc -> TyVarSet.union (free_tyVar_of_scheme scheme) acc)
    vars
    TyVarSet.empty
;;

let generalize (env : Tast.typeScheme TE.environment) (tau : Tast.tau)
  : Tast.typeScheme
  =
  let tyVar_of_tau = free_tyVar_of_tau tau in
  let tyVar_of_env = tyVar_of_env env in
  let tyVar_diff = TyVarSet.diff tyVar_of_tau tyVar_of_env in
  TyVarSet.fold
    (fun t acc -> Tast.TPoly { var = t; tps = acc })
    tyVar_diff
    (Tast.Tau tau)
;;

let is_syntactic_values (e : Tast.expr) : bool =
  match e with
  | Int _ | Bool _ | Var _ | Abs _ -> true
  | App _ | Let _ -> false
;;

let rec infer_type env (expr : Ast.expr) : Sub.subst * Tast.tau * Tast.expr =
  match expr with
  | Int { int } -> Sub.id (), Tast.TyCon Tast.Int, Tast.Int { int }
  | Bool { bool } -> Sub.id (), Tast.TyCon Tast.Bool, Tast.Bool { bool }
  | Var (Ident { id }) ->
    let tp = TE.lookup id env in
    let instantiation = instantiate tp in
    ( Sub.id ()
    , instantiation
    , Tast.Var { id = Ident { id }; tp = instantiation } )
  | Abs { x = Ident { id = xid; _ }; e } ->
    let t = fresh_tyVar () in
    let env' = TE.insert xid (Tast.Tau t) env in
    let s1, tau, e = infer_type env' e in
    let tp = Tast.TFunc { intp = Sub.apply s1 t; outtp = tau } in
    s1, tp, Tast.Abs { x = Ident { id = xid }; e; tp }
  | App { e1; e2 } ->
    let s1, tau1, e1 = infer_type env e1 in
    let s2, tau2, e2 = infer_type (Sub.apply_env s1 env) e2 in
    let alpha = fresh_tyVar () in
    let s3 = s2 @@@ s1 in
    let s4 =
      unify (Sub.apply s3 tau1) (Tast.TFunc { intp = tau2; outtp = alpha })
    in
    let s5 = s4 @@@ s3 in
    let tp = Sub.apply s5 alpha in
    s5, tp, Tast.App { e1; e2; tp }
  | Let { id = Ident { id }; vs; e1; e2 } ->
    let s1, tau1, e1 = infer_type env e1 in
    let env' = Sub.apply_env s1 env in
    let clos =
      if is_syntactic_values e1 then generalize env' tau1 else Tast.Tau tau1
    in
    let env'' = TE.insert id clos env' in
    let tvs = List.map (fun (Ast.Ident { id }) -> id) vs in
    let env''' =
      List.fold_left
        (fun envacc id ->
           let t = fresh_tyVar () in
           TE.insert id (Tast.Tau t) envacc)
        env''
        tvs
    in
    let vs =
      List.fold_left (fun acc id -> acc @ [ Tast.Ident { id } ]) [] tvs
    in
    let s2, tau2, e2 = infer_type (Sub.apply_env s1 env''') e2 in
    s2 @@@ s1, tau2, Tast.Let { id = Ident { id }; vs; e1; e2; tp = tau2 }
;;

let infer_program (expr : Ast.program) : Tast.program =
  let env = TE.init_env () in
  c := 0;
  let s, _, texpr = infer_type env expr in
  Sub.apply_expr s texpr
;;
