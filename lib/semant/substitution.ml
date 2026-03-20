module Tast = Asts.Tast
module TE = Util.Env

exception SubstitutionError of string

module SubstMap = Map.Make (struct
    type t = Tast.tau

    let compare = compare
  end)

type subst = Tast.tau SubstMap.t

let id () : subst = SubstMap.empty

let add_subst (sub : subst) (tyvar : Tast.tau) (tau : Tast.tau) : subst =
  SubstMap.add tyvar tau sub
;;

let rec apply (sub : subst) (tau : Tast.tau) : Tast.tau =
  match tau with
  | Tast.TyVar _ ->
    (match SubstMap.find_opt tau sub with Some t -> t | None -> tau)
  | Tast.TFunc { intp; outtp } ->
    Tast.TFunc { intp = apply sub intp; outtp = apply sub outtp }
  | Tast.TyCon tycon -> Tast.TyCon tycon
;;

let rec apply_expr (sub : subst) (expr : Tast.expr) : Tast.expr =
  match expr with
  | Int _ | Bool _ -> expr
  | Var { id; tp } -> Var { id; tp = apply sub tp }
  | Abs { x; e; tp } -> Abs { x; e = apply_expr sub e; tp = apply sub tp }
  | App { e1; e2; tp } ->
    App { e1 = apply_expr sub e1; e2 = apply_expr sub e2; tp = apply sub tp }
  | Let { id; vs; e1; e2; tp } ->
    Let
      { id
      ; vs
      ; e1 = apply_expr sub e1
      ; e2 = apply_expr sub e2
      ; tp = apply sub tp
      }
;;

module TyVarSet = Set.Make (struct
    type t = Tast.tyVar

    let compare = compare
  end)

let apply_sigma (sub : subst) (type_scheme : Tast.typeScheme)
  : Tast.typeScheme
  =
  let rec aux acc = function
    | Tast.Tau tau ->
      let tau1 = apply sub tau in
      let tps1 = Tast.Tau tau1 in
      TyVarSet.fold (fun var tps -> Tast.TPoly { var; tps }) acc tps1
    | Tast.TPoly { var; tps } ->
      let sbound = apply sub (Tast.TyVar var) in
      (match sbound with
       | Tast.TyVar tyvar -> aux (TyVarSet.add tyvar acc) tps
       | _ -> aux acc tps)
  in
  aux TyVarSet.empty type_scheme
;;

let apply_env (sub : subst) (env : Tast.typeScheme TE.environment)
  : Tast.typeScheme TE.environment
  =
  let TE.{ vars } = env in
  TE.fold_env
    (fun id tps env' -> TE.insert id (apply_sigma sub tps) env')
    vars
;;

let apply_subst (sub2 : subst) (sub1 : subst) : subst =
  SubstMap.merge
    (fun _ s1 s2 ->
       match s1, s2 with
       | Some x, _ -> Some (apply sub2 x) (*both in s1 and s2*)
       | _, Some y -> Some y
       | None, None -> None)
    sub1
    sub2
;;
