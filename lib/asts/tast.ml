type ident = Ident of { id : string }

type tyCon =
  | Int
  | Bool

type tyVar = string

type tau =
  | TyCon of tyCon
  | TyVar of tyVar
  | TFunc of
      { intp : tau
      ; outtp : tau
      }

type typeScheme =
  | Tau of tau
  | TPoly of
      { var : tyVar
      ; tps : typeScheme
      }

type expr =
  | Var of
      { id : ident
      ; tp : tau
      }
  | Int of { int : int64 }
  | Bool of { bool : bool }
  | Abs of
      { x : ident
      ; e : expr
      ; tp : tau
      }
  | App of
      { e1 : expr
      ; e2 : expr
      ; tp : tau
      }
  | Let of
      { id : ident
      ; vs : ident list
      ; e1 : expr
      ; e2 : expr
      ; tp : tau
      }
(* | LetRec of { id : ident ; vs : ident list ; e1 : expr ; e2 : expr ; tp :
   mu t1. t2} *)

type program = expr
