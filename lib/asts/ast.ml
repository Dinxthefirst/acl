module Pos = Position

type ident = Ident of { id : string }

type expr =
  | Var of ident
  | Int of { int : int64 }
  | Bool of { bool : bool }
  | Abs of
      { x : ident
      ; e : expr
      }
  | App of
      { e1 : expr
      ; e2 : expr
      }
  | Let of
      { id : ident
      ; vs : ident list
      ; e1 : expr
      ; e2 : expr
      }
(* | LetRec of { id : ident ; vs : ident list ; e1 : expr ; e2 : expr } *)

type program = expr
