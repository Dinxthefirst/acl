module Pos = Position

type ident = Ident of { id : string }

type binop =
  | Plus
  | Minus
  | Mul
  | Div
  | Rem
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge

type unop =
  | Neg
  | Not

type expr =
  | Var of ident
  | Int of { int : int64 }
  | Bool of { bool : bool }
  | Abs of
      { x : ident
      ; vs : ident list
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
  | BinOp of
      { l : expr
      ; op : binop
      ; r : expr
      }
  | UnOp of
      { op : unop
      ; expr : expr
      }

(* | LetRec of { id : ident ; vs : ident list ; e1 : expr ; e2 : expr } *)

type program = expr
