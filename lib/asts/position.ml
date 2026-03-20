module Lex = Lexing

type position =
  { start_pos : Lex.position
  ; end_pos : Lex.position
  }

let make_pos (start_pos, end_pos) = { start_pos; end_pos }
