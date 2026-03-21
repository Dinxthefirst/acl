module Lexer = Parsing.Lexer
module Parser = Parsing.Parser
module Semant = Semant.Robinson
module Pbox = Eval.Pbox_eval
module SO = Eval.Semobj
module Eval = Eval.Evaluator
module Tast = Asts.Tast

let init_lexer filename =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  filebuf.lex_curr_p <- { filebuf.lex_curr_p with pos_fname = filename };
  input, filebuf
;;

let parse file =
  let input, filebuf = init_lexer file in
  let lex_res =
    try Parser.prog Lexer.token filebuf with
    | Lexer.Err msg ->
      Printf.eprintf
        "Error in file %s at position %d:%d\n%s\n%!"
        filebuf.lex_start_p.pos_fname
        filebuf.lex_start_p.pos_lnum
        (filebuf.lex_start_p.pos_cnum - filebuf.lex_start_p.pos_bol)
        msg;
      exit 10
    | Parser.Error ->
      Printf.eprintf
        "%s:%d:%d - %d:%d syntax error: '%s'\n%!"
        filebuf.lex_start_p.pos_fname
        filebuf.lex_start_p.pos_lnum
        (filebuf.lex_start_p.pos_cnum - filebuf.lex_start_p.pos_bol)
        filebuf.lex_curr_p.pos_lnum
        (filebuf.lex_curr_p.pos_cnum - filebuf.lex_curr_p.pos_bol)
        (Lexing.lexeme filebuf);
      exit 20
  in
  close_in input;
  lex_res
;;

let semant prog =
  let tprog = Semant.infer_program prog in
  tprog
;;

let tree_of_SO prog = Pbox.result_to_tree prog
let eval (prog : Tast.program) : SO.result = prog |> Eval.eval_program

let () =
  Sys.argv.(1)
  |> parse
  |> semant
  |> eval
  |> tree_of_SO
  |> Printf.printf "%s\n%!"
;;
