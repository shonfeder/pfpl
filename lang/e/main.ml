open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    Printf.printf "%s\n" (E.Exp.to_string value);
    parse_and_print lexbuf
  | None -> ()

let () =
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
  parse_and_print lexbuf
