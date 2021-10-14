open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf
    outx
    "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
      Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
      None
  | Parser.Error    ->
      Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let ( let* ) = Option.bind

let () =
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
  try parse_with_error lexbuf |> Option.iter E.typecheck with
  | E.Statics.Type_error (ctx, exp, typ, expected_typ) ->
      Printf.printf
        "Type Error:\n  given:\n    %s : %s\n  expected:\n    %s : %s\n  in context:\n\n%s\n"
        (E.Exp.to_string exp)
        (E.Typ.to_string expected_typ)
        (E.Exp.to_string exp)
        (E.Typ.to_string typ)
        (E.Statics.Ctx.to_string ctx)
