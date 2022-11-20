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

open E

let typecheck_with_error progn =
  try Statics.typecheck progn with
  | Statics.Unbound_var v ->
      Printf.printf "Unbound variable: %s\n" (Abt.Var.to_string v)
  | Statics.Type_error (ctx, exp, typ, expected_typ) ->
      Printf.printf
        {|Type Error:
  given:
    %s : %s
  expected:
    %s : %s
  in context:
%s
|}
        (Exp.to_string exp)
        (Typ.to_string expected_typ)
        (Exp.to_string exp)
        (Typ.to_string typ)
        (Statics.Ctx.to_string ctx)

let parse () =
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
  parse_with_error lexbuf

let eval progn =
  let () = typecheck_with_error progn in
  E.Dynamics.eval progn |> E.Exp.to_string |> print_endline

let () =
  match Sys.argv.(1) with
  | "typecheck" -> () |> parse |> Option.iter typecheck_with_error
  | "eval" -> () |> parse |> Option.iter eval
  | cmd ->
      Printf.eprintf "Invalid command: %s\n" cmd;
      exit 1
  | exception Invalid_argument _ ->
      Printf.printf "Valid commands: typecheck, eval\n";
      exit 1
