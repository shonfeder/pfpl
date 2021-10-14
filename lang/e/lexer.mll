{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let ws = [ ' ' '\t']+
let nl = '\n' | '\r' | "\r\n"
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | ws     { read lexbuf }
  | nl     {next_line lexbuf; read lexbuf}
  | int    { NUM (int_of_string (lexeme lexbuf)) }
  | '"'    { read_str (Buffer.create 17) lexbuf }
  | "("    { OPEN_PAREN }
  | ")"    { CLOSE_PAREN }
  | ":"    { ANNOT }
  | "let"  { LET }
  | "in"   { IN }
  | "="    { EQ }
  | "+"    { PLUS }
  | "*"    { TIMES }
  | "++"   { CAT }
  | "len"  { LEN }
  | "Num"  { NUM_T }
  | "Str"  { STR_T }
  | id     { VAR (lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof    { EOF }

and read_str buf = parse
  | '"'       { STR (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'    ; read_str buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'   ; read_str buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'   ; read_str buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012' ; read_str buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'   ; read_str buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'   ; read_str buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'   ; read_str buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_str buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
