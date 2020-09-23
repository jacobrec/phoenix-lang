open Lexing
exception SyntaxError of string

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error exit_on_error lexbuf =
  try Some (Parser.main Lexer.token lexbuf) with
  | SyntaxError msg ->
     Printf.printf "%a: %s\n" print_position lexbuf msg;
     if exit_on_error then exit (-1) else None
  | Parser.Error ->
     Printf.printf "%a: syntax error\n" print_position lexbuf;
     if exit_on_error then exit (-1) else None

let parse ?(loud=false) ?(filename="repl") ?(exit_on_error=true) lexbuf = 
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let parsed = parse_with_error exit_on_error lexbuf in
  if loud then begin
  print_string "// Parsed to: ";
  match parsed with
  | Some parsed -> print_endline (String.concat "" (List.map Ast.string_of_stmt parsed))
  | None -> print_endline "error"
  end else ();
  parsed
