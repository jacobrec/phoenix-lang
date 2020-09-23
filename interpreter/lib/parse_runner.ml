open Lexing
exception SyntaxError of string

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.main Lexer.token lexbuf with
  | SyntaxError msg ->
     Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
     exit (-1)
  | Parser.Error ->
     Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
     exit (-1)

let parse ?(loud=false) ?(filename="repl") lexbuf = 
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let parsed = parse_with_error lexbuf in
  if loud then begin
  print_string "// Parsed to: ";
  print_endline (String.concat "" (List.map Ast.string_of_stmt parsed));
  end else ();
  parsed
