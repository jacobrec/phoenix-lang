let parse ?(loud=false) filebuf = 
  let parsed = Parser.main Lexer.token filebuf in
  if loud then begin
  print_string "// Parsed to: ";
  print_endline (String.concat "" (List.map Ast.string_of_stmt parsed));
  end else ();
  parsed
