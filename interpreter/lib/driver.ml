let eval filebuf =
  Parser.main Lexer.token filebuf

let eval_string str = 
  let filebuf = Lexing.from_string str in
  eval filebuf

let eval_file path = 
  let ch = open_in path in
  let filebuf = Lexing.from_channel ch in
  eval filebuf


let repl _ =
  let parsed = eval_string "\"hel\\\"\\nlo\"" in
  print_endline (Ast.string_of_stmt (List.nth parsed 0))
