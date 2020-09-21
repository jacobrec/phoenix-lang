let eval filebuf =
  let parsed = Parser.main Lexer.token filebuf in
  print_endline (String.concat "" (List.map Ast.string_of_stmt parsed));
  Eval.evaluate parsed

let eval_string str = 
  let filebuf = Lexing.from_string str in
  eval filebuf

let eval_file path = 
  let ch = open_in path in
  let filebuf = Lexing.from_channel ch in
  eval filebuf


let repl _ =
  eval_string "
               def a = 10;;
               defn b c = 10 * c;;
               b(a)
"
