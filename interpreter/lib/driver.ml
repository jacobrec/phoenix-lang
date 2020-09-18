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
   defn fib n = if n < 2 then n else fib(n-1) + fib(n-2) ;;
   def val = fib(10) : println(\"hi\") ;;
   defn main =
     println(\"Starting program\");
     def offset = 1;
     foreach(
       [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
       fn i = println(fib(offset + i))
     );
     println(\"Ending program\")
   ;;
   println(val)
"
