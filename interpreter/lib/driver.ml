let parse_and_eval ?(loud=false) filebuf =
  let parsed = Parse_runner.parse ~loud filebuf in
  Eval.evaluate ~loud parsed

let parse_and_eval_string ?(loud=false) str = 
  let filebuf = Lexing.from_string str in
  parse_and_eval ~loud filebuf

let parse_and_eval_file path = 
  let ch = open_in path in
  let filebuf = Lexing.from_channel ch in
  parse_and_eval filebuf


let rec repl _ =
  print_string "> ";
  flush stdout;
  parse_and_eval_string ~loud:true (input_line stdin);
  repl ()
