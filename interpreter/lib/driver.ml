let parse_and_eval ?(loud=false) ?(filename="repl") ?(exit_on_error=true) filebuf =
  let parsed = Parse_runner.parse ~loud ~filename ~exit_on_error filebuf in
  match parsed with
  | Some parsed -> Eval.evaluate ~loud parsed
  | None -> ()


let parse_and_eval_string str = 
  let filebuf = Lexing.from_string str in
  parse_and_eval ~loud:true ~exit_on_error:false filebuf

let parse_and_eval_file path = 
  let ch = open_in path in
  let filebuf = Lexing.from_channel ch in
  parse_and_eval filebuf ~filename:path

let load_lib _ =
  parse_and_eval_file "/home/jacob/phoenix-lang/lib/core.phx"

let rec repl _ =
  print_string "> ";
  flush stdout;
  parse_and_eval_string (input_line stdin);
  repl ()
