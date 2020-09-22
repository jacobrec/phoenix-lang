open Interpreter

let () =
  let didFile = ref false in
  let onfilename f =
    didFile := true;
    Driver.parse_and_eval_file f in
  Driver.load_lib ();
  Arg.parse [] onfilename "";
  if !didFile then () else Driver.repl ()
