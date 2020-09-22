open Interpreter

let () =
  let didFile = ref false in
  let onfilename f =
    didFile := true;
    Driver.eval_file f in
  Arg.parse [] onfilename "";
  if !didFile then () else Driver.repl ()
