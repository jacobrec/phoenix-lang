let eval ?(loud=false) filebuf =
  let parsed = Parser.main Lexer.token filebuf in
  if loud then begin
  print_string "// Parsed to: ";
  print_endline (String.concat "" (List.map Ast.string_of_stmt parsed));
  end else ();
  Eval.evaluate ~loud parsed

let eval_string ?(loud=false) str = 
  let filebuf = Lexing.from_string str in
  eval ~loud filebuf

let eval_file path = 
  let ch = open_in path in
  let filebuf = Lexing.from_channel ch in
  eval filebuf


let rec repl _ =
  print_string "> ";
  flush stdout;
  eval_string ~loud:true (input_line stdin);
  repl ()
(*
defn length l =
  if l then 1 + length(cdr(l)) else 0
;;
 
defn map f l = 
  if l then (f(car(l)) :: map(f, cdr(l))) else l
;;

defn foreach f l = 
  if l then (f(car(l)); foreach(f, cdr(l)); [||]) else l
;;

def a = [|1, 2, 3|];;
defn test x = 
  x + 1;;
foreach(println, a);;
*)
