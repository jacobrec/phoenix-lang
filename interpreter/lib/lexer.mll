{
  open Parser
  exception Error of string
}

rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
| ['0'-'9']+ as i { INT (int_of_string i) }
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { TIMES }
| '/'  { DIV }
| '%'  { MOD }
| '('  { LPAREN }
| ')'  { RPAREN }
| "==" { EQUAL_EQUAL }
| "<=" { LESS_EQUAL }
| ">=" { GREATER_EQUAL }
| '<'  { LESS }
| '>'  { GREATER }
| "::" { COLON_COLON }
| ":=" { COLON_EQUAL }
| ':'  { COLON }
| ';'  { SEMICOLON }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }