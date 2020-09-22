{
  open Parser
  exception Error of string

  let position lexbuf =
    let p = lexbuf.Lexing.lex_curr_p in
        Printf.sprintf "%s:%d:%d" 
        p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)
        
  let error lexbuf fmt = 
      Printf.kprintf (fun msg -> 
          raise (Error ((position lexbuf)^" "^msg))) fmt

  let errors lexbuf str = 
          raise (Error ((position lexbuf)^" "^str))

}

rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
| ['0'-'9']+ as i { INT (Int64.of_string i) }
| '"'  { STR (string (Buffer.create 100) lexbuf) } (* see below *)
| '#'  { waste_comment lexbuf } (* Comment, read til EOL *)
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { TIMES }
| '/'  { DIV }
| '%'  { MOD }
| '!'  { BANG }
| '('  { LPAREN }
| ')'  { RPAREN }
| '['  { LBRACK }
| ']'  { RBRACK }
| "[|" { LBRACK_PIPE }
| "|]" { RBRACK_PIPE }
| "{"  { LBRACE }
| "}"  { RBRACE }
| "==" { EQUAL_EQUAL }
| "<=" { LESS_EQUAL }
| ">=" { GREATER_EQUAL }
| '<'  { LESS }
| '>'  { GREATER }
| ";;" { SEMI_SEMI }
| "::" { COLON_COLON }
| ":=" { COLON_EQUAL }
| '='  { EQUAL }
| ':'  { COLON }
| ';'  { SEMICOLON }
| ','  { COMMA }
| "true"  { TRUE }
| "false" { FALSE }
| "if"    { IF }
| "then"  { THEN }
| "else"  { ELSE }
| "def"   { DEF }
| "defn"  { DEFN }
| "fn"    { FN }
| ['a'-'z' 'A'-'Z' '@' '$']['a'-'z' 'A'-'Z' '0'-'9' '@' '_' '?' '$']* as i { ID i }
| eof { EOF }
| _ { errors lexbuf ("unexpected character: [" ^ (Lexing.lexeme lexbuf) ^ "]") }

and string buf = parse (* use buf to build up result *)
| [^'"' '\n' '\\']+  
            { Buffer.add_string buf @@ Lexing.lexeme lexbuf;
              string buf lexbuf}
| '\n'      { Buffer.add_string buf @@ Lexing.lexeme lexbuf;
              Lexing.new_line lexbuf;
              string buf lexbuf }
| '\\' '"'  { Buffer.add_char buf '"'; string buf lexbuf }
| '\\' 'n'  { Buffer.add_char buf '\n'; string buf lexbuf }
| '\\' 't'  { Buffer.add_char buf '\t'; string buf lexbuf }
| '\\'      { Buffer.add_char buf '\\'; string buf lexbuf }

| '"'       { Buffer.contents buf } (* return *)
| eof       { error lexbuf "end of input inside of a string" }
| _         { error lexbuf 
                "found '%s' - don't know how to handle" @@ Lexing.lexeme lexbuf }
and waste_comment = parse (* use buf to build up result *)
| '\n'      { token lexbuf }
| _         { waste_comment lexbuf }