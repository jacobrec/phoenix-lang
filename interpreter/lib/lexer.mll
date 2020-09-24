{
  open Parser
  open Lexing
  exception SyntaxError of string



  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }


}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '@' '$' '?' '_']['a'-'z' 'A'-'Z' '0'-'9' '@' '_' '?' '!' '$']*

rule token = parse
| white { token lexbuf }
| newline { next_line lexbuf; token lexbuf }
| ['0'-'9']+ as i { INT (Int64.of_string i) }
| '"'  { STR (string (Buffer.create 100) lexbuf) } (* see below *)
| '\'' { CHAR (char_lit (Buffer.create 1) lexbuf) } (* see below *)
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
| "=>" { EQUAL_GREATER }
| '<'  { LESS }
| '>'  { GREATER }
| "&&" { AND_AND }
| "||" { OR_OR }
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
| id as i { ID i }
| eof { EOF }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and char_lit buf = parse (* use buf to build up result *)
| [^'\'' '\n' '\\']+  
            { Buffer.add_string buf @@ Lexing.lexeme lexbuf;
              char_lit buf lexbuf}
| '\n'      { Buffer.add_string buf @@ Lexing.lexeme lexbuf;
              Lexing.new_line lexbuf;
              char_lit buf lexbuf }
| '\\' '\''  { Buffer.add_char buf '\"'; char_lit buf lexbuf }
| '\\' 'n'   { Buffer.add_char buf '\n'; char_lit buf lexbuf }
| '\\' 't'   { Buffer.add_char buf '\t'; char_lit buf lexbuf }
| '\\'       { Buffer.add_char buf '\\'; char_lit buf lexbuf }

| '\''       { let s = Buffer.contents buf in 
                if 1 = String.length s then s 
                else raise (SyntaxError ("Multiple chars in char literal: " 
                                          ^ Lexing.lexeme lexbuf)) } (* return *)
| eof       { raise (SyntaxError ("Encountered EOF while in char literal: " ^ Lexing.lexeme lexbuf)) }
| _         { raise (SyntaxError ("Unexpected char inside char literal: " ^ Lexing.lexeme lexbuf)) }
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
| eof       { raise (SyntaxError ("Encountered EOF while in string: " ^ Lexing.lexeme lexbuf)) }
| _         { raise (SyntaxError ("Unexpected char inside string: " ^ Lexing.lexeme lexbuf)) }
and waste_comment = parse (* use buf to build up result *)
| '\n'      { token lexbuf }
| _         { waste_comment lexbuf }