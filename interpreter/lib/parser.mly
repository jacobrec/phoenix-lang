%token <int64> INT
%token <string> STR
%token <string> CHAR
%token <string> ID
%token PLUS MINUS TIMES DIV MOD
%token LPAREN RPAREN LBRACK RBRACK LBRACK_PIPE RBRACK_PIPE LBRACE RBRACE
%token TRUE FALSE
%token EQUAL_GREATER

%token AND_AND OR_OR

%token LESS GREATER LESS_EQUAL GREATER_EQUAL EQUAL_EQUAL EQUAL 
%token COLON_COLON COLON_EQUAL SEMICOLON COLON COMMA SEMI_SEMI

%token DEF DEFN FN
%token IF THEN ELSE

%token BANG

%token EOF
/* Low Precedence */
%right SEMICOLON COLON
%right EQUAL
%right COLON_EQUAL
%right ELSE
%left  OR_OR
%left  AND_AND
%left  EQUAL_EQUAL LESS_EQUAL GREATER_EQUAL
%left  LESS GREATER
%right COLON_COLON
%left  PLUS MINUS
%left  TIMES DIV MOD
%left  BANG
/* High Precedence */

%start <stmt list> main
%{ open Ast %}

%%


main:
  | stmt = stmt EOF { [stmt] }
  | stmt = stmt m = main { stmt :: m }

stmt:
  | stmt = statement SEMI_SEMI { stmt }

statement:
  | expr = expression { ExprStmt expr } // expr stmt


expression:
  | e = literal { LitExpr e }
  | LPAREN e = expression RPAREN  { e }

  | BANG e = expression { UniExpr (OpNot, e) }
  | MINUS e = expression { UniExpr (OpNegate, e) }

  | e1 = expression TIMES         e2 = expression { BinExpr (OpTimes, e1, e2) }
  | e1 = expression DIV           e2 = expression { BinExpr (OpDiv, e1, e2) }
  | e1 = expression MOD           e2 = expression { BinExpr (OpMod, e1, e2) }
  | e1 = expression PLUS          e2 = expression { BinExpr (OpPlus, e1, e2) }
  | e1 = expression MINUS         e2 = expression { BinExpr (OpMinus, e1, e2) }
  | e1 = expression LESS          e2 = expression { BinExpr (OpLess, e1, e2) }
  | e1 = expression GREATER       e2 = expression { BinExpr (OpGreater, e1, e2) }
  | e1 = expression LESS_EQUAL    e2 = expression { BinExpr (OpLessEqual, e1, e2) }
  | e1 = expression GREATER_EQUAL e2 = expression { BinExpr (OpGreaterEqual, e1, e2) }
  | e1 = expression EQUAL_EQUAL   e2 = expression { BinExpr (OpEqualEqual, e1, e2) }
  | e1 = expression AND_AND       e2 = expression { ShortBinExpr (OpAnd, e1, e2) }
  | e1 = expression OR_OR         e2 = expression { ShortBinExpr (OpOr, e1, e2) }
  | e1 = expression COLON_COLON   e2 = expression { BinExpr (OpCons, e1, e2) }
  | e1 = expression COLON_EQUAL   e2 = expression { BinExpr (OpAssign, e1, e2) }
  | e1 = expression COLON         e2 = expression { BinExpr (OpColon, e1, e2) }
  | e1 = expression SEMICOLON     e2 = expression { BinExpr (OpSemicolon, e1, e2) }
  | IF e1 = expression THEN e2 = expression ELSE e3 = expression { IfExpr (e1, e2, e3) }
  | DEF i = identifier EQUAL e = expression { DefExpr (i, e) }
  | name = identifier LPAREN args = separated_list(COMMA, expression) RPAREN { CallExpr (name, args) }
// TODO: its auto resolving of shift reduce is how I want it, but I should probably fix it
  | FN args = identifier* EQUAL e = body { FnExpr (args, e) }
  | DEFN name = identifier args = identifier* EQUAL e = body { DefExpr (name, (FnExpr (args, e))) }

body:
  | e = expression { e }

hashpair:
  | e1 = expression EQUAL_GREATER e2 = expression { (e1, e2) }

literal:
  | e = INT   { LitInt e }
  | e = STR   { LitString e }
  | e = CHAR  { LitChar (String.get e 0) }
  | TRUE  { LitBool true }
  | FALSE { LitBool false }
  | i = identifier { LitIdentifier i }
  | LBRACK      items = separated_list(COMMA, expression) RBRACK      { LitList items }
  | LBRACK_PIPE items = separated_list(COMMA, expression) RBRACK_PIPE { LitArray items }
  | LBRACE      items = separated_list(COMMA, hashpair)   RBRACE      { LitHash items }


identifier:
  | i = ID { i } 
