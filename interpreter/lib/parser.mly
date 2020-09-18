%token <int64> INT
%token <string> STR
%token <string> ID
%token PLUS MINUS TIMES DIV MOD
%token LPAREN RPAREN LBRACK RBRACK
%token TRUE FALSE

%token LESS GREATER LESS_EQUAL GREATER_EQUAL EQUAL_EQUAL EQUAL 
%token COLON_COLON COLON_EQUAL SEMICOLON COLON COMMA SEMI_SEMI

%token DEF DEFN FN
%token IF THEN ELSE

%token EOF

/* Low Precedence */
%left  SEMICOLON COLON
%right COLON_EQUAL
%right COLON_COLON
%right EQUAL
%right ELSE
%left EQUAL_EQUAL LESS_EQUAL GREATER_EQUAL
%left LESS GREATER
%left PLUS MINUS
%left TIMES DIV MOD
/* High Precedence */

%start <stmt list> main
%{ open Ast %}

%%


main:
  | stmt = statement EOF { [stmt] }
  | stmt = statement SEMI_SEMI m = main { stmt :: m }

statement:
  | expr = expression { ExprStmt expr } // expr stmt


expression:
  | e = literal { LitExpr e }
  | LPAREN e = expression RPAREN  { e }
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
  | e1 = expression COLON_COLON   e2 = expression { BinExpr (OpCons, e1, e2) }
  | e1 = expression COLON_EQUAL   e2 = expression { BinExpr (OpAssign, e1, e2) }
  | e1 = expression COLON         e2 = expression { BinExpr (OpColon, e1, e2) }
  | e1 = expression SEMICOLON     e2 = expression { BinExpr (OpSemicolon, e1, e2) }
  | IF e1 = expression THEN e2 = expression ELSE e3 = expression { IfExpr (e1, e2, e3) }
  | DEF i = identifier EQUAL e = expression { DefExpr (i, e) }
  | name = identifier LPAREN args = commaexprs RPAREN { CallExpr (name, args) }
// TODO: its auto resolving of shift reduce is how I want it, but I should probably fix it
  | FN args = identifier* EQUAL e = body { FnExpr (args, e) }
  | DEFN name = identifier args = identifier* EQUAL e = body { DefExpr (name, (FnExpr (args, e))) }

body:
  | e = expression { e }

commaexprs:
  | e = expression el = commaexprs2 { e :: el }
  | { [] }
commaexprs2:
  | COMMA e = expression el = commaexprs2 { e :: el }
  | { [] }

literal:
  | e = INT   { LitInt e }
  | e = STR   { LitString e }
  | TRUE  { LitBool true }
  | FALSE { LitBool false }
  | i = identifier { LitIdentifier i }
  | LBRACK items = commaexprs RBRACK { LitArray items }


identifier:
  | i = ID { i } 
