%token <int> INT
%token <string> STR
%token PLUS MINUS TIMES DIV MOD
%token LPAREN RPAREN

%token LESS GREATER LESS_EQUAL GREATER_EQUAL EQUAL_EQUAL
%token COLON_COLON COLON_EQUAL SEMICOLON COLON

%token EOF

/* Low Precedence */
%left  SEMICOLON COLON
%right COLON_EQUAL
%right COLON_COLON
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
| stmt = statement m = main { stmt :: m}

statement:
| stmt = expr_stmt { stmt }

expr_stmt:
| expr = expression { ExprStmt expr }

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

literal:
| e = INT { LitInt e }
| e = STR { LitString e }


