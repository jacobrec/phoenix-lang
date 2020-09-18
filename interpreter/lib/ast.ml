type identifier = string

type literal =
  | LitIdentifier of identifier
  | LitInt        of int
  | LitString     of string
  | LitBool       of bool

type uniop =
  | OpNegate
  | OpNot

type binop =
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDiv
  | OpMod
  | OpLess
  | OpGreater
  | OpEqualEqual
  | OpGreaterEqual
  | OpLessEqual
  | OpCons
  | OpAssign
  | OpSemicolon
  | OpColon

type expr =
  | UniExpr  of uniop * expr
  | BinExpr  of binop * expr * expr
  | IfExpr   of expr * expr * expr
  | DefExpr  of identifier * expr
  | FnExpr   of identifier list * expr
  | LitExpr  of literal

type stmt =
  | DefnStmt of identifier list * expr
  | ExprStmt of expr

let string_of_uniop op =
  match op with
  | OpNegate -> "-"
  | OpNot -> "!"

let string_of_binop op =
  match op with
  | OpPlus -> "+"
  | OpMinus -> "-"
  | OpTimes -> "*"
  | OpDiv -> "/"
  | OpMod -> "%"
  | OpLess -> "<"
  | OpGreater -> ">"
  | OpEqualEqual -> "=="
  | OpGreaterEqual -> ">="
  | OpLessEqual -> "<="
  | OpCons -> "::"
  | OpAssign -> ":="
  | OpSemicolon -> ";"
  | OpColon -> ":"

let string_of_identifier i = i
let string_of_identifer_list il = (String.concat " " (List.map string_of_identifier il))

let string_of_literal lit =
  match lit with
  | LitInt i -> string_of_int i
  | LitString s -> "\"" ^ s ^ "\"" (* TODO: escape string properly *)
  | LitIdentifier s -> string_of_identifier s
  | LitBool b -> if b then "true" else "false"

let rec string_of_expr expr =
  match expr with
  | UniExpr (op, e) -> (string_of_uniop op) ^ (string_of_expr e)
  | BinExpr (op, e1, e2) -> "("^(string_of_expr e1) ^ (string_of_binop op) ^
                                (string_of_expr e2)^")"
  | IfExpr (cond, e1, e2) -> "if " ^ (string_of_expr cond) ^ " then "
                             ^ (string_of_expr e1) ^ " else " ^ (string_of_expr e2)
  | DefExpr (i, e) -> "def " ^ (string_of_identifier i) ^ " = " ^ (string_of_expr e)
  | FnExpr (il, e) -> "fn " ^ (string_of_identifer_list il) ^ " = " ^ (string_of_expr e)
  | LitExpr l -> string_of_literal l

let string_of_stmt stmt =
  match stmt with
  | DefnStmt (il, e) -> "defn " ^ (string_of_identifer_list il)
                        ^ " = " ^ (string_of_expr e) ^ "\n"
  | ExprStmt e ->  (string_of_expr e) ^ "\n"
