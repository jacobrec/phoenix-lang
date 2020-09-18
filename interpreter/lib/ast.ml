type identifier = string

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

type literal =
  | LitIdentifier of identifier
  | LitInt        of int
  | LitString     of string
  | LitBool       of bool
  | LitArray      of expr list
and expr =
  | UniExpr  of uniop * expr
  | BinExpr  of binop * expr * expr
  | IfExpr   of expr * expr * expr
  | DefExpr  of identifier * expr
  | FnExpr   of identifier list * expr
  | DefnExpr of identifier * identifier list * expr
  | CallExpr of identifier * expr list
  | LitExpr  of literal

type stmt =
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
  | OpSemicolon -> ";\n"
  | OpColon -> ":\n"

let string_of_identifier i = i
let string_of_identifer_list il = (String.concat " " (List.map string_of_identifier il))


let rec string_of_expr expr =
  match expr with
  | UniExpr (op, e) -> (string_of_uniop op) ^ (string_of_expr e)
  | BinExpr (op, e1, e2) -> (string_of_expr e1) ^ (string_of_binop op) ^
                                (string_of_expr e2)
  | IfExpr (cond, e1, e2) -> "if " ^ (string_of_expr cond) ^ " then "
                             ^ (string_of_expr e1) ^ " else " ^ (string_of_expr e2)
  | DefExpr (i, e) -> "def " ^ (string_of_identifier i) ^ " = " ^ (string_of_expr e)
  | FnExpr (il, e) -> "fn " ^ (string_of_identifer_list il) ^ " = " ^ (string_of_expr e)
  | LitExpr l -> string_of_literal l
  | CallExpr (n, el) -> (string_of_identifier n) ^ "(" ^
                           (string_of_expression_list el) ^ ")"
  | DefnExpr (n, il, e) -> "defn " ^ (string_of_identifier n) ^ " "
                           ^ (string_of_identifer_list il)
                           ^ " = " ^ (string_of_expr e) ^ "\n"
  and string_of_expression_list il = (String.concat ", " (List.map string_of_expr il))
  and string_of_literal lit =
    match lit with
    | LitInt i -> string_of_int i
    | LitString s -> "\"" ^ s ^ "\"" (* TODO: escape string properly *)
    | LitIdentifier s -> string_of_identifier s
    | LitBool b -> if b then "true" else "false"
    | LitArray b -> "[" ^ (string_of_expression_list b) ^ "]"

let string_of_stmt stmt =
  match stmt with
  | ExprStmt e ->  (string_of_expr e) ^ ";;\n"
