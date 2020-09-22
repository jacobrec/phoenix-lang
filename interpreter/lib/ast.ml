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
  | LitInt        of int64
  | LitString     of string
  | LitBool       of bool
  | LitArray      of expr list
  | LitList      of expr list

and expr =
  | UniExpr  of uniop * expr
  | BinExpr  of binop * expr * expr
  | IfExpr   of expr * expr * expr
  | DefExpr  of identifier * expr
  | FnExpr   of identifier list * expr
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
  and string_of_expression_list il = (String.concat ", " (List.map string_of_expr il))
  and string_of_literal lit =
    match lit with
    | LitInt i -> Int64.to_string i
    | LitString s -> "\"" ^ s ^ "\"" (* TODO: escape string properly *)
    | LitIdentifier s -> string_of_identifier s
    | LitBool b -> if b then "true" else "false"
    | LitArray b -> "[" ^ (string_of_expression_list b) ^ "]"
    | LitList b -> "[|" ^ (string_of_expression_list b) ^ "|]"

let string_of_stmt stmt =
  match stmt with
  | ExprStmt e ->  (string_of_expr e) ^ ";;\n"

let rec walk
          ?(uni=fun op e -> UniExpr (op, e))
          ?(bin=fun op e1 e2 -> BinExpr (op, e1, e2))
          ?(eif=fun e1 e2 e3 -> IfExpr (e1, e2, e3))
          ?(def=fun n e -> DefExpr (n, e))
          ?(fn=fun a b -> FnExpr (a, b))
          ?(call=fun a b -> CallExpr (a, b))
          ?(lit=fun l -> LitExpr l)
          ?(walk_uni=true)
          ?(walk_bin=true)
          ?(walk_if=true)
          ?(walk_def=true)
          ?(walk_fn=true)
          ?(walk_call=true)
          exp =
  let mwalk = walk ~uni ~bin ~eif ~def ~fn ~call ~lit
                   ~walk_uni ~walk_bin ~walk_if ~walk_def ~walk_fn ~walk_call in
  match exp with
  | UniExpr (op, e) -> if walk_uni then uni op (mwalk e) else uni op e
  | BinExpr (op, e1, e2) -> if walk_bin then bin op (mwalk e1) (mwalk e2) else bin op e1 e2
  | IfExpr  (e1, e2, e3) -> if walk_if then eif (mwalk e1) (mwalk e2) (mwalk e3) else eif e1 e2 e3
  | DefExpr (n, e) -> if walk_def then def n (mwalk e) else def n e
  | FnExpr  (args, e) -> if walk_fn then fn args (mwalk e) else fn args e
  | CallExpr(name, args) -> if walk_call then call name (List.map mwalk args) else call name args
  | LitExpr l -> lit l
