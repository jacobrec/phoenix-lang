open Ast
open Types

let rec eval_uniop op e =
  let v = eval e in
  match op with
  | OpNegate -> v
  | OpNot -> v

and  eval_binop op e1 e2 =
  let v1 = eval e1 in
  let v2 = eval e2 in
  match op with
  | OpPlus -> v1
  | OpMinus -> v1
  | OpTimes -> v1
  | OpDiv -> v1
  | OpMod -> v1
  | OpLess -> v1
  | OpGreater -> v1
  | OpEqualEqual -> v1
  | OpGreaterEqual -> v1
  | OpLessEqual -> v1
  | OpCons -> v1
  | OpAssign -> v1
  | OpSemicolon -> v2
  | OpColon -> v1

and eval_lit = function
  | LitBool v -> Bool v
  | LitIdentifier _v -> Int48 0L (* TODO: add variable *)
  | LitInt v -> Int48 v
  | LitString v -> String v
  | LitArray _v -> Int48 0L (* TODO: add arrays *)
  

and eval_if e1 e2 e3 =
  let v1 = eval e1 in
  if is_truthy v1 then
    eval e2
  else
    eval e3

and eval_fn _args _e =
  Int48 1L (* TODO: add functions *)

and eval_def _name _e =
  Int48 1L (* TODO: add variable *)

and eval_call _name _args =
  Int48 1L (* TODO: add functions *)

and eval = function
  | BinExpr (op, e1, e2) -> eval_binop op e1 e2
  | UniExpr (op, e) -> eval_uniop op e
  | LitExpr l -> eval_lit l
  | IfExpr (e1, e2, e3) -> eval_if e1 e2 e3
  | FnExpr (args, e) -> eval_fn args e
  | DefExpr (name, e) -> eval_def name e
  | CallExpr (name, args) -> eval_call name args


let eval_stmt stmt = 
  match stmt with
  | ExprStmt e ->
     let p = eval e in
     print_string "Got value: ";
     print_endline (string_of_ptype p)

let evaluate ast = 
  ignore (List.map eval_stmt ast)

