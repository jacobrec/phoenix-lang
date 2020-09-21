open Ast
open Types

exception EvalErr of string

let rec call env fn args =
  let (argnames, expr) = fn in
  let env = Enviroment.push env in
  let add_env name item = Enviroment.add env name item in
  ignore (List.map2 add_env argnames args);
  eval env expr


and eval_uniop env op e =
  let v = eval env e in
  match op with
  | OpNegate -> v
  | OpNot -> v

and  eval_binop env op e1 e2 =
  let v1 = eval env e1 in
  let v2 = eval env e2 in
  match op with
  | OpPlus -> Builtins.add v1 v1
  | OpMinus -> Builtins.subtract v1 v2
  | OpTimes -> Builtins.times v1 v2
  | OpDiv -> Builtins.divide v1 v2
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

and eval_lit env = function
  | LitBool v -> Bool v
  | LitIdentifier v -> Enviroment.get env v
  | LitInt v -> Int48 v
  | LitString v -> String v
  | LitArray _v -> Int48 0L (* TODO: add arrays *)
  

and eval_if env e1 e2 e3 =
  let v1 = eval env e1 in
  if is_truthy v1 then
    eval env e2
  else
    eval env e3

and eval_fn _env args e =
  Func (args, e)

and eval_def env name e =
  let v = eval env e in
  Enviroment.add env name v;
  v

and eval_call env name args =
  let fn = Enviroment.get env name in
  match fn with
  | Func fn ->
     let evals = eval env in
     call env fn (List.map evals args)
  | _ -> raise (EvalErr "cannot call non callable object")

and eval env = function
  | BinExpr (op, e1, e2) -> eval_binop env op e1 e2
  | UniExpr (op, e) -> eval_uniop env op e
  | LitExpr l -> eval_lit env l
  | IfExpr (e1, e2, e3) -> eval_if env e1 e2 e3
  | FnExpr (args, e) -> eval_fn env args e
  | DefExpr (name, e) -> eval_def env name e
  | CallExpr (name, args) -> eval_call env name args


let eval_stmt env stmt = 
  match stmt with
  | ExprStmt e ->
     let p = eval env e in
     print_string "Got value: ";
     print_endline (string_of_ptype p)

let evaluate ast = 
  let env = Enviroment.create () in
  let e = eval_stmt env in
  ignore (List.map e ast)

