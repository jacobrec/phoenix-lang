open Ast
open Types

exception EvalErr of string

let get_free_vars bound expression =
  let bound = ref bound in
  let free = ref [] in
  let lit = function
    | LitIdentifier i -> if List.mem i !bound then ()
                         else free := i :: !free;
                         LitExpr (LitIdentifier i)
    | i -> LitExpr i in
  let def name expr = bound := name :: !bound; DefExpr (name, expr) in
  ignore (walk ~lit ~def ~walk_fn:false expression);
  !free

let rec call env fn args =
  let (argnames, expr) = fn in
  let nenv = Enviroment.push env in
  let add_env name item =
    Enviroment.add nenv name item in
  ignore (List.map2 add_env argnames args);
  eval nenv expr

and eval_uniop env op e =
  let v = eval env e in
  match op with
  | OpNegate -> v
  | OpNot -> v

and  eval_binop env op e1 e2 =
  let v1 = eval env e1 in
  let v2 = eval env e2 in
  match op with
  | OpPlus -> Builtins.add v1 v2
  | OpMinus -> Builtins.subtract v1 v2
  | OpTimes -> Builtins.times v1 v2
  | OpDiv -> Builtins.divide v1 v2
  | OpMod -> Builtins.modulo v1 v2
  | OpLess -> Builtins.less v1 v2
  | OpGreater -> Builtins.greater v1 v2
  | OpEqualEqual -> raise (EvalErr "Equality is not yet supported")
  | OpGreaterEqual -> Builtins.greater_equal v1 v2
  | OpLessEqual -> Builtins.less_equal v1 v2
  | OpCons -> raise (EvalErr "Cons is not yet supported")
  | OpSemicolon -> v2
  | OpColon -> v1
  | OpAssign -> raise (EvalErr "Assignment is not yet supported")

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

and eval_fn env args e =
  let eval_ident i = Enviroment.get env i in
  let free_vars = get_free_vars args e in
  let free_vals = (List.map eval_ident free_vars) in
  let fn = (args, e) in
  Closure (fn, free_vars, free_vals)

and eval_def env name e =
  let v = eval env e in
  Enviroment.add env name v;
  v

and eval_call env name args =
  let fn = Enviroment.get env name in
  match fn with
  | Closure (fn, free, vals) ->
     let (fargs, fn_expr) = fn in
     let evaled = (List.map (fun x -> eval env x) args) in
     call env ((List.append fargs free), fn_expr) (List.append evaled vals)
  | Func fn ->
     let evals = eval env in
     call env fn (List.map evals args)
  | BuiltinFunc fn ->
     let evals = eval env in
     fn (List.map evals args)
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

let env_with_builtins _ =
  let env = Enviroment.create () in
  Enviroment.add env "println" (Types.BuiltinFunc Builtins.println);
  Enviroment.add env "print" (Types.BuiltinFunc Builtins.print);
  env
  
let evaluate ast = 
  let env = env_with_builtins () in
  let e = eval_stmt env in
  ignore (List.map e ast)

