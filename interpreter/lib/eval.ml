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
  if ((List.length argnames) <> (List.length args)) then failwith "Invalid number of arguments" else ();
  let nenv = Enviroment.push env in
  let add_env name item =
    Enviroment.add nenv name item in
  ignore (List.map2 add_env argnames args);
  eval nenv expr

and eval_uniop env op e =
  let v = eval env e in
  match op with
  | OpNegate -> (match v with
                 | Int48 v -> Int48 (Int64.neg v)
                 | Float v -> Float (Float.neg v)
                 | _ -> failwith "cannot negate something that's not a number")
  | OpNot -> Bool (if is_truthy v then false else true)

and eval_short_binop env op e1 e2 =
  let v1 = eval env e1 in
  match op with
  | OpAnd -> if is_truthy v1 then eval env e2 else v1
  | OpOr -> if is_truthy v1 then v1 else eval env e2
and eval_binop env op e1 e2 =
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
  | OpEqualEqual -> Builtins.equal v1 v2
  | OpGreaterEqual -> Builtins.greater_equal v1 v2
  | OpLessEqual -> Builtins.less_equal v1 v2
  | OpCons -> Builtins.cons v1 v2
  | OpSemicolon -> v2
  | OpColon -> v1
  | OpAssign -> raise (EvalErr "Assignment is not yet supported")

and eval_lit env = function
  | LitBool v -> Bool v
  | LitIdentifier v -> Enviroment.get env v
  | LitInt v -> Int48 v
  | LitString v -> String v
  | LitList v ->
     List (List.map (fun i -> eval env i) v)
  | LitArray v ->
     Array (Array.of_list (List.map (fun i -> eval env i) v))

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
  | ShortBinExpr (op, e1, e2) -> eval_short_binop env op e1 e2
  | BinExpr (op, e1, e2) -> eval_binop env op e1 e2
  | UniExpr (op, e) -> eval_uniop env op e
  | LitExpr l -> eval_lit env l
  | IfExpr (e1, e2, e3) -> eval_if env e1 e2 e3
  | FnExpr (args, e) -> eval_fn env args e
  | DefExpr (name, e) -> eval_def env name e
  | CallExpr (name, args) -> eval_call env name args


let eval_stmt ?(loud=false) env stmt = 
  match stmt with
  | ExprStmt e ->
     let p = eval env e in
     if loud then begin
       print_string "=> ";
       print_endline (string_of_ptype p);
     end else ()

let base_env = Enviroment.create ()
let should_init_base_env = ref true
let rec env_with_builtins _ =
  if !should_init_base_env then begin
  should_init_base_env := false;
  
  ignore (List.map (fun (a, b) -> Enviroment.add base_env a (Types.BuiltinFunc b)) Builtins.builtins);
  end else ();
  base_env
  
and evaluate ?(loud=false) ast = 
  let env = env_with_builtins () in
  let e = eval_stmt ~loud env in
  ignore (List.map e ast)
