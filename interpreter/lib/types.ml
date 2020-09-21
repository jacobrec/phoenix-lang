open Ast

type pfunc = identifier list * expr
type ptype =
  | Int48 of int64
  | Bool of bool
  | String of string
  | Char of char
  | Float of float (* 64bit floats *)
  | Func of pfunc
  | BuiltinFunc of (ptype list -> ptype)
  | Closure of pfunc * identifier list * ptype list

let is_int = function
  | Int48 _ -> true
  | _ -> false

let is_bool = function
  | Bool _ -> true
  | _ -> false

let is_string = function
  | String _ -> true
  | _ -> false

let is_char = function
  | Char _ -> true
  | _ -> false

let is_float = function
  | Float _ -> true
  | _ -> false

let is_func = function
  | Func _ -> true
  | _ -> false

let is_closure = function
  | Closure _ -> true
  | _ -> false

let is_truthy = function
  | Int48 v -> v <> 0L
  | Bool v -> v
  | String v -> 0 <> String.length v
  | Char v -> 0 <> Char.code v
  | Float v -> 0.0 <> v
  | Func (_args, _e) -> true
  | BuiltinFunc _ -> true
  | Closure (_fn, _free, _vals) -> true

let string_of_ptype = function
  | Int48 v -> Int64.to_string v
  | Bool v -> if v then "true" else "false"
  | String v -> v
  | Char v -> String.make 1 v
  | Float v -> Float.to_string v
  | Func (args, e) -> "[fn " ^ (String.concat " " args) ^ " = " ^ Ast.string_of_expr e ^ "]"
  | BuiltinFunc _ -> "[builtin fn]"
  | Closure (_fn, _free, _vals) -> "[closure]"
