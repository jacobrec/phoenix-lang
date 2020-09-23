open Ast

type pfunc = identifier list * expr
type ptype =
  | Int48 of int64
  | Bool of bool
  | String of string
  | Char of char
  | Float of float (* 64bit floats *)
  | Func of pfunc
  | List of ptype list
  | Array of ptype Array.t
  | Hash of (ptype, ptype) Hashtbl.t
  | BuiltinFunc of (ptype list -> ptype)
  | Closure of pfunc * identifier list * ptype list

let unwrap_int ?(err="Unwrap error, expected int") = function
  | Int48 i -> i
  | _ -> failwith err

let unwrap_bool ?(err="Unwrap error, expected bool") = function
  | Bool i -> i
  | _ -> failwith err

let unwrap_string ?(err="Unwrap error, expected string") = function
  | String i -> i
  | _ -> failwith err

let unwrap_char ?(err="Unwrap error, expected char") = function
  | Char i -> i
  | _ -> failwith err

let unwrap_float ?(err="Unwrap error, expected float") = function
  | Float i -> i
  | _ -> failwith err

let unwrap_func ?(err="Unwrap error, expected func") = function
  | Func i -> i
  | _ -> failwith err

let unwrap_list ?(err="Unwrap error, expected list") = function
  | List i -> i
  | _ -> failwith err

let unwrap_array ?(err="Unwrap error, expected array") = function
  | Array i -> i
  | _ -> failwith err

let unwrap_hash ?(err="Unwrap error, expected hash") = function
  | Hash i -> i
  | _ -> failwith err

let unwrap_builtin ?(err="Unwrap error, expected builtin") = function
  | BuiltinFunc i -> i
  | _ -> failwith err

let unwrap_closure ?(err="Unwrap error, expected closure") = function
  | Closure (a, b, c) -> (a, b, c)
  | _ -> failwith err


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

let is_array = function
  | Array _ -> true
  | _ -> false

let is_list = function
  | List _ -> true
  | _ -> false

let is_hash = function
  | Hash _ -> true
  | _ -> false



let is_truthy = function
  | Int48 v -> v <> 0L
  | Bool v -> v
  | String v -> 0 <> String.length v
  | Char v -> 0 <> Char.code v
  | Float v -> 0.0 <> v
  | Func (_args, _e) -> true
  | BuiltinFunc _ -> true
  | Array l -> 0 <> Array.length l
  | List l -> 0 <> List.length l
  | Hash l -> 0 <> Hashtbl.length l
  | Closure (_fn, _free, _vals) -> true

let rec seq_to_list = function
  | Seq.Cons (a, s) -> a :: (seq_to_list (s ()))
  | Seq.Nil -> []
  
let rec string_of_ptype = function
  | Int48 v -> Int64.to_string v
  | Bool v -> if v then "true" else "false"
  | String v -> v
  | Char v -> String.make 1 v
  | Float v -> Float.to_string v
  | Func (args, e) -> "[fn " ^ (String.concat " " args) ^ " = " ^ Ast.string_of_expr e ^ "]"
  | BuiltinFunc _ -> "[builtin fn]"
  | Array v -> "[|" ^ (String.concat ", " (List.map string_of_ptype (Array.to_list v))) ^ "|]"
  | List v -> "[" ^ (String.concat ", " (List.map string_of_ptype v)) ^ "]"
  | Hash v -> "{" ^ (String.concat ", "
                       (List.map (fun (a, b) -> (string_of_ptype a) ^ "=>" ^ (string_of_ptype b))
                          (seq_to_list ((Hashtbl.to_seq v) ())))) ^ "}"
  | Closure (_fn, _free, _vals) -> "[closure]"
