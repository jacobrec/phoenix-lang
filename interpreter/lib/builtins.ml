open Types
exception TypeErr of string

let add v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (Int64.add v1 v2)
  | (Float v1, Int48 v2) -> Float (v1 +. (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Float ((Int64.to_float v1) +. v2)
  | (Float v1, Float v2) -> Float (v1 +. v2)
  | (_, _) -> raise (TypeErr "Cannot add these types")

let subtract v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (Int64.sub v1 v2)
  | (Float v1, Int48 v2) -> Float (v1 -. (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Float ((Int64.to_float v1) -. v2)
  | (Float v1, Float v2) -> Float (v1 -. v2)
  | (_, _) -> raise (TypeErr "Cannot subtract these types")

let times v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (Int64.mul v1 v2)
  | (Float v1, Int48 v2) -> Float (v1 *. (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Float ((Int64.to_float v1) *. v2)
  | (Float v1, Float v2) -> Float (v1 *. v2)
  | (_, _) -> raise (TypeErr "Cannot multiply these types")

let divide v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (Int64.div v1 v2)
  | (Float v1, Int48 v2) -> Float (v1 /. (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Float ((Int64.to_float v1) /. v2)
  | (Float v1, Float v2) -> Float (v1 /. v2)
  | (_, _) -> raise (TypeErr "Cannot multiply these types")
