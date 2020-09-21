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
  | (_, _) -> raise (TypeErr "Cannot divide these types")

let modulo v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (let r = Int64.rem v1 v2 in
                                   if r >= 0L then r else Int64.add r v2)
  | (Float v1, Int48 v2) -> Float (let f2 = (Int64.to_float v2) in
                                   let r = Float.rem v1 f2 in
                                   if r >= 0.0 then r else r +. f2)
  | (_, _) -> raise (TypeErr "Cannot modulo these types")

let less v1 v2 =
  let icmp i1 i2 = i1 < i2 in
  let fcmp f1 f2 = -1 = Float.compare f1 f2 in
  let b = match (v1, v2) with
  | (Int48 v1, Int48 v2) -> icmp v1 v2
  | (Float v1, Int48 v2) -> fcmp v1 (Int64.to_float v2)
  | (Int48 v1, Float v2) -> fcmp (Int64.to_float v1) v2
  | (Float v1, Float v2) -> fcmp v1 v2
  | (_, _) -> raise (TypeErr "Cannot check less then of these types") in
  Bool b
  
  
let greater v1 v2 =
  let icmp i1 i2 = i1 > i2 in
  let fcmp f1 f2 = 1 = Float.compare f1 f2 in
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Bool (icmp v1 v2)
  | (Float v1, Int48 v2) -> Bool (fcmp v1 (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Bool (fcmp (Int64.to_float v1) v2)
  | (Float v1, Float v2) -> Bool (fcmp v1 v2)
  | (_, _) -> raise (TypeErr "Cannot check greater then of these types")

let less_equal v1 v2 =
  let icmp i1 i2 = i1 <= i2 in
  let fcmp f1 f2 = let v = Float.compare f1 f2 in
                   v = -1 || v = 0 in
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Bool (icmp v1 v2)
  | (Float v1, Int48 v2) -> Bool (fcmp v1 (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Bool (fcmp (Int64.to_float v1) v2)
  | (Float v1, Float v2) -> Bool (fcmp v1 v2)
  | (_, _) -> raise (TypeErr "Cannot check less or equal of these types")

let greater_equal v1 v2 =
  let icmp i1 i2 = i1 >= i2 in
  let fcmp f1 f2 = let v = Float.compare f1 f2 in
                   v = 1 || v = 0 in
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Bool (icmp v1 v2)
  | (Float v1, Int48 v2) -> Bool (fcmp v1 (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Bool (fcmp (Int64.to_float v1) v2)
  | (Float v1, Float v2) -> Bool (fcmp v1 v2)
  | (_, _) -> raise (TypeErr "Cannot check greater or equal of these types")

let print values =
  assert ((List.length values) = 1);
  let v = List.hd values in
  let str = string_of_ptype v in
  print_string str;
  String str

let println values =
  let v = print values in
  print_newline ();
  v
  
