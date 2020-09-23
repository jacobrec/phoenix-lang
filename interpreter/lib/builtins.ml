open Types
exception TypeErr of string


(* For the == operator *)
let rec equal v1 v2 =
  Bool (match (v1, v2) with
        | (Int48 v1, Int48 v2) -> Int64.equal v1 v2
        | (Float v1, Float v2) -> Float.equal v1 v2
        | (String v1, String v2) -> String.equal v1 v2
        | (Bool v1, Bool v2) -> Bool.equal v1 v2
        | (Char v1, Char v2) -> Char.equal v1 v2
        | (List v1, List v2) -> list_equal v1 v2
        | (Array v1, Array v2) -> array_equal v1 v2
        | (_, _) -> false)

and list_equal l1 l2 =
  if (List.length l1) = (List.length l2) then
    List.for_all2 (fun a b -> is_truthy (equal a b)) l1 l2
  else false

and array_equal l1 l2 =
  if (Array.length l1) = (Array.length l2) then
    Array.for_all2 (fun a b -> is_truthy (equal a b)) l1 l2
  else false

(* For the + operator *)
let add v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (Int64.add v1 v2)
  | (Float v1, Int48 v2) -> Float (v1 +. (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Float ((Int64.to_float v1) +. v2)
  | (Float v1, Float v2) -> Float (v1 +. v2)
  | (String v1, String v2) -> String (v1 ^ v2)
  | (_, _) -> raise (TypeErr "Cannot add these types")

(* For the - operator *)
let subtract v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (Int64.sub v1 v2)
  | (Float v1, Int48 v2) -> Float (v1 -. (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Float ((Int64.to_float v1) -. v2)
  | (Float v1, Float v2) -> Float (v1 -. v2)
  | (_, _) -> raise (TypeErr "Cannot subtract these types")

(* For the * operator *)
let times v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (Int64.mul v1 v2)
  | (Float v1, Int48 v2) -> Float (v1 *. (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Float ((Int64.to_float v1) *. v2)
  | (Float v1, Float v2) -> Float (v1 *. v2)
  | (_, _) -> raise (TypeErr "Cannot multiply these types")

(* For the / operator *)
let divide v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (Int64.div v1 v2)
  | (Float v1, Int48 v2) -> Float (v1 /. (Int64.to_float v2))
  | (Int48 v1, Float v2) -> Float ((Int64.to_float v1) /. v2)
  | (Float v1, Float v2) -> Float (v1 /. v2)
  | (_, _) -> raise (TypeErr "Cannot divide these types")

(* For the % operator *)
let modulo v1 v2 =
  match (v1, v2) with
  | (Int48 v1, Int48 v2) -> Int48 (let r = Int64.rem v1 v2 in
                                   if r >= 0L then r else Int64.add r v2)
  | (Float v1, Int48 v2) -> Float (let f2 = (Int64.to_float v2) in
                                   let r = Float.rem v1 f2 in
                                   if r >= 0.0 then r else r +. f2)
  | (_, _) -> raise (TypeErr "Cannot modulo these types")

(* For the < operator *)
let less v1 v2 =
  let icmp i1 i2 = i1 < i2 in
  let fcmp f1 f2 = -1 = Float.compare f1 f2 in
  let b = match (v1, v2) with
    | (Int48 v1, Int48 v2) -> icmp v1 v2
    | (Float v1, Int48 v2) -> fcmp v1 (Int64.to_float v2)
    | (Int48 v1, Float v2) -> fcmp (Int64.to_float v1) v2
    | (Float v1, Float v2) -> fcmp v1 v2
    | (String v1, String v2) -> -1 = String.compare v1 v2
    | (_, _) -> raise (TypeErr "Cannot check less then of these types") in
  Bool b

(* For the > operator *)
let greater v1 v2 =
  let icmp i1 i2 = i1 > i2 in
  let fcmp f1 f2 = 1 = Float.compare f1 f2 in
  let b = match (v1, v2) with
    | (Int48 v1, Int48 v2) -> icmp v1 v2
    | (Float v1, Int48 v2) -> fcmp v1 (Int64.to_float v2)
    | (Int48 v1, Float v2) -> fcmp (Int64.to_float v1) v2
    | (Float v1, Float v2) -> fcmp v1 v2
    | (String v1, String v2) -> 1 = String.compare v1 v2
    | (_, _) -> raise (TypeErr "Cannot check greater then of these types") in
  Bool b

(* For the <= operator *)
let less_equal v1 v2 =
  let icmp i1 i2 = i1 <= i2 in
  let fcmp f1 f2 = let v = Float.compare f1 f2 in
                   v = -1 || v = 0 in
  let b = match (v1, v2) with
    | (Int48 v1, Int48 v2) -> icmp v1 v2
    | (Float v1, Int48 v2) -> fcmp v1 (Int64.to_float v2)
    | (Int48 v1, Float v2) -> fcmp (Int64.to_float v1) v2
    | (Float v1, Float v2) -> fcmp v1 v2
    | (String v1, String v2) -> 1 <> String.compare v1 v2
    | (_, _) -> raise (TypeErr "Cannot check less or equal of these types") in
  Bool b

(* For the >= operator *)
let greater_equal v1 v2 =
  let icmp i1 i2 = i1 >= i2 in
  let fcmp f1 f2 = let v = Float.compare f1 f2 in
                   v = 1 || v = 0 in
  let b = match (v1, v2) with
    | (Int48 v1, Int48 v2) -> icmp v1 v2
    | (Float v1, Int48 v2) -> fcmp v1 (Int64.to_float v2)
    | (Int48 v1, Float v2) -> fcmp (Int64.to_float v1) v2
    | (Float v1, Float v2) -> fcmp v1 v2
    | (String v1, String v2) -> -1 <> String.compare v1 v2
    | (_, _) -> raise (TypeErr "Cannot check greater or equal of these types") in
  Bool b

(* For the :: operator *)
let cons v1 v2 =
  match v2 with
  | List l -> List (v1 :: l)
  | _ -> raise (TypeErr "Cannot cons to a non list")

let car v1 =
  match v1 with
  | List (h :: _) -> h
  | List _ -> raise (TypeErr "Cannot take car of empty list")
  | _ -> raise (TypeErr "Cannot take car of non list")

let cdr v1 =
  match v1 with
  | List (_ :: t) -> List t
  | List _ -> raise (TypeErr "Cannot take cdr of empty list")
  | _ -> raise (TypeErr "Cannot take cdr of non list")

let print v1 =
  let str = string_of_ptype v1 in
  print_string str;
  String str

let println v1 =
  let v = print v1 in
  print_newline ();
  v

let length = function
  | String s -> Int48 (Int64.of_int (String.length s))
  | List l -> Int48 (Int64.of_int (List.length l))
  | Array a -> Int48 (Int64.of_int (Array.length a))
  | Hash h -> Int48 (Int64.of_int (Hashtbl.length h))
  | _ -> raise (TypeErr "Cannot take length of this type")
  
let wrap1 fn = 
  let res_fn values = 
    assert ((List.length values) = 1);
    let v = List.hd values in
    fn v
  in res_fn

let wrap1_bool fn = 
  let fn2 = wrap1 fn in
  let res values =
    Bool (fn2 values)
  in res


let builtins = [("println",   (wrap1 println));
                ("print",     (wrap1 print));

                ("int?",      (wrap1_bool Types.is_int));
                ("bool?",     (wrap1_bool Types.is_bool));
                ("string?",   (wrap1_bool Types.is_string));
                ("list?",     (wrap1_bool Types.is_list));
                ("array?",    (wrap1_bool Types.is_array));
                ("hash?",     (wrap1_bool Types.is_hash));
                ("function?", (wrap1_bool Types.is_func));
                ("char?",     (wrap1_bool Types.is_char));
                ("float?",    (wrap1_bool Types.is_float));

                ("length",    (wrap1 length));

                ("car",       (wrap1 car));
                ("cdr",       (wrap1 cdr))]
