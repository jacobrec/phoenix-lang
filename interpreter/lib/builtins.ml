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
        | (Array v1, Array v2) -> array_equal !v1 !v2
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
  | Array a -> Int48 (Int64.of_int (Array.length !a))
  | Hash h -> Int48 (Int64.of_int (Hashtbl.length h))
  | _ -> raise (TypeErr "Cannot take length of this type")


let at p i =
  let i = Int64.to_int (Types.unwrap_int i) in
  match p with
  | String s -> Char (String.get s i)
  | Array a -> Array.get !a i
  | _ -> raise (TypeErr "@ is not applicable to this type")

let set_at p i v =
  (* TODO: Make set_at mutate array *)
  let i = Int64.to_int (Types.unwrap_int i) in
  match p with
  | String s -> String (String.mapi (fun id c -> if i = id then Types.unwrap_char v else c) s)
  | Array a -> Array (ref (Array.mapi (fun id p -> if i = id then v else p) !a))
  | _ -> raise (TypeErr "@ is not applicable to this type")

let substring s i =
  let s = Types.unwrap_string s in
  let i = Types.unwrap_int i in
  let i = Int64.to_int i in
  String (String.sub s i ((String.length s) - i))

let substring_prefix s i =
  let s = Types.unwrap_string s in
  let i = Types.unwrap_int i in
  let i = Int64.to_int i in
  String (String.sub s 0 i)

let push v p =
  let v = Types.unwrap_array v in
  v := Array.append !v [|p|];
  Array v

let pop v =
  let v = Types.unwrap_array v in
  let l = (Array.length !v) - 1 in
  let p = Array.get !v l in
  v := Array.sub !v 0 l;
  p

let wrap1 fn = 
  let res_fn values = 
    assert ((List.length values) = 1);
    let v = List.hd values in
    fn v
  in res_fn

let wrap2 fn = 
  let res_fn values = 
    assert ((List.length values) = 2);
    let v1 = List.nth values 0 in
    let v2 = List.nth values 1 in
    fn v1 v2
  in res_fn

let wrap3 fn = 
  let res_fn values = 
    assert ((List.length values) = 3);
    let v1 = List.nth values 0 in
    let v2 = List.nth values 1 in
    let v3 = List.nth values 2 in
    fn v1 v2 v3
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
                ("@",         (wrap2 at));
                ("set@",      (wrap3 set_at));
                ("substring", (wrap2 substring));
                ("subprefix", (wrap2 substring_prefix));

                ("push!",     (wrap2 push));
                ("pop!",      (wrap1 pop));

                ("car",       (wrap1 car));
                ("cdr",       (wrap1 cdr))]
