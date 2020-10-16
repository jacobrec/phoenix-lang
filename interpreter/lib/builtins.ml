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
        | (String v1, Char v2) -> String.equal v1 (String.make 1 v2)
        | (Char v1, String v2) -> String.equal (String.make 1 v1) v2
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
  | (Int48 v1, String v2) -> String ((Int64.to_string v1) ^ v2)
  | (String v1, Int48 v2) -> String (v1 ^ (Int64.to_string v2))
  | (Char v1, String v2) -> String ((String.make 1 v1) ^ v2)
  | (String v1, Char v2) -> String (v1 ^ (String.make 1 v2))
  | (List v1, List v2) -> List (List.append v1 v2)
  | (Array v1, Array v2) -> Array (ref (Array.append !v1 !v2))
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
    | (Char v1, Char v2) -> v1 < v2
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
    | (Char v1, Char v2) -> v1 > v2
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
    | (Char v1, Char v2) -> v1 <= v2
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
    | (Char v1, Char v2) -> v1 >= v2
    | (_, _) -> raise (TypeErr "Cannot check greater or equal of these types") in
  Bool b

(* shift(1, 3) = 8, shift(8, -1) = 4 *)
let shift v1 v2 =
  let i1 = Types.unwrap_int v1 in
  let i2 = Int64.to_int (Types.unwrap_int v2) in

  Int48 (if i2 >= 0
         then Int64.shift_left i1 i2
         else Int64.shift_right_logical i1 (-i2))

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
  match v1 with 
  | String str -> print_string str; String str
  | Char c -> print_char c; String (String.make 1 c)
  | _ ->
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
  let i = Int64.to_int (Types.unwrap_int i) in
  match p with
  | String s -> String (String.mapi (fun id c -> if i = id then Types.unwrap_char v else c) s)
  | Array a -> Array (ref (Array.mapi (fun id p -> if i = id then v else p) !a))
  | _ -> raise (TypeErr "@ is not applicable to this type")

let set_at_mut p i v =
  let i = Int64.to_int (Types.unwrap_int i) in
  match p with
  | Array a -> Array.set !a i v; Array a
  | String _ -> raise (TypeErr "Cannot set@! an immutable string")
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

let insert h k v =
  let h = Types.unwrap_hash h in
  Hashtbl.replace h k v;
  Hash h

let remove h k =
  let h = Types.unwrap_hash h in
  Hashtbl.remove h k;
  Hash h

let get h k =
  let h = Types.unwrap_hash h in
  try Hashtbl.find h k
  with Not_found -> failwith ("Key [" ^ (string_of_ptype k) ^ "] not in hashmap")

let has h k =
  let h = Types.unwrap_hash h in
  Bool (Hashtbl.mem h k)

let ord c =
  let v = Types.unwrap_char c in
  Int48 (Int64.of_int (Char.code v))

let chr i =
  let v = Types.unwrap_int i in
  let v = Int64.to_int v in
  Char (Char.chr v)

let pfile_open path mode =
  let mode = Types.unwrap_string mode in
  let path = Types.unwrap_string path in
  let bin = String.contains mode 'b' in
  let append = String.contains mode 'a' in
  let write = String.contains mode 'w' in
  let read = String.contains mode 'r' in
  let create = String.contains mode '+' in
  let mode_in = read && not write && not append in
  let mode_append = not read && not write && append in
  let mode_truncate = not read && write && not append in
  let perm = 0o644 in
  let flags = List.concat [
                  if mode_in then       [Open_rdonly] else [];
                  if mode_append then   [Open_append; Open_wronly] else [];
                  if mode_truncate then [Open_trunc; Open_wronly] else [];
                  if bin then           [Open_binary] else [Open_text];
                  if create then        [Open_creat] else [];
                ] in
  if mode_in then
    File (InFile (open_in_gen flags perm path))
  else if mode_append || mode_truncate then
    File (OutFile (open_out_gen flags perm path))
  else
    raise (TypeErr "Illegal combination of file opening flags")
    
let pfile_close file =
  let file = Types.unwrap_file file in
  (match file with
   | InFile f -> close_in f
   | OutFile f -> close_out f);
  List []
  
let readchar file =
  let file = Types.unwrap_file_in file in
  try Char (input_char file)
    with End_of_file -> EOF
  
let writechar file char =
  let char = Types.unwrap_char char in
  let file = Types.unwrap_file_out file in
  output_char file char;
  List []
                  

let pexit v =
  let code = Types.unwrap_int v in
  let code = Int64.to_int code in
  exit code

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
                ("exit",      (wrap1 pexit));

                ("int?",      (wrap1_bool Types.is_int));
                ("bool?",     (wrap1_bool Types.is_bool));
                ("string?",   (wrap1_bool Types.is_string));
                ("list?",     (wrap1_bool Types.is_list));
                ("array?",    (wrap1_bool Types.is_array));
                ("hash?",     (wrap1_bool Types.is_hash));
                ("function?", (wrap1_bool Types.is_func));
                ("char?",     (wrap1_bool Types.is_char));
                ("float?",    (wrap1_bool Types.is_float));
                ("eof?",      (wrap1_bool Types.is_EOF));
                ("file?",     (wrap1_bool Types.is_file));

                ("shift",     (wrap2 shift));

                ("length",    (wrap1 length));
                ("@",         (wrap2 at));
                ("set@",      (wrap3 set_at));
                ("substring", (wrap2 substring));
                ("subprefix", (wrap2 substring_prefix));

                ("push!",     (wrap2 push));
                ("pop!",      (wrap1 pop));
                ("set@!",     (wrap3 set_at_mut));

                ("insert!",   (wrap3 insert));
                ("remove!",   (wrap2 remove));
                ("get",       (wrap2 get));
                ("has",       (wrap2 has));

                ("chr",       (wrap1 chr));
                ("ord",       (wrap1 ord));

                ("open!",      (wrap2 pfile_open));
                ("close!",     (wrap1 pfile_close));
                ("readchar!",  (wrap1 readchar));
                ("writechar!", (wrap2 writechar));

                ("car",       (wrap1 car));
                ("cdr",       (wrap1 cdr))]
