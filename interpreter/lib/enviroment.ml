exception EnvPop of string
exception EnvLookup of string

type t = ((Ast.identifier, Types.ptype) Hashtbl.t) list 

let create _ =
  [Hashtbl.create 1000]

let push env =
  Hashtbl.create 100 :: env

let add env key value =
  assert (0 < List.length env);
  let tbl = List.hd env in
  Hashtbl.add tbl key value

let rec find env key =
  assert (0 < List.length env);
  let tbl = List.hd env in
  let next = List.tl env in
  if Hashtbl.mem tbl key then
    Some tbl
  else
    match next with
    | [] -> None
    | _ :: _ -> find next key
  
let lookup_raise key = 
  raise (EnvLookup ("Key[" ^ (Ast.string_of_identifier key) ^
                      "] not found in current env"))

let set env key value =
  assert (0 < List.length env);
  let maybetbl = find env key in
  match maybetbl with
  | None -> lookup_raise key
  | Some tbl -> Hashtbl.replace tbl key value

let get env key =
  assert (0 < List.length env);
  let maybetbl = find env key in
  match maybetbl with
  | None -> lookup_raise key
  | Some tbl -> Hashtbl.find tbl key

