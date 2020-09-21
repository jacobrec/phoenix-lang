exception EnvPop of string
exception EnvLookup of string

type t = ((Ast.identifier, Types.ptype) Hashtbl.t) list 

let create _ =
  [Hashtbl.create 1000]

let push env =
  Hashtbl.create 100 :: env

let pop env =
  match env with
  | _ :: t -> t
  | [] -> raise (EnvPop "Cannot pop base enviroment")

let add env key value =
  let tbl = List.hd env in
  Hashtbl.add tbl key value

let rec find env key =
  let tbl = List.hd env in
  let next = List.tl env in
  if Hashtbl.mem tbl key then
    Some tbl
  else
    match next with
    | [] -> None
    | _ :: n_env -> find n_env key
  
let lookup_raise key = 
  raise (EnvLookup ("Key[" ^ (Ast.string_of_identifier key) ^
                      "] not found in current env"))

let set env key value =
  let maybetbl = find env key in
  match maybetbl with
  | None -> lookup_raise key
  | Some tbl -> Hashtbl.replace tbl key value

let get env key =
  let maybetbl = find env key in
  match maybetbl with
  | None -> lookup_raise key
  | Some tbl -> Hashtbl.find tbl key

