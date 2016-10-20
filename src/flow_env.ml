module SMap = Map.Make(String)

type scope = {
  scope_name: string;
  mutable modules: scope SMap.t;
  mutable types: Types.type_declaration SMap.t;
}

type t = {
  mutable stack: scope list;
}

let enter_module env name =
  let new_scope = {
    scope_name = name;
    modules = SMap.empty;
    types = SMap.empty;
  } in
  let scope = List.hd env.stack in
  scope.modules <- SMap.add name new_scope scope.modules;
  env.stack <- (new_scope :: env.stack)

let leave_module env =
  env.stack <- List.tl env.stack

let mk_new () =
  let global = {
    scope_name = "glob";
    modules = SMap.empty;
    types = SMap.empty;
  } in
  {
    stack = [global];
  }

let map_get k map = if SMap.mem k map then Some (SMap.find k map) else None

let lookup env get =
  let rec lookup stack =
    match stack with
    | [] -> None
    | scope :: stack ->
        (match get scope with
        | None -> lookup stack
        | x -> x)
  in
  lookup env.stack

let path_to_list path =
  let rec loop path acc =
    match path with
    | Path.Pident id -> (Ident.name id) :: acc
    | Path.Pdot (path, name, _) -> loop path (name :: acc)
    | Path.Papply _ -> loop path acc (* TODO: handle *)
  in
  loop path []

let get_module env path =
  let rec helper scope path =
    match path with
    | [] -> Some scope
    | name :: path ->
        if SMap.mem name scope.modules
        then helper (SMap.find name scope.modules) path
        else None
  in
  let path = path_to_list path in
  let first = lookup env (fun scope -> map_get (List.hd path) scope.modules) in
  match first with
  | Some scope -> helper scope (List.tl path)
  | None -> None

let add_type env name type_decl =
  let scope = List.hd env.stack in
  scope.types <- SMap.add name type_decl scope.types

let get_type env path =
  let get_fn name scope = map_get name scope.types in
  match path with
  | Path.Pident id -> lookup env (get_fn (Ident.name id))
  | Path.Pdot (path, name, _) ->
      begin match get_module env path with
      | Some scope -> map_get name scope.types
      | None -> None
      end
  | Path.Papply _ -> None (* Not supported *)
