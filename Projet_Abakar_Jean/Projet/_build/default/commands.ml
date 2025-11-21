open Filesystem

(* mkdir *)
let mkdir (name_str : string) (fs : filesystem) : filesystem =
  let new_name = Name name_str in
  if name_exists new_name fs.root.children then fs
  else
    let new_dir = { name = new_name; children = [] } in
    let new_node = Dir new_dir in
    let updated_root = { fs.root with children = fs.root.children @ [new_node] } in
    { fs with root = updated_root }


(*touch*)
let touch (name_str : string) (fs : filesystem) : filesystem =
  let new_name = Name name_str in
  if name_exists new_name fs.root.children then fs
  else
    let new_file = { name = new_name; content = "" } in
    let new_node = File new_file in
    let updated_root = { fs.root with children = fs.root.children @ [new_node] } in
    { fs with root = updated_root }

(* ls *)
let ls fs  =
  let children = fs.root.children in
  List.iter (fun node ->
    match node with
    | File f ->
        let Name fname = f.name in
        Printf.printf "%s\n" fname
    | Dir d ->
        let Name dname = d.name in
        Printf.printf "%s\n" dname
  ) children;
  flush stdout

(* write *)
let write name_str content  fs =
  let target_name = Name name_str in
  match find_file target_name fs.root.children with
  | None -> fs
  | Some file ->
      let new_content =
        if file.content = "" then content
        else file.content ^ content
      in
      let updated_file = { file with content = new_content } in
      let updated_children = replace_file file updated_file fs.root.children in
      let updated_root = { fs.root with children = updated_children } in
      { fs with root = updated_root }

let cat (name_str : string) (fs : filesystem) : unit =
  let target_name = Name name_str in
  match find_file target_name fs.root.children with
  | None -> ()
  | Some file ->
      Printf.printf "%s\n%!" file.content


let name_exists name nodes =
  List.exists (fun node ->
    match node with
    | File f -> f.name = name
    | Dir d -> d.name = name
  ) nodes

let rec find_file name nodes =
  match nodes with
  | [] -> None
  | File f :: _ when f.name = name -> Some f
  | _ :: rest -> find_file name rest

let rec replace_file old_file new_file  nodes  =
  match nodes with
  | [] -> []
  | File f :: rest when f.name = old_file.name ->
      File new_file :: rest
  | node :: rest -> node :: replace_file old_file new_file rest



let name_exists name nodes =
  List.exists (fun node ->
    match node with
    | File f -> f.name = name
    | Dir d -> d.name = name
  ) nodes

let rec find_file name nodes =
  match nodes with
  | [] -> None
  | File f :: _ when f.name = name -> Some f
  | _ :: rest -> find_file name rest

let rec replace_file old_file new_file nodes =
  match nodes with
  | [] -> []
  | File f :: rest when f.name = old_file.name ->
      File new_file :: rest
  | node :: rest -> node :: replace_file old_file new_file rest

