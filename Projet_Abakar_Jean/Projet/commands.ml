(*projet_Abakar_Jean*)
(*  JEAN Karl Philippe
    Abakar Adoum
    581C
*)
open Filesystem

(* Fonctions auxiliaires*)
let name_exists nom liste_node =
  List.exists (fun node ->
    match node with
    | File f -> f.name = nom
    | Dir d -> d.name = nom
  ) liste_node

let rec search_file nom liste_node =
  match liste_node with
  | [] -> None
  | File f :: _ when f.name = nom -> Some f
  | _ :: rest -> search_file nom rest

let rec remplace_file old_file new_file liste_node =
  match liste_node with
  | [] -> []
  | File f :: rest when f.name = old_file.name ->
      File new_file :: rest
  | node :: rest -> node :: remplace_file old_file new_file rest

(* mkdir *)
let mkdir nom fs =
  let nouv_nom = nom in
  if name_exists nouv_nom fs.root.children then fs
  else
    let nouv_dir = { name = nouv_nom; children = [] } in
    let nouv_node = Dir nouv_dir in
    let maj_root = { fs.root with children = fs.root.children @ [nouv_node] } in
    { fs with root = maj_root }

(* touch *)
let touch nom fs =
  let nouv_nom = Name nom in
  if name_exists nouv_nom fs.root.children then fs
  else
    let nouv_file = { name = nouv_nom; content = "" } in
    let nouv_node = File nouv_file in
    let maj_root = { fs.root with children = fs.root.children @ [nouv_node] } in
    { fs with root = maj_root }

(* ls *)
let ls fs =
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
let write nom content fs =
  let nom_cible =  nom in
  match search_file nom_cible fs.root.children with
  | None -> fs
  | Some file ->
      let nouv_content =
        if file.content = "" then content
        else file.content ^ content
      in
      let maj_file = { file with content = nouv_content } in
      let maj_children = remplace_file file maj_file fs.root.children in
      let maj_root = { fs.root with children = maj_children } in
      { fs with root = maj_root }

(* cat *)
let cat name_str fs =
  let nom_cible = Name name_str in
  match search_file nom_cible fs.root.children with
  | None -> ()
  | Some file ->
      Printf.printf "%s\n%!" file.content
