type name = Name of string
(** Le nom ne contient pas de '/' *)

type path = name list

type file = { name: name; content: string }

type directory = { name: name; children: node list }
and node = File of file | Dir of directory

type filesystem = { root: directory; current_path: path }



(* Les déclarations des deux types suivants sont mutuellement récursifs:
  - le type directory des répertoires est un enregistrement dont le champ children contient une liste de node
  - le type node est un type de données algébrique dont un des constructeurs encapsule une valeur de type directory
*)


let init () =
  let root_dir = { name = Name "/"; children = [] } in
  (* Le répertoire racine est le seul nom de répertoire qui peut contenir un '/' *)
  { root = root_dir; current_path = [] }


let path_to_string path =
  let names = List.map (fun (Name s) -> s) path in
  String.concat "/" names

