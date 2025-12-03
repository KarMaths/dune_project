(*projet_Abakar_Jean*)
(*  JEAN Karl Philippe
    Abakar Adoum
    581C
*)
open Filesystem

(* --- Fonctions auxiliaires --- *)
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

let rec search_file_or_dir nom liste =
  match liste with
  | [] -> None 
  | File f :: _ when f.name = nom -> Some (File f )
  | Dir d :: _ when d.name = nom -> Some (Dir d )
  | _ :: rest -> search_file_or_dir nom rest


let rec remplace_file  old_file new_file liste_node =
  match liste_node with
  | [] -> []
  | File f :: rest when f.name = old_file.name ->
      File new_file :: rest
  | x :: rest -> 
      x :: remplace_file  old_file  new_file rest

let supprimer nom liste = 
  List.filter(fun node ->
    match node with
    | File f -> f.name <> nom
    | Dir d -> d.name <> nom
    
    ) liste

let rec parcours_chemin chemin dir = 
  match chemin with
  | [] -> Some dir
  | nom::reste -> 
      match search_file_or_dir nom dir.children with 
      | Some (Dir d) -> parcours_chemin reste d
      | _-> None

let name_to_string (Name s) = s

(* MKDIR--> rajoute dans le répertoire courant un répertoire *)
let mkdir nomdudir fs =
  if name_exists nomdudir (fs.root.children) then begin
    prerr_string "nom du dossier deja exister ";
    fs 
  end
  else
    let nouv_dir = { name = nomdudir; children = [] } in
    let nouv_node = Dir nouv_dir in
    let maj_root = { fs.root with children = fs.root.children @ [nouv_node] } in
    { fs with root = maj_root }

(* TOUCH --> rajoute dans le répertoire courant un fichier *)
let touch nomdufichier fs =
  if name_exists nomdufichier (fs.root.children) then begin
    prerr_string " nom du fichier deja exister"; 
    fs
  end
  else
    let nouv_fichier = { name = nomdufichier; content = "" } in
    let nouv_node = File nouv_fichier in
    let maj_root = { fs.root with children = fs.root.children @ [nouv_node] } in
    { fs with root = maj_root }

(* LS --> affiche la liste des répertoires et des fichiers dans le répertoire courant *)
let ls fs =
  let children = fs.root.children in
  List.iter (fun node ->
    match node with
    | File f ->
        Printf.printf "%s\n" (name_to_string f.name)
    | Dir d ->
        Printf.printf "%s\n" (name_to_string d.name)
  ) children;
  flush stdout

(* WRITE --> placer dans le contenu du fichier nomdufichier du répertoire courant contenu*)
let write nomdufichier contenu fs =
  match search_file nomdufichier fs.root.children with
  | None -> 
      prerr_string "";
      fs
  | Some f ->
      let nouv_content =
        if f.content = "" then contenu
        else f.content ^ contenu
      in
      let maj_file = { f with content = nouv_content } in
      let maj_children = 
        remplace_file f maj_file  fs.root.children in
      let maj_root = { fs.root with children = maj_children } in
      { fs with root = maj_root }

(* CAT --> affiche le contenu du contenu du fichier du répertoire courant*)
let cat nomdufichier fs =
  match search_file nomdufichier fs.root.children with
  | None -> ()
  | Some file ->
      Printf.printf "%s\n%!" file.content
  

(* CD -->  permet de se déplacer dans l’arborescence en suivant le chemin relatif nomchemin pour modifier le chemin courant*)
let cd nomchemin fs =
  match search_file_or_dir nomchemin fs.root.children with
  | None -> 
      prerr_string "aucun repertoire de ce nom \n";
      fs 
  | Some (Dir d ) -> 
      {fs with root = d}
  | Some (File _) -> 
      prerr_string "il s'agit d'un fichier\n";
      fs

(* CD -->  permet de se déplacer dans l’arborescence en suivant le chemin relatif nomchemin pour modifier le chemin courant*)
let cd nomchemin fs =
  match parcours_chemin nomchemin fs.root with
  | None -> 
      prerr_string " chemin invalide\n";
      fs
  | Some d -> 
      {fs with root = d}


(* FIND --> qui affiche tous les fichiers 'nomdufichier' dans la sous-arborescence partant du répertoire courant.*)
let find nomdufichier fs = 
  let rec findDir nom fs_root dir =
    List.iter(fun node -> 
      match node with
      | File f -> 
          if f.name = nom then 
            Printf.printf "%s/%s\n"
              fs_root
              (name_to_string f.name)
      | Dir d ->
          
          let nouv_dir = fs_root ^ (name_to_string d.name) in
          findDir nom nouv_dir d
      ) dir.children
    in
    let debut_chemin = "/" ^ (name_to_string fs.root.name) in 
    findDir nomdufichier debut_chemin fs.root

(* RM --> qui permet de supprimer un fichier ou un répertoire du répertoire courant. *)
let rm nomdelelement fs =
  match search_file_or_dir nomdelelement fs.root.children with
  | None -> 
    prerr_string " ";
    fs
  | Some _ -> 
      let maj_children = supprimer nomdelelement fs.root.children in 
      let maj_root = {fs.root with children = maj_children } in 
      {fs with root = maj_root}
  
(* MV --> qui permet de déplacer un fichier ou un réper-toire  dans le chemin relatif *)
let mv nomdelelement nomduchemin fs = 
  match search_file_or_dir nomdelelement fs.root.children with
  | None -> 
      prerr_string " aucun fichier de ce nom";
      fs
  | Some node ->
      match parcours_chemin nomduchemin fs.root with
      | None ->
          prerr_string " aucun repertoire ce nom";
          fs
      | Some destination -> 
          let nomelt = match node with
            | File f -> f.name
            | Dir d -> d.name
          in
          if name_exists nomelt destination.children then begin
            prerr_string " fichier deja exitant";
            fs
          end
          else
            let supprime_nomdelement = supprimer nomdelelement fs.root.children in
            let ajoutDansDestination = destination.children @ [node] in 
            let maj_root = {fs.root with children =  ajoutDansDestination} in 
            { fs with root = maj_root }


