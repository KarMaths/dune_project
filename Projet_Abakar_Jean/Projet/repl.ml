let split_ws (s : string) : string list =
  let str_l = String.split_on_char ' ' s in
  List.filter (fun t -> t <> "") str_l

let rec loop (fs : Filesystem.filesystem) : unit =
  (* Affiche le chemin courant en couleur  *)
  Printf.printf "\027[32m%s\027[0m> %!" (Filesystem.path_to_string fs.current_path);
  match read_line () with
  | exception End_of_file -> print_newline ()
  | line ->
      let tokens = split_ws line in
      begin
        match tokens with
        | "quit" :: _ | "exit" :: _ ->
            print_endline "Exit"
        | [] -> loop fs

        (* Insérer les commandes ici *)

        | command :: _ ->
            Printf.eprintf "Command not found: %s\n%!" command;
            loop fs
      end
