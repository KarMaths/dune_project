type name = Name of string
(** Le nom ne contient pas de '/' *)

type path = name list

type file = { name: name; content: string }

type directory = { name: name; children: node list }
and node = File of file | Dir of directory

type filesystem = { root: directory; current_path: path }


val path_to_string : path -> string

val init : unit -> filesystem
