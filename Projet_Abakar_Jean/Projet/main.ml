let () =
  let fs = Filesystem.init () in
  Repl.loop fs