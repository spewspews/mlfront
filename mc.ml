open Util

let opts = []
let argv0 = Sys.argv.(0)
let usage = Printf.sprintf "Usage: %s [ files... ]" argv0

let add_input file = in_files := file :: !in_files

let compile file in_chan = begin
  let lexbuf = Lexing.from_channel in_chan in
  ()
end

let compile_file file = begin
  let in_chan = open_in file in
  compile file in_chan
end

let () = begin
  Arg.parse opts add_input usage;
  match List.rev !in_files with
  | [] -> compile "<stdin>" stdin
  | files -> List.iter compile_file files;
  exit 0;
end
