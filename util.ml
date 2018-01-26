module A = Ast

let in_files : string list ref = ref []

let last_sym = ref A.{n="No Sym"; lnum=0}

let hashtbl_create size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) init;
  tbl
