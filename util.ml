type sym = {n : string; lnum : int}

let argv0 = Sys.argv.(0)

let in_files : string list ref = ref []

let last_sym = ref {n="No Sym"; lnum=0}

let error {n; lnum} fmt =
  let f msg = Printf.eprintf "%s:%d: %s: near symbol %s" argv0 lnum msg n in
  Printf.ksprintf f fmt

let hashtbl_create size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) init;
  tbl

let quote {n} = "\"" ^ n ^ "\""
