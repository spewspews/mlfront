type stream = {
  t :char Stream.t;
  pos_fname : string;
  mutable pos_lnum : int;
  mutable pos_bol : int;
  mutable pos_cnum : int;
}

let peek {t} = Stream.peek t

let next stream =
  let c = Stream.next stream.t in
  stream.pos_cnum <- stream.pos_cnum + 1;
  match c with
  | '\n' ->
    stream.pos_lnum <- stream.pos_lnum + 1;
    stream.pos_bol <- stream.pos_cnum;
    c
  | _ -> c

let lexing_position {pos_fname; pos_lnum; pos_bol; pos_cnum} =
  Lexing.{pos_fname; pos_lnum; pos_bol; pos_cnum}

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_alpha_num = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false

let is_ws = function
  | ' ' | '\n' | '\t' -> true
  | _ -> false

let skip_ws in_stream =
  let rec skip () =
    match peek in_stream with
    | None -> ()
    | Some c ->
      if is_ws c then
        let _ = next in_stream in
        skip ()
      else ()
  in
  skip ()

let next_word =
  let buf = Buffer.create 50 in
  let contents () =
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s
  in
  let rec next_w in_stream =
    match peek in_stream with
    | None -> contents ()
    | Some c ->
      if is_ws c then contents () else begin
        Buffer.add_char buf c;
        let _ = next in_stream in
        next_w in_stream
      end
  in
  next_w

let parse =
  MenhirLib.Convert.Simplified.traditional2revised Grammar.ml

let lexer fname in_stream () =
  let in_stream = {t=in_stream; pos_fname=fname; pos_lnum=0; pos_bol=0; pos_cnum=0} in
  let skip () = skip_ws in_stream in
  let word () = next_word in_stream in
  skip ();
  let spos = lexing_position in_stream in
  match peek in_stream with
  | None -> (Grammar.EOF, spos, spos)
  | Some c ->
    if is_alpha c then
      let s = word () in
      let epos = lexing_position in_stream in
      (Grammar.SYM Ast.{name=s; lnum=spos.Lexing.pos_lnum}, spos, epos)
    else (Grammar.SYM Ast.{name="nope"; lnum=0}, spos, spos)
