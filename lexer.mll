{

module P = Parser
module A = Ast
module U = Util

let keywords = U.hashtbl_create 293 [
  "and", P.AND;
  "let", P.LET;
  "rec", P.REC;
  "true", P.TRUE;
  "false", P.FALSE;
]

let string_buf = Buffer.create 200

let escape_char = function
| 'b' -> '\b'
| 'n' -> '\n'
| 'r' -> '\r'
| 't' -> '\t'
| c -> c

}

let ws = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule token = parse
| eof { P.EOF }
| ws+ { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| '=' { P.EQ }
| '+' { P.PLUS }
| ('0' ['o' 'O' 'x' 'X' 'b' 'B'])? ['0'-'9' '_']+ {
    P.INT (int_of_string (Lexing.lexeme lexbuf))
  }
| '"' {
    Buffer.clear string_buf;
    match string lexbuf with
    | Ok s -> P.STRING s
    | Error s -> P.ERROR s
  }
| '\'' { P.SINGLEQ }
| "[]" { P.EMPTY }
| "()" { P.UNIT_VAL }
| "::" { P.COLONCOLON }
| ':' { P.COLON }
| "->" { P.ARROW }
| lower alnum* {
    let Lexing.{pos_lnum=lnum} = Lexing.lexeme_start_p lexbuf in
    let n = Lexing.lexeme lexbuf in
    let s = A.{n; lnum} in
    A.last_sym := s;
    match Hashtbl.find_opt keywords n with
    | Some kw -> kw
    | None -> P.LOWER_NAME s
  }
| upper alnum* {
    let Lexing.{pos_lnum=lnum} = Lexing.lexeme_start_p lexbuf in
    let s = A.{n = (Lexing.lexeme lexbuf); lnum} in
    A.last_sym := s;
    P.UPPER_NAME s
  }
and string = parse
| '"' { Ok (Buffer.contents string_buf) }
| eof { Error "Eof within string literal" }
| '\\' _ {
    Buffer.add_char string_buf (escape_char (Lexing.lexeme_char lexbuf 1));
    string lexbuf
  }
| _ { Buffer.add_char string_buf (Lexing.lexeme_char lexbuf 0); string lexbuf }
