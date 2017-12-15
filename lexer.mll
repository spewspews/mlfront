{
  module L = Lexing
  module P = Grammar
  module A = Ast
}

let ws = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z' '_']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule token = parse
| ws+ { token lexbuf }
| '\n' { L.new_line lexbuf; token lexbuf }
| "let" { P.LET }
| "=" { P.EQ }
| "FOO" { P.FOO }
| "rec" { P.REC }
| "and" { P.AND }
| ( lower | '_' ) alnum+
  {
    let L.{pos_lnum=lnum} = L.lexeme_start_p lexbuf in
    P.VAL_SYM A.{
      n = (Lexing.lexeme lexbuf);
      lnum;
    }
  }
