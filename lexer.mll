{
  module L = Lexing
  module P = Parser
  module A = Ast
}

let ws = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z' '_']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule token = parse
| eof { P.EOF }
| ws+ { token lexbuf }
| '\n' { L.new_line lexbuf; token lexbuf }
| "let" { P.LET }
| "=" { P.EQ }
| "rec" { P.REC }
| "and" { P.AND }
| '+' { P.PLUS }
| lower alnum*
  {
    let L.{pos_lnum=lnum} = L.lexeme_start_p lexbuf in
    P.SYM A.{
      n = (Lexing.lexeme lexbuf);
      lnum;
    }
  }
| ('0' ['o' 'O' 'x' 'X' 'b' 'B'])? ['0'-'9' '_']+
  { P.INT (int_of_string (Lexing.lexeme lexbuf)) }
