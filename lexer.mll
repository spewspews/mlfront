{
module P = Parser
module U = Util

let keywords =
  U.hashtbl_create 293 [
    "and", P.AND;
    "asr", P.ASR;
    "begin", P.BEGIN;
    "end", P.END;
    "false", P.FALSE;
    "fun", P.FUN;
    "function", P.FUNCTION;
    "if", P.IF;
    "in", P.IN;
    "land", P.LAND;
    "let", P.LET;
    "lnot", P.LNOT;
    "lor", P.LOR;
    "lsl", P.LSL;
    "lsr", P.LSR;
    "match", P.MATCH;
    "rec", P.REC;
    "true", P.TRUE;
    "type", P.TYPE;
    "with", P.WITH;
    "of", P.OF;
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
let integer = '-'? ['0'-'9' '_']+
let float = '-'? ['0'-'9' '_']+ ('.' ['0'-'9' '_']+)? (['e' 'E'] ['0'-'9' '_']+)?

rule token = parse
  | "()" { P.UNIT_VAL }
  | "->" { P.ARROW }
  | "::" { P.COLONCOLON }
  | "<=" { P.LESSEQ }
  | "<>" { P.NOTEQ }
  | ">=" { P.GREATEREQ }
  | "[]" { P.EMPTY }
  | "[|" { P.LBRACKARR }
  | "|]" { P.RBRACKARR }
  | '(' { P.LPAREN }
  | ')' { P.RPAREN }
  | '*' { P.MUL }
  | '+' { P.PLUS }
  | ',' { P.COMMA }
  | '-' { P.MINUS }
  | '/' { P.DIV }
  | ':' { P.COLON }
  | ';' { P.SEMICOLON }
  | '<' { P.LESS }
  | '=' { P.EQ }
  | '[' { P.LBRACK }
  | '\'' { P.SINGLEQ }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | ']' { P.RBRACK }
  | '{' { P.LBRACE }
  | '|' { P.ALT }
  | '}' { P.RBRACE }
  | eof { P.EOF }
  | ws+ { token lexbuf }
  | integer
    {
      let s = Lexing.lexeme lexbuf in
      match int_of_string_opt s with
      | None -> P.ERROR (Printf.sprintf "Invalid integer: %s\n" s)
      | Some i -> P.INT i
    }
  | float
    {
      let s = Lexing.lexeme lexbuf in
      match float_of_string_opt s with
      | None -> P.ERROR (Printf.sprintf "Invalid float: %s\n" s)
      | Some f -> P.FLOAT f
    }
  | '"'
    {
      Buffer.clear string_buf;
      match string lexbuf with
      | Ok s -> P.STRING s
      | Error s -> P.ERROR s
    }
  | lower alnum*
    {
      let Lexing.{pos_lnum=lnum} = Lexing.lexeme_start_p lexbuf in
      let n = Lexing.lexeme lexbuf in
      let s = U.{n; lnum} in
      U.last_sym := s;
      (*Printf.eprintf "read sym %s\n" n;*)
      match Hashtbl.find_opt keywords n with
      | Some kw -> kw
      | None -> P.LOWER_NAME s
    }
  | upper alnum*
    {
      let Lexing.{pos_lnum=lnum} = Lexing.lexeme_start_p lexbuf in
      let s = U.{n = (Lexing.lexeme lexbuf); lnum} in
      U.last_sym := s;
      P.UPPER_NAME s
    }
and string = parse
  | '"' { Ok (Buffer.contents string_buf) }
  | eof { Error "Eof within string literal" }
  | '\\' _
    {
      Buffer.add_char string_buf (escape_char (Lexing.lexeme_char lexbuf 1));
      string lexbuf
    }
  | _ { Buffer.add_char string_buf (Lexing.lexeme_char lexbuf 0); string lexbuf }
