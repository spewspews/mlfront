val lexer:
  string -> char Stream.t -> unit -> Grammar.token * Lexing.position * Lexing.position

val parse:
  (unit -> Grammar.token * Lexing.position * Lexing.position) -> Ast.t
