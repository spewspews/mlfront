type sym = {n : string; lnum : int}

type exp =
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Plus of exp * exp

type bind = {sym : sym; args : sym list; exp : exp}

type mutual_bind = {binds : bind list; is_rec : bool}
