type sym = {n : string; lnum : int}

type pattern =
| Name of sym
| Tuple of pattern list
| Type of {constr : sym; body : pattern}

type exp =
| Int of int
| Float of float
| String of string
| Char of char
| Plus of exp * exp
| Bool of bool
| Error of string

type bind =
| Value of {bound : pattern; exp : exp}
| Function of {sym : sym; args : sym list; exp : exp}

type mutual_bind = {binds : bind list; is_rec : bool}

type prog = mutual_bind list

let last_sym = ref {n="No Sym"; lnum=0}
