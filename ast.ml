type sym = {n : string; lnum : int}

type exp = unit

type bind = {sym : sym; exp : exp}

type mutual_bind = {binds : bind list; is_rec : bool}
