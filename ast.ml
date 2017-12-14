type sym = {n : string; lnum : int}

type exp = unit

type binding = {sym : sym; exp : exp; is_rec : bool}
