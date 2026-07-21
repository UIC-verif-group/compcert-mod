type t = { lineno : int;
   filename: string;
   byteno: int;
   ident : int;
 }

let location_to_string loc =
  Printf.sprintf "%d:%d" loc.lineno loc.byteno

type 'a located = { elt : 'a ; loc : t }

let none = { lineno = 0; filename = ""; byteno = 0; ident = 0 }

let make file line col id = { lineno = line; filename = file; byteno = col; ident = id }