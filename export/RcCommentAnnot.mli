type inlined_code = {
  prelude : string list;
  section : string list;
  final : string list;
}
type t = {
  inlined : inlined_code;
  requires : string list;
  imports : (string * string) list;
  proof_imports : (string * string) list;
  code_imports : (string * string) list;
  context : string list;
  typedefs : RcDefn.typedef list;
}
val parse : string list -> t
