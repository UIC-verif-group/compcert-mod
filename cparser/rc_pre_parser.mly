%{
  open Rc_pre_parser_aux

%}

%token<string * Cabs.loc> TY_NAME IDENT TY_NAME_OR_IDENT
%token<(string * Cabs.loc) list> PATTERN
%token<Cabs.loc> GLOBAL OWN SHARE FRAC

%start<unit> 
  parameters_anno 
  refined_by_anno 
  exists_anno 
  let_anno 
  constraints_anno 
  args_anno 
  requires_anno 
  ensures_anno
  inv_vars_anno
  annot_args_anno
  tactics_anno
  lemmas_anno
  typedef_anno
  size_anno
  tagged_union_anno
  union_tag_anno
  field_anno
  global_anno
  returns_anno
  manual_proof_anno
  annot_anno
  unfold_order_anno

%inline ioption(X):
| /* nothing */
    { None }
| x = X
    { Some x }

option(X):
  o = ioption(X)
    { o }

(* [optional(X, Y)] is equivalent to [X? Y]. However, by inlining
   the two possibilies -- either [X Y] or just [Y] -- we are able
   to give more meaningful syntax error messages. [optional(X, Y)]
   itself is usually NOT inlined, as that would cause a useless
   explosion of cases. *)
optional(X, Y):
  ioption(X) Y {}

as_ident:
| IDENT
| TY_NAME_OR_IDENT
  {}

as_ty_name:
| TY_NAME
| TY_NAME_OR_IDENT
  {}

pattern:
| LPAREN RPAREN 
| as_ident 
| LPAREN as_ident as_ident as_ident* RPAREN 
  {}

dot_ident:
| DOTONE as_ident

rocq_expr:
| as_ident 
| rocq_term
  {}

named_rocq_expr:
| as_ident COLON rocq_expr

named_rocq_expr_parens:
| LPAREN named_rocq_expr RPAREN

named_full_type_expr:
| as_ident COLON full_type_expr

ptr_kind:
| OWN
| SHARE
| FRAC rocq_expr

constr: 
| iris_term
| EXISTS as_ident optional(COLON rocq_expr, DOTONE constr)
| rocq_expr
| GLOBAL as_ident COLON full_type_expr
| ptr_kind as_ident COLON full_type_expr
| as_ident COLON full_type_expr
  {}

atomic_type_expr:
| as_ident
| rocq_term
| rocq_expr AT atomic_type_expr
| as_ty_name LANGLE type_args RANGLE
| DOTTHREE
| LPAREN full_type_expr RPAREN 

cstring_type_expr:
| atomic_type_expr
| cstring_type_expr AMPERSAND constr

full_type_expr:
| cstring_type_expr
| EXISTS pattern optional(COLON rocq_expr, DOTONE full_type_expr)

type_expr_arg:
| full_type_expr
| LAMBDA pattern optional(COLON rocq_expr, DOTONE type_expr_arg)
  {}

type_args_tail:
|
| COMMA type_expr_arg type_args_tail

type_args:
|
| type_expr_arg type_args_tail

(* specific entry points *)

parameters_anno: named_rocq_expr

refined_by_anno: named_rocq_expr

exists_anno: named_rocq_expr

let_anno:
| as_ident optional(COLON rocq_expr, EQUAL rocq_expr)

constraints_anno: constr

arg_anno: full_type_expr

requires_anno: constr

ensures_anno: constr

inv_vars_anno: named_full_type_expr

annot_args_anno:
| INTEGER COLON INTEGER rocq_expr

tactics_anno: RAW_TEXT

lemmas_anno: RAW_TEXT

typedef_anno: named_full_type_expr

size_anno: rocq_expr

tagged_union_anno: rocq_expr

union_tag_anno: 
| as_ident named_rocq_expr_parens*

field_anno: full_type_expr

global_anno: full_type_expr

returns_anno: full_type_expr

manual_proof_anno:
| as_ident dot_ident* COLON as_ident COMMA as_ident
  {}

annot_anno: RAW_TEXT

unfold_order_anno: INTEGER





