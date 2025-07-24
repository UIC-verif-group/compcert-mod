%{
  module Rc_pp_aux = Rc_pre_parser_aux

%}

%token<string * Cabs.loc> TY_NAME IDENT TY_NAME_OR_IDENT UCHAR QUOT
%token<Cabs.loc> GLOBAL OWN SHARE FRAC DOTTHREE DOTONE LANGLE RANGLE AT EXISTS
                 COLON LPAREN RPAREN LAMBDA COMMA
%token<Rc_pp_aux.quote> PRE_BRACKETED_ROCQ PRE_BRACKETED_IRIS
%token<int * Cabs.loc> INTEGER
%token EOF ROCQ_WELL_BR_OPEN ROCQ_WELL_BR_CLOS IRIS_WELL_BR_OPEN
       IRIS_WELL_BR_CLOS ANTI_OPEN ANTI_CLOS 

%start<unit> 
  let_anno 
  annot_args_anno
  union_tag_anno
  manual_proof_anno
  named_rocq_expr
  constr
  full_type_expr
  named_full_type_expr
  raw_text
  rocq_expr
  integer

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
| LPAREN as_ident COMMA as_ident (COMMA as_ident)* RPAREN 
  {}

dot_ident:
| DOTONE as_ident
  {}

rocq_term:
| ROCQ_WELL_BR_OPEN rocq_term_contents ROCQ_WELL_BR_CLOS
  {}

rocq_term_contents:
| (* nothing *)
| QUOT
| QUOT ANTI_OPEN full_type_expr ANTI_CLOS rocq_term_contents
  {}

iris_term:
| IRIS_WELL_BR_OPEN iris_term_contents IRIS_WELL_BR_CLOS
  {}

iris_term_contents:
| (* nothing *)
| QUOT 
| QUOT ANTI_OPEN full_type_expr ANTI_CLOS iris_term_contents
  {}

rocq_expr:
| as_ident 
| rocq_term
  {}

named_rocq_expr:
| as_ident COLON rocq_expr
  {}

named_rocq_expr_parens:
| LPAREN named_rocq_expr RPAREN
  {}

named_full_type_expr:
| as_ident COLON full_type_expr
  {}

ptr_kind:
| OWN
| SHARE
| FRAC rocq_expr
  {}

constr: 
| iris_term
| EXISTS as_ident optional(COLON rocq_expr, DOTONE constr)
| rocq_expr
| GLOBAL as_ident COLON full_type_expr
| ptr_kind as_ident COLON full_type_expr
| as_ident COLON full_type_expr
  {}

atomic_type_expr:
| rocq_expr option(AT atomic_type_expr)
| as_ty_name LANGLE type_args RANGLE
| DOTTHREE
| LPAREN full_type_expr RPAREN 
  {}

cstring_type_expr:
| cstring_type_expr AMPERSAND constr
| atomic_type_expr
  {}

full_type_expr:
| EXISTS pattern optional(COLON rocq_expr, DOTONE full_type_expr)
| cstring_type_expr
  {}

type_expr_arg:
| full_type_expr
| LAMBDA pattern optional(COLON rocq_expr, DOTONE type_expr_arg)
  {}

type_args_tail:
|
| COMMA type_expr_arg type_args_tail
  {}

type_args:
|
| type_expr_arg type_args_tail
  {}

let_anno:
| as_ident optional(COLON rocq_expr, EQUAL rocq_expr)
  {}

annot_args_anno:
| INTEGER COLON INTEGER rocq_expr
  {}

union_tag_anno: 
| as_ident named_rocq_expr_parens*
  {}

manual_proof_anno:
| as_ident dot_ident* COLON as_ident COMMA as_ident
  {}





