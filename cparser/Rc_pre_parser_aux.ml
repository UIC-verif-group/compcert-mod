
type decl = 
  | Parameters of Cabs.loc
  | Refined_by of Cabs.loc
  | Typedef of Cabs.loc
  | Size of Cabs.loc
  | Exists of Cabs.loc
  | Let of Cabs.loc
  | Constraints of Cabs.loc
  | Immovable of Cabs.loc
  | Tagged_union of Cabs.loc
  | Union_tag of Cabs.loc
  | Field of Cabs.loc
  | Global of Cabs.loc
  | Args of Cabs.loc
  | Requires of Cabs.loc
  | Returns of Cabs.loc
  | Ensures of Cabs.loc
  | Annot of Cabs.loc
  | Asrt of Cabs.loc
  | Inv_vars of Cabs.loc
  | Annot_args of Cabs.loc
  | Tactics of Cabs.loc
  | Lemmas of Cabs.loc
  | Trust_me of Cabs.loc
  | Skip of Cabs.loc
  | Manual_proof of Cabs.loc
  | Block of Cabs.loc
  | Full_block of Cabs.loc
  | Inlined of Cabs.loc
  | Unfold_order of Cabs.loc

type arguments = 
  | Zero 
  | One of (Cabs.loc * string list)
  | Many of ((Cabs.loc * string list) list)

type 'a quote_elt = 
  | Plain of string 
  | Anti of 'a

type 'a quote = 'a quote_elt list

type ident   = string
type pattern = ident list

type coq_term  = type_expr quote

and  iris_term = type_expr quote

and coq_expr =
  | Coq_ident of string
  | Coq_all   of coq_term

and constr =
  | Constr_Iris  of iris_term
  | Constr_exist of string * coq_expr option * constr
  | Constr_own   of string * ptr_kind * type_expr
  | Constr_val   of string * type_expr
  | Constr_Coq   of coq_expr
  | Constr_glob  of string * type_expr

and ptr_kind = Own | Shr | Frac of coq_expr

and type_expr =
  | Ty_refine of coq_expr * type_expr
  | Ty_dots
  | Ty_exists of pattern * coq_expr option * type_expr
  | Ty_constr of type_expr * constr
  | Ty_params of ident * type_expr_arg list
  | Ty_Coq    of coq_expr

and type_expr_arg =
  | Ty_arg_expr   of type_expr
  | Ty_arg_lambda of pattern * coq_expr option * type_expr_arg

type annot_arg = int * int * coq_expr