
type decl = 
  | Parameters of Cabs.loc
  | Refined_by of Cabs.loc
  | Exists of Cabs.loc
  | Let of Cabs.loc
  | Constraints of Cabs.loc
  | Args of Cabs.loc
  | Requires of Cabs.loc
  | Ensures of Cabs.loc
  | Inv_vars of Cabs.loc
  | Annot_args of Cabs.loc
  | Tactics of Cabs.loc
  | Lemmas of Cabs.loc
  | Typedef of Cabs.loc
  | Size of Cabs.loc
  | Tagged_union of Cabs.loc
  | Union_tag of Cabs.loc
  | Field of Cabs.loc
  | Global of Cabs.loc
  | Returns of Cabs.loc
  | Manual_proof of Cabs.loc
  | Annot of Cabs.loc
  | Unfold_order of Cabs.loc
  | Immovable of Cabs.loc
  | Asrt of Cabs.loc
  | Trust_me of Cabs.loc
  | Skip of Cabs.loc
  | Block of Cabs.loc
  | Full_block of Cabs.loc
  | Inlined of Cabs.loc

let decl_of_string : string -> Cabs.loc -> decl = function 
  | "parameters"    -> (fun loc -> Parameters loc)
  | "refined_by"    -> (fun loc -> Refined_by loc)
  | "exists"        -> (fun loc -> Exists loc)
  | "let"           -> (fun loc -> Let loc)
  | "constraints"   -> (fun loc -> Constraints loc)
  | "args"          -> (fun loc -> Args loc)
  | "requires"      -> (fun loc -> Requires loc)
  | "ensures"       -> (fun loc -> Ensures loc)
  | "inv_vars"      -> (fun loc -> Inv_vars loc)
  | "annot_args"    -> (fun loc -> Annot_args loc)
  | "tactics"       -> (fun loc -> Tactics loc)
  | "lemmas"        -> (fun loc -> Lemmas loc)
  | "typedef"       -> (fun loc -> Typedef loc)
  | "size"          -> (fun loc -> Size loc)
  | "tagged_union"  -> (fun loc -> Tagged_union loc)
  | "union_tag"     -> (fun loc -> Union_tag loc)
  | "field"         -> (fun loc -> Field loc)
  | "global"        -> (fun loc -> Global loc)
  | "returns"       -> (fun loc -> Returns loc)
  | "manual_proof"  -> (fun loc -> Manual_proof loc)
  | "annot"         -> (fun loc -> Annot loc)
  | "unfold_order"  -> (fun loc -> Unfold_order loc)
  | "immovable"     -> (fun loc -> Immovable loc)
  | "asrt"          -> (fun loc -> Asrt loc)
  | "trust_me"      -> (fun loc -> Trust_me loc)
  | "skip"          -> (fun loc -> Skip loc)
  | "block"         -> (fun loc -> Block loc)
  | "full_block"    -> (fun loc -> Full_block loc)
  | "inlined"       -> (fun loc -> Inlined loc)
  | _               -> assert false (* impossible as called from Lexer.mll *)

type arguments = 
  | Zero 
  | One of (Cabs.loc * string list)
  | Many of ((Cabs.loc * string list) list)

type 'a quote_elt = 
  | Quot of string 
  | Anti of 'a

type 'a quote = 'a quote_elt list

type rocq_term  = type_expr quote

and  iris_term = type_expr quote

and rocq_expr =
  | Rocq_ident of string
  | Rocq_other of rocq_term

and constructor =
  | Iris_cons  of iris_term
  | Exist_cons of string * rocq_expr option * constructor
  | Own_cons   of string * ptr_kind * type_expr
  | Val_cons   of string * type_expr
  | Rocq_cons  of rocq_expr
  | Glob_cons  of string * type_expr

and ptr_kind = Own | Shr | Frac of rocq_expr

and type_expr =
  | Refine_ty of rocq_expr * type_expr
  | Dots_ty
  | Exists_ty of string list * rocq_expr option * type_expr
  | Cons_ty   of type_expr * constructor
  | Params_ty of string * type_expr_arg list
  | Rocq_ty   of rocq_expr

and type_expr_arg =
  | Expr_arg_ty of type_expr
  | Lam_arg_ty  of string list * rocq_expr option * type_expr_arg

type arg_annotation = int * int * rocq_expr

type let_annotation = string * rocq_expr option * rocq_expr

type 'a named = string * 'a

type typedef_annotation = 
  { td_id           : string
  ; td_refinements  : rocq_expr named list
  ; td_parameters   : rocq_expr named list
  ; td_body         : type_expr
  ; td_immovable    : bool
  ; td_unfold_order : int }

type manual_proof = string * string * string

type proof_kind = 
  | Normal_proof
  | Skipped_proof
  | Trusted_proof
  | Manual_proof of manual_proof
  | Inlined_proof

type function_annotation = 
  { fa_parameters : rocq_expr named list
  ; fa_args       : type_expr list 
  ; fa_returns    : type_expr 
  ; fa_exists     : rocq_expr named list
  ; fa_requires   : constructor list 
  ; fa_ensures    : constructor list 
  ; fa_tactics    : constructor list 
  ; fa_proof_kind : proof_kind }

type member_annotation = 
  | MA_None 
  | MA_Field of type_expr
  | MA_utag  of string * rocq_expr named list

type basic_struct_annotation = 
  { st_parameters   : rocq_expr named list
  ; st_refined_by   : rocq_expr named list
  ; st_exists       : rocq_expr named list
  ; st_lets         : let_annotation list 
  ; st_constructors : constructor list 
  ; st_size         : rocq_expr option
  ; st_typedef      : type_expr named option
  ; st_immovable    : bool
  ; st_unfold_order : int }

type struct_annotation = 
  | SA_Union 
  | SA_Basic of basic_struct_annotation 
  | SA_Tagged_Union of rocq_expr

type state_description = 
  { sd_exists       : rocq_expr named list 
  ; sd_constructors : constructor list 
  ; sd_inv_vars     : rocq_expr named list }

type raw_expr_annotation = 
  | RawExprAnno_Annot  of string 
  | RawExprAnno_Assert of state_description

type global_annotation = 
  { ga_parameters : rocq_expr named list 
  ; ga_type       : type_expr }

type rc_annotation = 
  | Parameters_Anno   of rocq_expr named list
  | Refined_By_Anno   of rocq_expr named list 
  | Typedef_Anno      of type_expr named
  | Size_Anno         of rocq_expr
  | Exist_Anno        of rocq_expr named list 
  | Lets_Anno         of let_annotation list
  | Constraint_Anno   of constructor list
  | Tagged_Union_Anno of rocq_expr
  | Union_Tag_Anno    of string * rocq_expr named list
  | Field_Anno        of type_expr
  | Global_Anno       of type_expr
  | Args_Anno         of type_expr list
  | Requires_Anno     of constructor list
  | Returns_Anno      of type_expr
  | Ensures_Anno      of constructor list
  | Annot_Anno        of string
  | Inv_Vars_Anno     of type_expr named list
  | Annot_Args_Anno   of arg_annotation list
  | Tactics_Anno      of string list 
  | Manual_Anno       of manual_proof
  | Unfold_Order_Anno of int