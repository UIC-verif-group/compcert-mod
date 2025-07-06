
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