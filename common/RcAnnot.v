Require Import ZArith.

(* OCaml's string type. *)
Parameter string : Type.

(* RefinedC annotation types. *)
Parameter function_annot : Type.
Parameter state_descr : Type.
Parameter raw_expr_annot : Type.
Parameter global_annot : Type.
Parameter struct_annot : Type.
Parameter member_annot : Type.

Parameter default_function_annot : function_annot.
Parameter default_struct_annot : struct_annot.
Parameter default_union_annot : struct_annot.
Parameter default_member_annot : member_annot.

Inductive expr_annot :=
  | ExprAnnot_annot  (s : string)
  | ExprAnnot_assert (i : Z).
