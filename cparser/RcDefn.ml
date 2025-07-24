open Earley_core
open Earley
open Extra


let default_unfold_order : int = 100

let annot_lemmas : string list -> string list =
  List.map (Printf.sprintf "all: try by apply: %s; solve_goal.")

let function_annot : rc_attr list -> function_annot = fun attrs ->
  let parameters = ref [] in
  let args = ref [] in
  let exists = ref [] in
  let returns = ref None in
  let requires = ref [] in
  let ensures = ref [] in
  let tactics = ref [] in
  let proof = ref Proof_normal in
  let inlined = ref false in

  let nb_attrs = ref 0 in
  let handle_attr ({rc_attr_id = id; _} as attr) =
    let error msg =
      invalid_annot id.loc (Printf.sprintf "Annotation [%s] %s." id.elt msg)
    in
    if !inlined then error "should be the only attribute";
    incr nb_attrs;
    match (parse_attr attr, !returns) with
    | (_                  , _   ) when !proof = Proof_skipped ->
        error "a skipped function should not have other annotations";
    | (Annot_skip         , _   ) ->
        if !proof <> Proof_normal then error "proof mode already specified";
        if !nb_attrs <> 1 then error "other annotations are given";
        proof := Proof_skipped
    | (Annot_trust_me     , _   ) ->
        if !proof <> Proof_normal then error "proof mode already specified";
        proof := Proof_trusted
    | (Annot_manual(cfg)  , _   ) ->
        if !proof <> Proof_normal then error "proof mode already specified";
        proof := Proof_manual(cfg)
    | (Annot_parameters(l), _   ) -> parameters := !parameters @ l
    | (Annot_args(l)      , _   ) -> args := !args @ l
    | (Annot_returns(ty)  , None) -> returns := Some(ty)
    | (Annot_returns(_)   , _   ) -> error "already specified"
    | (Annot_requires(l)  , _   ) -> requires := !requires @ l
    | (Annot_ensures(l)   , _   ) -> ensures := !ensures @ l
    | (Annot_exist(l)     , _   ) -> exists := !exists @ l
    | (Annot_annot_args(_), _   ) -> () (* Handled separately. *)
    | (Annot_tactics(l)   , _   ) -> tactics := !tactics @ l
    | (Annot_inlined      , _   ) ->
        if !nb_attrs <> 1 then error "should be the only attribute";
        proof := Proof_inlined;
        inlined := true
    | (_                  , _   ) -> error "is invalid for a function"
  in
  List.iter handle_attr attrs;

  (* When no annotations are given, the function is skipped. *)
  if !nb_attrs = 0 then proof := Proof_skipped;

  { fa_parameters = !parameters
  ; fa_args       = !args
  ; fa_returns    = Option.get (Ty_params("void", [])) !returns
  ; fa_exists     = !exists
  ; fa_requires   = !requires
  ; fa_ensures    = !ensures
  ; fa_tactics    = !tactics
  ; fa_proof_kind = !proof }

let function_annot_args : rc_attr list -> annot_arg list = fun attrs ->
  let annot_args = ref [] in

  let handle_attr ({rc_attr_id = id; _} as attr) =
    if id.elt <> "annot_args" then () else
    match parse_attr attr with
    | Annot_annot_args(l) -> annot_args := !annot_args @ l
    | _                   -> assert false (* Unreachable. *)
  in
  List.iter handle_attr attrs;

  !annot_args

let member_annot : rc_attr list -> member_annot = fun attrs ->
  let annot = ref MA_none in

  let handle_attr ({rc_attr_id = id; _} as attr) =
    let error msg =
      invalid_annot id.loc (Printf.sprintf "Annotation [%s] %s." id.elt msg)
    in
    match (parse_attr attr, !annot) with
    | (Annot_field(ty)   , MA_none) -> annot := MA_field(ty)
    | (Annot_field(_)    , _      ) -> error "already specified"
    | (Annot_union_tag(s), MA_none) -> annot := MA_utag(s)
    | (Annot_union_tag(_), _      ) -> error "already specified"
    | (_                 , _      ) -> error "is invalid for a field"
  in
  List.iter handle_attr attrs; !annot

let default_basic_struct_annot : basic_struct_annot =
  { st_parameters   = []
  ; st_refined_by   = []
  ; st_exists       = []
  ; st_lets         = []
  ; st_constrs      = []
  ; st_size         = None
  ; st_typedef      = None
  ; st_immovable    = false
  ; st_unfold_order = default_unfold_order }

(* Decides whether the annotation on the structure should lead to the
   definition of a RefinedC type. *)
let basic_struct_annot_defines_type : basic_struct_annot -> bool = fun annot ->
  annot.st_refined_by <> [] || annot.st_typedef <> None

let struct_annot : rc_attr list -> struct_annot = fun attrs ->
  let parameters = ref [] in
  let refined_by = ref [] in
  let exists = ref [] in
  let lets = ref [] in
  let constrs = ref [] in
  let size = ref None in
  let ptr = ref None in
  let immovable = ref false in
  let tagged_union = ref None in
  let unfold_order = ref None in

  let handle_attr ({rc_attr_id = id; _} as attr) =
    let error msg =
      invalid_annot id.loc (Printf.sprintf "Annotation [%s] %s." id.elt msg)
    in
    let check_and_set r v =
      if !r <> None then error "already specified";
      r := Some(v)
    in
    match (parse_attr attr, !tagged_union) with
    (* Tagged union stuff. *)
    | (Annot_tagged_union(e), None   ) -> tagged_union := Some(e)
    | (Annot_tagged_union(e), Some(_)) -> error "already specified"
    (* Normal struct stuff. *)
    | (Annot_parameters(l)  , None   ) -> parameters := !parameters @ l
    | (Annot_refined_by(l)  , None   ) -> refined_by := !refined_by @ l
    | (Annot_exist(l)       , None   ) -> exists := !exists @ l
    | (Annot_lets(l)        , None   ) -> lets := !lets @ l
    | (Annot_constraint(l)  , None   ) -> constrs := !constrs @ l
    | (Annot_size(s)        , None   ) -> check_and_set size s
    | (Annot_typedef(e)     , None   ) -> check_and_set ptr e
    | (Annot_immovable      , None   ) ->
        if !immovable then error "already specified";
        immovable := true
    | (Annot_unfold_order(i), None   ) ->
         begin
           match !unfold_order with
           | Some _ ->  error "already specified"
           | None -> unfold_order := Some(i)
         end
    | (Annot_parameters(_)  , _      )
    | (Annot_refined_by(_)  , _      )
    | (Annot_exist(_)       , _      )
    | (Annot_constraint(_)  , _      )
    | (Annot_size(_)        , _      )
    | (Annot_typedef(_)     , _      )
    | (Annot_immovable      , _      ) ->
        error "is invalid for tagged unions"
    | (_                    , _      ) ->
        error "is invalid for a struct or a tagged union"
  in
  List.iter handle_attr attrs;

  match !tagged_union with
  | Some(e) -> SA_tagged_u(e)
  | None    ->
  let basic_annot =
    { st_parameters   = !parameters
    ; st_refined_by   = !refined_by
    ; st_exists       = !exists
    ; st_lets         = !lets
    ; st_constrs      = !constrs
    ; st_size         = !size
    ; st_typedef      = !ptr
    ; st_immovable    = !immovable
    ; st_unfold_order = Option.get default_unfold_order !unfold_order }
  in
  SA_basic(basic_annot)

let loop_annot : rc_attr list -> bool option * state_descr = fun attrs ->
  let exists = ref [] in
  let constrs = ref [] in
  let vars = ref [] in
  let full_block = ref None in

  let handle_attr ({rc_attr_id = id; _} as attr) =
    let error msg =
      invalid_annot id.loc (Printf.sprintf "Annotation [%s] %s." id.elt msg)
    in
    let set_full_block b =
      match !full_block with
      | Some(_) -> error "mode already specified"
      | None    -> full_block := Some(b)
    in
    match parse_attr attr with
    | Annot_exist(l)      -> exists := !exists @ l
    | Annot_constraint(l) -> constrs := !constrs @ l
    | Annot_inv_vars(l)   -> vars := !vars @ l
    | Annot_block         -> set_full_block false
    | Annot_full_block    -> set_full_block true
    | _                   -> error "is invalid (wrong kind)"
  in
  List.iter handle_attr attrs;

  (!full_block, {sd_exists = !exists; sd_constrs = !constrs; sd_inv_vars = !vars})

let raw_expr_annot : rc_attr list -> raw_expr_annot option = fun attrs ->
  let error msg =
    invalid_annot_no_pos (Printf.sprintf "Expression annotation %s." msg)
  in
  match attrs with
  | []      -> None
  | [attr]  -> begin
     match parse_attr attr with
     | Annot_annot(s) -> Some(RawExprAnnot_annot s)
     | _              -> error "is invalid (wrong kind)"
    end
  | _       ->
     let filtered_attrs = List.filter (fun attr -> parse_attr attr <> Annot_assert) attrs in
     if List.length attrs = List.length filtered_attrs then
       (* if this is not an assert_annotation, only one attribute is allowed *)
       error "carries more than one attribute"
     else
       let (full, sd) = loop_annot filtered_attrs in
       if full <> None then
         error "has block annotation"
       else
         Some (RawExprAnnot_assert(sd))

let global_annot : rc_attr list -> global_annot option = fun attrs ->
  let typ = ref None in
  let parameters = ref [] in

  let handle_attr ({rc_attr_id = id; _} as attr) =
    let error msg =
      invalid_annot id.loc (Printf.sprintf "Annotation [%s] %s." id.elt msg)
    in
    match (parse_attr attr, !typ) with
    | (Annot_global(e)    , None) -> typ := Some e
    | (Annot_parameters(l), _   ) -> parameters := !parameters @ l
    | (Annot_global(_)    , _   ) -> error "already specified"
    | (_                  , _   ) -> error "is invalid for a global"
  in
  List.iter handle_attr attrs;

  match !typ with
  | Some(ty) -> Some {ga_parameters = !parameters; ga_type = ty}
  | None -> None
