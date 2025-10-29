(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Jacques-Henri Jourdan, INRIA Paris-Rocquencourt            *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 2.1 of   *)
(*  the License, or  (at your option) any later version.               *)
(*  This file is also distributed under the terms of the               *)
(*  INRIA Non-Commercial License Agreement.                            *)
(*                                                                     *)
(* *********************************************************************)
module SSet = Set.Make(String)

type 'id fun_declarator_cxt =
  | Decl_ident
  | Decl_other
  | Decl_fun of (unit -> unit)
  | Decl_krfun of 'id

module IdContext : sig 
  type identifier_type
  type pre_name
  type t
  val save_context : unit -> unit -> unit
  val set_id_type : pre_name -> identifier_type -> unit
  val declare_varname : pre_name -> unit
  val declare_typename : pre_name -> unit
  val create : string list -> t

end = struct 
  type identifier_type = 
  | VarId
  | TypedefId
  | OtherId

  type pre_name = string * identifier_type ref * Cabs.loc

  type t = SSet.t ref

  let save_context_st : (unit -> (unit -> unit)) ref = ref (fun _ -> assert false)
  
  let save_context () = !save_context_st ()

  let set_id_type : pre_name -> identifier_type -> unit = 
    fun (_, r, _) t -> r := t

  let declare_varname_st : (string -> unit) ref = ref (fun _ -> assert false)

  let declare_varname : pre_name -> unit =
    fun (s, _, _) -> !declare_varname_st s

  let declare_typename_st : (string -> unit) ref = ref (fun _ -> assert false)

  let declare_typename : pre_name -> unit =
    fun (s, _, _) -> !declare_typename_st s

  let create (builtins : string list) : t = 
    let types_context = ref (SSet.of_list builtins) in
    save_context_st := begin fun () ->
      let save = !types_context in
      fun () -> types_context := save
    end;
    declare_varname_st := begin fun id ->
      types_context := SSet.remove id !types_context
    end;
    declare_typename_st := begin fun id ->
      types_context := SSet.add id !types_context
    end;
    types_context
end

type pre_quote_elt = 
| PreQuot of string * Lexing.position
| PreAnti of string * Lexing.position

type pre_quote = pre_quote_elt list

module Bracketed : sig
    type t 
    val create : Lexing.position -> t
    exception Ill_bracketed of string
    val enter_quot : t -> unit
    val exit_quot : t -> unit
    val enter_anti : ?start_p:Lexing.position -> t -> unit
    val exit_anti : ?start_p:Lexing.position -> t -> unit
    val add_char : t -> char -> unit
    val add_string : t -> string -> unit
    val finalize : t -> pre_quote
    val outermost : t -> bool 
    val in_anti : t -> bool

end = struct
  type t = 
    { mutable quot_level : int
    ; mutable anti_level : int 
    ; acc : Buffer.t
    ; mutable acc_start : Lexing.position
    ; mutable quote : pre_quote  }

  exception Ill_bracketed of string
    
  let create start_p = 
    { quot_level= 1
    ; anti_level= 0
    ; acc= Buffer.create 100
    ; acc_start= start_p
    ; quote= [] }

  let end_span bk f =     
    bk.quote <- (f ()) :: bk.quote;
    Buffer.clear bk.acc

  let end_quot bk = 
    end_span bk (fun () -> 
      PreQuot (Buffer.contents bk.acc, bk.acc_start))

  let end_anti bk = 
    end_span bk (fun () ->
      PreAnti (Buffer.contents bk.acc, bk.acc_start))
  
  let enter_quot bk =
    match bk.quot_level, bk.anti_level with 
    | 0, _ ->
      raise (Ill_bracketed ("attempted to open second well-bracketed" ^ 
                            "expression where one is permitted"))
    | _, ai when ai > 0 ->
      raise (Ill_bracketed ("cannot reenter quotation within antiquotation"))
    | i, 0 when i > 0 -> begin
      bk.quot_level <- i + 1 end
    | _ ->
      assert false

  let exit_quot bk =
    match bk.quot_level, bk.anti_level with 
    | 0, _ -> 
      raise (Ill_bracketed "excess closing bracket")
    | 1, ai when ai = 0 -> begin 
      bk.quot_level <- 0;
      end_quot bk end
    | qi, ai when qi > 0 && ai = 0 -> begin
      bk.quot_level <- qi - 1 end
    | qi, ai when qi > 0 && ai > 0 -> 
      raise (Ill_bracketed "unclosed antiquotation")
    | _, _ ->
      assert false
  
  let enter_anti ?start_p bk =
    match bk.quot_level, bk.anti_level, start_p with 
    | 0, _, _ ->
      raise (Ill_bracketed "antiquotation outside of quotation")
    | qi, 0, Some p when qi > 0 -> begin 
      end_quot bk;
      bk.anti_level <- 1;
      bk.acc_start <- p end
    | qi, ai, _ when qi > 0 && ai > 0 -> begin
      bk.anti_level <- ai + 1 end
    | _, _, _ ->
      assert false
      
  let outermost bk = 
    match bk.quot_level, bk.anti_level with 
    | qi, 1 when qi > 0 ->
      true 
    | 1, 0 ->
      true
    | _ ->
      false

  let exit_anti ?start_p bk = 
    match bk.quot_level, bk.anti_level, start_p with 
    | 0, _, _ ->
      raise (Ill_bracketed "antiquotation outside of quotation")
    | qi, 0, _  when qi > 0 ->
      raise (Ill_bracketed "excess closing bracket")
    | qi, 1, Some p when qi > 0 -> begin 
      end_anti bk;
      bk.anti_level <- 0;
      bk.acc_start <- p end
    | qi, ai, _ when qi > 0 && ai > 1 -> begin 
      bk.anti_level <- ai - 1 end
    | _, _, _ -> 
      assert false

  let in_anti bk = bk.anti_level > 0

  let guard_extension bk fn = fun x ->
    match bk.quot_level, bk.anti_level with 
    | 0, _ ->
      raise (Ill_bracketed ("attempted to open second well-bracketed" ^ 
                            "expression where one is permitted"))
    | qi, ai when qi > 0 && ai >= 0 ->
      fn x
    | _, _ ->
      assert false

  let add_char : t -> char -> unit = 
    fun bk ->
      guard_extension bk (Buffer.add_char bk.acc)

  let add_string : t -> string -> unit =
    fun bk ->
      guard_extension bk (Buffer.add_string bk.acc)

  let finalize bk = 
    match bk.quot_level, bk.anti_level with 
    | 1, 0 -> begin 
      bk.quot_level <- 0;
      end_quot bk; 
      bk.quote <- List.rev bk.quote; 
      bk.quote end
    | 0, 0 -> 
      bk.quote
    | qi, ai when qi > 0 && ai > 0 ->
      raise (Ill_bracketed "unclosed antiquotation")
    | _, _ ->
      assert false 
end

type bracket_type = Rocq | Iris 

and bracket_scope = (Lexing.lexbuf option * pre_quote * bracket_type)

let map_scope rocq_opt iris_opt = function 
  | Rocq -> rocq_opt 
  | Iris -> iris_opt

module Decl = struct 
  type 'a t = 
  | Parameters of 'a
  | Refined_by of 'a
  | Exists of 'a
  | Let of 'a
  | Constraints of 'a
  | Args of 'a
  | Requires of 'a
  | Ensures of 'a
  | Inv_vars of 'a
  | Annot_args of 'a
  | Tactics of 'a
  | Lemmas of 'a
  | Typedef of 'a
  | Size of 'a
  | Tagged_union of 'a
  | Union_tag of 'a
  | Field of 'a
  | Global of 'a
  | Returns of 'a
  | Manual_proof of 'a
  | Annot of 'a
  | Unfold_order of 'a
  | Immovable of 'a
  | Asrt of 'a
  | Trust_me of 'a
  | Skip of 'a
  | Block of 'a
  | Full_block of 'a
  | Inlined of 'a

  let of_string : string -> 'a -> 'a t = function 
    | "parameters"    -> (fun x -> Parameters x)
    | "refined_by"    -> (fun x -> Refined_by x)
    | "exists"        -> (fun x -> Exists x)
    | "let"           -> (fun x -> Let x)
    | "constraints"   -> (fun x -> Constraints x)
    | "args"          -> (fun x -> Args x)
    | "requires"      -> (fun x -> Requires x)
    | "ensures"       -> (fun x -> Ensures x)
    | "inv_vars"      -> (fun x -> Inv_vars x)
    | "annot_args"    -> (fun x -> Annot_args x)
    | "tactics"       -> (fun x -> Tactics x)
    | "lemmas"        -> (fun x -> Lemmas x)
    | "typedef"       -> (fun x -> Typedef x)
    | "size"          -> (fun x -> Size x)
    | "tagged_union"  -> (fun x -> Tagged_union x)
    | "union_tag"     -> (fun x -> Union_tag x)
    | "field"         -> (fun x -> Field x)
    | "global"        -> (fun x -> Global x)
    | "returns"       -> (fun x -> Returns x)
    | "manual_proof"  -> (fun x -> Manual_proof x)
    | "annot"         -> (fun x -> Annot x)
    | "unfold_order"  -> (fun x -> Unfold_order x)
    | "immovable"     -> (fun x -> Immovable x)
    | "asrt"          -> (fun x -> Asrt x)
    | "trust_me"      -> (fun x -> Trust_me x)
    | "skip"          -> (fun x -> Skip x)
    | "block"         -> (fun x -> Block x)
    | "full_block"    -> (fun x -> Full_block x)
    | "inlined"       -> (fun x -> Inlined x)
    | _               -> assert false (* impossible as called from Lexer.mll *)
end

type 'a arguments = 
  | Zero 
  | One of 'a * string list
  | Many of ('a * string list) list

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

type anno_context = 
  | Lemmas_Cxt 
  | Function_Cxt 
  | Struct_Defn_Cxt
  | Struct_Memb_Cxt
  | Loop_Cxt
  | StmtExpr_Cxt 
  | Global_Cxt