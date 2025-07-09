{
open Lexing
module Rc_pp = Rc_pre_parser
module Rc_pp_aux = Rc_pre_parser_aux

let currentLoc = 
  let nextident = ref 0 in 
  let getident () = 
    nextident := !nextident + 1;
    !nextident
  in 
  fun lb ->
    let p = Lexing.lexeme_start_p lb in 
    Cabs.({ lineno   = p.Lexing.pos_lnum
          ; filename = p.Lexing.pos_fname
          ; byteno   = p.Lexing.pos_cnum
          ; ident    = getident () })

let fatal_error lb fmt =
  Diagnostics.fatal_error
    (lb.lex_curr_p.pos_fname,lb.lex_curr_p.pos_lnum) fmt

module SSet = Set.Make(String)

let lexicon : SSet.t = SSet.of_list [
                            "global"
                          ; "own"
                          ; "shr"
                          ; "frac" ]

let clash name = SSet.mem name lexicon

}

let udot = 
    ['\x00' - '\x7F'] 
  | ['\xC0' - '\xDF'] ['\x80' - '\xBF'] 
  | ['\xE0' - '\xEF'] ['\x80' - '\xBF'] ['\x80' - '\xBF'] 
  | ['\xF0' - '\xF7'] ['\x80' - '\xBF'] ['\x80' - '\xBF'] ['\x80' - '\xBF']

let digit = ['0'-'9']
let nondigit = ['_' 'a'-'z' 'A'-'Z']
let ident_base = nondigit ( digit | nondigit ) *
let integer = digit +

rule token = parse
  | "&" ident_base "*"     { fatal_error lexbuf "invalid RefinedC identifier" }
  | "&" (ident_base as n)  { if clash n then fatal_error lexbuf 
                               "reserved keyword used as RefinedC identifier";
                             Rc_pp.TY_NAME (n, currentLoc lexbuf) }
  | "void*" as n           { Rc_pp.IDENT (n, currentLoc lexbuf) }
  | (ident_base as n) "*"  { fatal_error lexbuf "invalid RefinedC identifier" }
  | ident_base as n        { if clash_n then fatal_error lexbuf 
                               "reserved keyword used as RefinedC identifier";
                             Rc_pp.TY_NAME_OR_IDENT (n, currentLoc lexbuf) }
  | "global"               { Rc_pp.GLOBAL (currentLoc lexbuf) }
  | "own"                  { Rc_pp.OWN (currentLoc lexbuf) }
  | "shr"                  { Rc_pp.SHARE (currentLoc lexbuf) }
  | "frac"                 { Rc_pp.FRAC (currentLoc lexbuf) }
  | "!{"                   { Rc_pp.LBANGBRACK (currentLoc lexbuf) }
  | "..."                  { Rc_pp.DOTTHREE (currentLoc lexbuf) }
  | "."                    { Rc_pp.DOTONE (currentLoc lexbuf) }
  | "{"                    { Rc_pp.LBRACK (currentLoc lexbuf) }
  | "}"                    { Rc_pp.RBRACK (currentLoc lexbuf) }
  | "["                    { Rc_pp.LBRACE (currentLoc lexbuf) }
  | "]"                    { Rc_pp.RBRACE (currentLoc lexbuf) }
  | "<"                    { Rc_pp.LANGLE (currentLoc lexbuf) }
  | ">"                    { Rc_pp.RANGLE (currentLoc lexbuf) }
  | "@"                    { Rc_pp.AT (currentLoc lexbuf) }
  | "∃"                    { Rc_pp.EXISTS (currentLoc lexbuf) }
  | ":"                    { Rc_pp.COLON (currentLoc lexbuf) }
  | "("                    { Rc_pp.LPAREN (currentLoc lexbuf) }
  | ")"                    { Rc_pp.RPAREN (currentLoc lexbuf) }
  | "λ"                    { Rc_pp.LAMBDA (currentLoc lexbuf) }
  | ","                    { Rc_pp.COMMA (currentLoc lexbuf) }
  | udot as c              { Rc_pp.UCHAR (c, currentLoc lexbuf) }
(* 
rule parameters_anno = parse 
  | ""                     { let name = ident lexbuf in 
                             }

rule refined_by_anno = parse 
| "" {} 

rule exists_anno = parse 
| "" {} 

rule let_anno = parse 
| "" {} 

rule constraints_anno = parse 
| "" {} 

rule args_anno = parse
| "" {} 

rule requires_anno = parse 
| "" {} 

rule ensures_anno = parse 
| "" {} 

rule inv_vars_anno = parse
| "" {} 

rule annot_args_anno = parse 
| "" {} 

rule tactics_anno = parse 
| "" {} 

rule lemmas_anno = parse 
| "" {} 

rule typedef_anno = parse 
| "" {} 

rule size_anno = parse 
| "" {} 

rule tagged_union_anno = parse 
| "" {} 

rule union_tag_anno = parse 
| "" {} 

rule field_anno = parse 
| "" {} 

rule global_anno = parse 
| "" {} 

rule returns_anno = parse 
| "" {} 

rule manual_proof_anno = parse 
| "" {} 

rule annot_anno = parse 
| "" {} 

rule unfold_order_anno = parse
| "" {}  *)

{

  let invoke_rc_pre_parser loc text decl buffer = 
    let lexbuf = Lexing.from_string text in 
    lexbuf.lex_curr_p <- 
      { lexbuf.lex_curr_p with 
        pos_fname = loc.filename
      ; pos_lnum = loc.lineno
      ; pos_cnum = loc.byteno };
    let module I = Rc_pp.MenhirInterpreter in
    let module M = Rc_pp.Incremental in
    let (checkpoint, supplier) = 
      let (lexer, parser) = begin match decl with 
      | Parameters _   -> (parameters_anno, M.parameters_anno)
      | Refined_by _   -> (refined_by_anno, M.refined_by_anno)
      | Exists _       -> (exists_anno, M.exists_anno)
      | Let _          -> (let_anno, M.let_anno)
      | Constraints _  -> (constraints_anno, M.constraints_anno)
      | Args _         -> (args_anno, M.args_anno)
      | Requires _     -> (requires_anno, M.requires_anno)
      | Ensures _      -> (ensures_anno, M.ensures_anno)
      | Inv_vars _     -> (inv_vars_anno, M.inv_vars_anno)
      | Annot_args _   -> (annot_args_anno, M.annot_args_anno)
      | Tactics _      -> (tactics_anno, M.tactics_anno)
      | Lemmas _       -> (lemmas_anno, M.lemmas_anno)
      | Typedef _      -> (typedef_anno, M.typedef_anno)
      | Size _         -> (size_anno, M.size_anno)
      | Tagged_union _ -> (tagged_union_anno, M.tagged_union_anno)
      | Union_tag _    -> (union_tag_anno, M.union_tag_anno)
      | Field _        -> (field_anno, M.field_anno)
      | Global _       -> (global_anno, M.global_anno)
      | Returns _      -> (returns_anno, M.returns_anno)
      | Manual_proof _ -> (manual_proof_anno, M.manual_proof_anno)
      | Annot _        -> (annot_anno, M.annot_anno)
      | Unfold_order _ -> (unfold_order_anno, M.unfold_order_anno)
      | Immovable _ | Asrt _ | Trust_me _ | Skip _ 
      | Block _ | Full_block _ | Inlined _ ->
        assert false (* cannot be invoked by `annot` *)
      end in (
        parser lexbuf.lex_curr_p
      , I.lexer_lexbuf_to_supplier token lexbuf ) in 
    let succeed () = () in
    let fail checkpoint = 
      Diagnostics.fatal_error_raw "%s" (ErrorReports.report text !buffer checkpoint)
    in
    I.loop_handle succeed fail supplier checkpoint
  
  let compute_buffer tokens transf = fun () ->
    let loop t = Buf_cons (t, Lazy.from_fun go) in
    loop (transf (Queue.pop tokens)) 

  let annot : Rc_pp_aux.decl -> Rc_pp_aux.arguments : buffer = 
    fun decl args -> begin 
      let tokens = Queue.create () in
      let buffer = ref ErrorReports.Zero in
      let compute_buffer = compute_buffer tokens from_pre in
      let args = 
        match args with 
        | Zero -> begin
          Queue.push (Rc_pre_parser.ZERO_ARG_DECL decl);
          [] end
        | One a -> begin
          Queue.push (Rc_pre_parser.ONE_ARG_DECL decl);
          [a] end
        | Many aa -> begin
          Queue.push (Rc_pre_parser.MANY_ARG_DECL decl);
          aa end 
      in 
      let rec push_all = function 
      | (loc, s) :: nil  -> begin
        invoke_rc_pre_parser loc s decl buffer;
        Queue.push Rc_pre_parser.ARG_END end
      | (loc, s) :: rest -> begin
        invoke_rc_pre_parser loc s decl buffer;
        Queue.push Rc_pre_parser.ARG_SEP;
        push_all rest end
      | nil               ->
        Queue.push Rc_pre_parser.ARG_END
      in 
      push_all args;
      Lazy.from_fun compute_buffer
    end

}