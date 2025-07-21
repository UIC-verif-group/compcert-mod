{
open Lexing
module Rc_pp = Rc_pre_parser
module Rc_pp_aux = Rc_pre_parser_aux

let (loc_of_lb, loc_of_start_p) = 
  let nextident = ref 0 in 
  let getident () = 
    next_ident := !next_ident + 1;
    !nextident
  in 
  let of_start_p p = 
    Cabs.({ lineno   = p.Lexing.pos_lnum;
            filename = p.Lexing.pos_fname;
            byteno   = p.Lexing.pos_cnum;
            ident    = getident () })
  in 
  let of_lb lb = 
    let p = Lexing.lexeme_start_p lb in 
    of_start_p p 
  in
  (of_lb, of_start_p)

let fatal_error lb fmt =
  Diagnostics.fatal_error
    (lb.lex_curr_p.pos_fname,lb.lex_curr_p.pos_lnum) fmt

module SSet = Set.Make(String)

let lexicon : SSet.t = SSet.of_list [
                          ; "void"
                          ; "global"
                          ; "own"
                          ; "shr"
                          ; "frac" ]

let clash name = SSet.mem name lexicon

module B = Rc_pp_aux.Bracket
}

let whitespace_char = [' ' '\t' '\n' '\011' '\012' '\r']

let udot = 
    ['\x00' - '\x7F'] 
  | ['\xC0' - '\xDF'] ['\x80' - '\xBF'] 
  | ['\xE0' - '\xEF'] ['\x80' - '\xBF'] ['\x80' - '\xBF'] 
  | ['\xF0' - '\xF7'] ['\x80' - '\xBF'] ['\x80' - '\xBF'] ['\x80' - '\xBF']

let non_ascii = 
    ['\xC0' - '\xDF'] ['\x80' - '\xBF'] 
  | ['\xE0' - '\xEF'] ['\x80' - '\xBF'] ['\x80' - '\xBF'] 
  | ['\xF0' - '\xF7'] ['\x80' - '\xBF'] ['\x80' - '\xBF'] ['\x80' - '\xBF']

let digit = ['0'-'9']
let nondigit = ['_' 'a'-'z' 'A'-'Z']
let ident_base = nondigit ( digit | nondigit ) *
let integer = digit +

rule rocq_term_quot bk = parse 
  | non_ascii as s         { B.add_string bk s; 
                             rocq_term_quot bk lexbuf }
  | "!{"                   { let start_p = Some lexbuf.lex_curr_p in
                             begin try B.enter_anti bk ~start_p with 
                             | Ill_bracketed es ->
                               fatal_error lexbuf es end;
                             rocq_term_anti bk lexbuf }
  | "{" as s               { begin try B.enter_quot bk with 
                             | Ill_bracketed es ->
                               fatal_error lexbuf es end;
                             B.add_string bk s;
                             rocq_term_quot bk lexbuf }
  | "}" as s               { let outer = B.outermost bk in 
                             B.exit_quot bk;
                             try begin 
                               if outer then begin
                                 B.finalize bk 
                               end else begin 
                                   B.add_string bk s;
                                   rocq_term_quot bk lexbuf end
                               end with 
                             | Ill_bracketed es ->
                               fatal_error lexbuf es }
  | eof                    { fatal_error lexbuf "reached eof inside nested brackets" }
  | _ as c                 { B.add_string bk c;
                             rocq_term_quot bk lexbuf }

and rocq_term_anti bk = parse
  | non_ascii as s         { B.add_string bk s;
                             rocq_term_anti bk lexbuf }
  | "{" as s               { begin try B.enter_anti with 
                             | Ill_bracketed es ->
                               fatal_error lexbuf es end;
                             B.add_string bk s;
                             rocq_term_anti bk lexbuf }
  | "}" as s               { let outer = B.outermost bk in 
                             let start_p = if outer then Some lexbuf.lex_curr_p
                                                         else None in
                             try begin 
                               B.exit_anti bk ~start_p;
                               if not outer then B.add_string bk s;
                               if outer then rocq_term_quot bk lexbuf
                                        else rocq_term_anti bk lexbuf
                             end with Ill_bracketed es ->
                               fatal_error lexbuf es }
  | eof                    { fatal_error lexbuf "reached eof inside nested brackets" }
  | _ as c                 { B.add_string bk c;
                             rocq_term_anti bk lexbuf }


rule iris_term_quot bk = parse 
  | non_ascii as s         { B.add_string bk s; 
                             iris_term_quot bk lexbuf }
  | "!{" as s              { let start_p = Some lexbuf.lex_curr_p in 
                             begin try B.enter_anti bk ~start_p with 
                             | Ill_bracketed es ->
                               fatal_error lexbuf es end;
                             iris_term_anti bk lexbuf }
  | "[" as s               { begin try B.enter_quot bk with 
                             | Ill_bracketed es ->
                               fatal_error lexbuf es end;
                             B.add_string bk s;
                             iris_term_quot bk lexbuf }
  | "]" as s               { try begin
                               let outer = B.outermost bk in
                               B.exit_quot bk;
                               if outer then begin
                                 B.finalize bk 
                               end else begin 
                                 B.add_string bk s;
                                 iris_term_quot bk lexbuf
                               end
                             end with Ill_bracketed es ->
                               fatal_error lexbuf es }
  | eof                    { fatal_error lexbuf "reached eof inside nested brackets" }
  _ as c                   { B.add_string bk c;
                             iris_term_quot bk lexbuf }

and iris_term_anti bk = parse
  | non_ascii as s         { B.add_string bk s;
                             iris_term_anti bk lexbuf }
  | "{"                    { begin try B.enter_anti bk with 
                             | Ill_bracketed es ->
                               fatal_error lexbuf es end;
                             B.add_string bk s;
                             iris_term_anti bk }
  | "}" as s               { try begin
                               let outer = B.outermost bk in
                               if not outer then B.add_string bk s;
                               let start_p = if outer then Some lexbuf.lex_curr_p
                                                      else None in
                               B.exit_anti bk ~start_p; 
                               if outer then iris_term_quot bk lexbuf
                                        else iris_term_anti bk lexbuf
                             end with Ill_bracketed es ->
                               fatal_error lexbuf es }
  | eof                    { fatal_error lexbuf "reached eof inside nested brackets" }
  | _ as c                 { B.add_string bk c;
                             iris_term_anti bk lexbuf }

rule tokenize = parse
  | eof                    { Rc_pp.EOF }
  | whitespace_char +      { tokenize lexbuf }
  | integer                { Rc_pp.INTEGER (int_of_string i, loc_of_lb lexbuf) }
  | "&" ident_base "*"     { fatal_error lexbuf "invalid RefinedC identifier" }
  | "&" (ident_base as n)  { if clash n then fatal_error lexbuf 
                               "reserved keyword used as RefinedC identifier";
                             Rc_pp.TY_NAME ("&" ^ n, loc_of_lb lexbuf) }
  | "void*" as n           { Rc_pp.IDENT (n, loc_of_lb lexbuf) }
  | (ident_base as n) "*"  { fatal_error lexbuf "invalid RefinedC identifier" }
  | ident_base as n        { if clash_n then fatal_error lexbuf 
                               "reserved keyword used as RefinedC identifier";
                             Rc_pp.TY_NAME_OR_IDENT (n, loc_of_lb lexbuf) }
  | "global"               { Rc_pp.GLOBAL (loc_of_lb lexbuf) }
  | "own"                  { Rc_pp.OWN (loc_of_lb lexbuf) }
  | "shr"                  { Rc_pp.SHARE (loc_of_lb lexbuf) }
  | "frac"                 { Rc_pp.FRAC (loc_of_lb lexbuf) }
  | "..."                  { Rc_pp.DOTTHREE (loc_of_lb lexbuf) }
  | "."                    { Rc_pp.DOTONE (loc_of_lb lexbuf) }
  | "<"                    { Rc_pp.LANGLE (loc_of_lb lexbuf) }
  | ">"                    { Rc_pp.RANGLE (loc_of_lb lexbuf) }
  | "@"                    { Rc_pp.AT (loc_of_lb lexbuf) }
  | "∃"                    { Rc_pp.EXISTS (loc_of_lb lexbuf) }
  | ":"                    { Rc_pp.COLON (loc_of_lb lexbuf) }
  | "("                    { Rc_pp.LPAREN (loc_of_lb lexbuf) }
  | ")"                    { Rc_pp.RPAREN (loc_of_lb lexbuf) }
  | "λ"                    { Rc_pp.LAMBDA (loc_of_lb lexbuf) }
  | ","                    { Rc_pp.COMMA (loc_of_lb lexbuf) }
  | "{"                    { let start_p = lexbuf.lex_curr_p in
                             let bk = B.create start_p in 
                             let qs = rocq_term_quot bk lexbuf in 
                             Rc_pp.PRE_BRACKETED_ROCQ qs }
  | "["                    { let start_p = lexbuf.lex_curr_p in 
                             let bk = B.create start_p in 
                             let qs = iris_term_quot bk lexbuf in
                             Rc_pp.PRE_BRACKETED_IRIS qs }
  | udot as c              { Rc_pp.UCHAR (c, loc_of_lb lexbuf) }

{

  let lexer tokens buffer : lexbuf -> Rc_pp.token = 
    let bk_cxt : bracket_scope list ref = ref nil in
    let push_yield e = Queue.push e tokens; e in
    let digest = function 
      | Rc_pp.PRE_BRACKETED_ROCQ qs -> begin 
        bk_cxt := (None, qs, Rocq) :: !bk_ckt;
        push_yield Rc_pp.ROCQ_WELL_BR_OPEN end
      | Rc_pp.PRE_BRACKETED_IRIS qs -> begin
        bk_cxt := (None, qs, Iris) :: !bk_cxt;
        push_yield Rc_pp.IRIS_WELL_BR_OPEN end
      | other -> push_yield other
    in
    let push_split scope rocq_opt iris_opt =
      push_yield (map_scope rocq_opt iris_opt scope)
    in
    let nest_lb_of_string p s = 
      let lb = Lexbuf.of_string s in 
      lb.lex_curr_p <- p; 
      p
    in
    fun base_lb ->
      match !bk_cxt with
      | (Some nest_lb, qs, scope) :: rest -> begin 
        match tokenize nest_lb with
        | Rc_pp.EOF -> begin 
          bk_cxt := (None, qs, scope) :: rest;
          push_yield Rc_pp.ANTI_CLOS end
        | e -> 
          digest e end
      | (None, PreQuot (s, pos) :: qs, scope) :: rest -> begin
        bk_cxt := (None, qs, scope) :: rest;
        push_yield (Rc_pp.QUOT (s, loc_of_start_p pos)) end
      | (None, PreAnti (s, pos) :: qs, scope) :: rest -> begin 
        let nest_lb = nest_lb_of_string pos s in
        bk_cxt := (Some nest_lb, qs, scope) :: rest;
        push_yield Rc_pp.ANTI_OPEN end
      | (None, nil, scope) :: rest -> begin 
        bk_cxt := rest;
        push_split scope Rc_pp.ROCQ_WELL_BR_CLOS Rc_pp.IRIS_WELL_BR_CLOS end
      | nil -> begin 
        let token = digest (tokenize base_lb) in
        let start_p = lexbuf.lex_start_p in 
        let end_p = lexbuf.lex_curr_p in 
        buffer := ErrorReports.update !buffer (start_p, end_p);
        token end

  let invoke_rc_pre_parser loc text decl tokens buffer = 
    let lexbuf = Lexing.from_string text in 
    lexbuf.lex_curr_p <- 
      { lexbuf.lex_curr_p with 
        pos_fname = loc.filename
      ; pos_lnum = loc.lineno
      ; pos_cnum = loc.byteno };
    let module I = Rc_pp.MenhirInterpreter in
    let module M = Rc_pp.Incremental in
    let checkpoint = 
      let parser = begin 
        match decl with 
        | Parameters _   -> M.named_rocq_expr
        | Refined_by _   -> M.named_rocq_expr
        | Exists _       -> M.named_rocq_expr
        | Let _          -> M.let_anno
        | Constraints _  -> M.constr
        | Args _         -> M.full_type_expr
        | Requires _     -> M.constr
        | Ensures _      -> M.constr
        | Inv_vars _     -> M.named_full_type_expr
        | Annot_args _   -> M.annot_args_anno
        | Tactics _      -> M.raw_text
        | Lemmas _       -> M.raw_text
        | Typedef _      -> M.named_full_type_expr
        | Size _         -> M.rocq_expr
        | Tagged_union _ -> M.rocq_expr
        | Union_tag _    -> M.union_tag_anno
        | Field _        -> M.full_type_expr
        | Global _       -> M.full_type_expr
        | Returns _      -> M.full_type_expr
        | Manual_proof _ -> M.manual_proof_anno
        | Annot _        -> M.raw_text
        | Unfold_order _ -> M.integer
        | Immovable _ | Asrt _ | Trust_me _ | Skip _ 
        | Block _ | Full_block _ | Inlined _ ->
          assert false (* cannot be invoked by `annot` *)
      end in 
      parser lexbuf.lex_curr_p  
    and supplier = 
      let lexer = begin 
        match decl with 
        | Lemmas _ | Typedef _ | Annot _ ->
          passthrough
        | _ ->
          lexer tokens buffer
      end in 
      I.lexer_lexbuf_to_supplier lexer lexbuf 
    and succeed () = ()
    and fail checkpoint = 
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
          Queue.push (Rc_pre_parser.ZERO_ARG_DECL decl) tokens;
          [] end
        | One a -> begin
          Queue.push (Rc_pre_parser.ONE_ARG_DECL decl) tokens;
          [a] end
        | Many aa -> begin
          Queue.push (Rc_pre_parser.MANY_ARG_DECL decl) tokens;
          aa end 
      in 
      let rec push_all = function 
      | (loc, s) :: nil  -> begin
        invoke_rc_pre_parser loc s decl tokens buffer;
        Queue.push Rc_pre_parser.ARG_END tokens end
      | (loc, s) :: rest -> begin
        invoke_rc_pre_parser loc s decl tokens buffer;
        Queue.push Rc_pre_parser.ARG_SEP tokens;
        push_all rest end
      | nil ->
        Queue.push Rc_pre_parser.ARG_END tokens
      in 
      push_all args;
      Lazy.from_fun compute_buffer
    end

}