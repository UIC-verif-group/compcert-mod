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
                            "global"
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

rule rocq_term bk = parse
  | eof                    { begin try B.finalize bk with 
                             | Ill_bracketed es -> 
                               fatal_error lexbuf es end }
  | "!{" as s              { if B.in_anti bk then 
                               fatal_error lexbuf 
                                 "outermost antiquotation must be formed as \"!{ ... }\""
                             else begin try B.enter_anti bk with 
                               | Ill_bracketed es ->
                                 fatal_error lexbuf es end;
                             rocq_term bk lexbuf }
  | "{" as s               { try begin 
                               if B.in_anti bk 
                                 then B.enter_anti bk
                                 else B.enter_quot bk 
                             end with Ill_bracketed es ->
                               fatal_error lexbuf es;
                             B.add_string bk s;
                             rocq_term bk lexbuf } 
  | "}" as s               { try begin 
                               if B.in_anti bk 
                                 then begin
                                   if not B.outermost bk
                                     then B.add_string bk s;
                                   B.exit_anti bk;
                                   rocq_term bk lexbuf 
                                 end
                                 else begin 
                                   let final = B.outermost bk in 
                                   B.exit_quot bk;
                                   if final 
                                     then B.finalize bk 
                                     else begin
                                       B.add_string bk s;
                                       rocq_term bk lexbuf
                                     end
                                 end
                             end with Ill_bracketed es ->
                               fatal_error lexbuf es }
  | ""                     { if B.in_anti bk 
                               then rocq_term_anti bk lexbuf 
                               else rocq_term_quot bk lexbuf }

and rocq_term_quot bk = parse 
  | non_ascii as s         { B.add_string bk s; 
                             rocq_term_quot bk lexbuf }
  | [^ '!' '{' '}'] as c   { B.add_char bk c;
                             rocq_term_quot bk lexbuf }
  | ""                     { rocq_term bk lexbuf }

and rocq_term_anti bk = parse
  | non_ascii as s         { B.add_string bk s;
                             rocq_term_anti bk lexbuf }
  | [^ '!' '{' '}'] as c   { B.add_char bk c;
                             rocq_term_anti bk lexbuf }
  | ""                     { rocq_term bk lexbuf }

rule iris_term bk = parse 
  | eof                    { begin try B.finalize bk with 
                             | Ill_bracketed es -> 
                               fatal_error lexbuf es end }
  | "!{"                   { if B.in_anti bk then 
                               fatal_error lexbuf 
                                 "outermost antiquotation must be formed as \"!{ ... }\""
                             else begin try B.enter_anti bk with 
                               | Ill_bracketed es ->
                                 fatal_error lexbuf es end;
                             iris_term bk lexbuf }
  | "{" as s               { begin try B.enter_anti bk with 
                             | Ill_bracketed es ->
                               fatal_error lexbuf es end;
                             B.add_string bk s;
                             iris_term bk lexbuf } 
  | "}" as s               { try begin
                               if not B.outermost bk
                                 then B.add_string bk s;
                               B.exit_anti bk;
                               iris_term bk lexbuf 
                             end with Ill_bracketed es ->
                               fatal_error lexbuf es }
  | "[" as s               { begin try B.enter_quot bk with 
                             | Ill_bracketed es ->
                               fatal_error lexbuf es end;
                             B.add_string bk s;
                             iris_term bk lexbuf }
  | "]" as s               { try begin 
                               let final = B.outermost bk in 
                               B.exit_quot bk;
                               if final 
                                 then B.finalize bk 
                                 else begin 
                                   B.add_string bk s;
                                   iris_term bk lexbuf
                                 end

                             end with Ill_bracketed es ->
                               fatal_error lexbuf es }
  | ""                     { if B.in_anti bk 
                               then iris_term_anti bk lexbuf 
                               else iris_term_quot bk lexbuf }

and iris_term_quot bk = parse 
  | non_ascii as s         { B.add_string bk s; 
                             iris_term_quot bk lexbuf }
  | [^ '!' '{' '}' '[' ']'] as c   
                           { B.add_char bk c;
                             iris_term_quot bk lexbuf }
  | ""                     { iris_term bk lexbuf }

and iris_term_anti bk = parse
  | non_ascii as s         { B.add_string bk s;
                             iris_term_anti bk lexbuf }
  | [^ '!' '{' '}' '[' ']'] as c
                           { B.add_char bk c;
                             iris_term_anti bk lexbuf }
  | ""                     { iris_term bk lexbuf }

rule token = parse
  | whitespace_char +      { token lexbuf }
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
  | "{"                    { let bk = B.create () in 
                             let q = rocq_term bk lexbuf in 
                             Rc_pp.PRE_BRACKETED (q, loc_of_lb lexbuf) }
  | "["                    { let bk = B.create () in 
                             let q = iris_term bk lexbuf in
                             Rc_pp.PRE_BRACKETED (q, loc_of_lb lexbuf) }
  | udot as c              { Rc_pp.UCHAR (c, loc_of_lb lexbuf) }

{

  let lexer = 

  let invoke_rc_pre_parser loc text decl buffer = 
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
          lexer 
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