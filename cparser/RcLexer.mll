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

}

let udot = 
    ['\x00' - '\x7F'] 
  | ['\xC0' - '\xDF'] ['\x80' - '\xBF'] 
  | ['\xE0' - '\xEF'] ['\x80' - '\xBF'] ['\x80' - '\xBF'] 
  | ['\xF0' - '\xF7'] ['\x80' - '\xBF'] ['\x80' - '\xBF'] ['\x80' - '\xBF']



rule coq_term = parse 
  | "{"                    { Rc_pp.LBRACK }
  | "}"                    { Rc_pp.RBRACK }
  | "["                    { Rc_pp.LBRACE }
  | "]"                    { Rc_pp.RBRACE }
  | "!{"                   { Rc_pp.LBANGBRACK}
  | 


{

  let lexer : lexbuf -> Rc_pre_parser.token = 
    fun lexbuf -> assert false

  let invoke_rc_pre_parser loc text lexer buffer = 
    let lexbuf = Lexing.from_string text in 
    lexbuf.lex_curr_p <- 
      { lexbuf.lex_curr_p with 
        pos_fname = loc.filename
      ; pos_lnum = loc.lineno
      ; pos_cnum = loc.byteno };
    let module I = Rc_pre_parser.MenhirInterpreter in 
    let checkpoint = Rc_pre_parser.Incremental.arg lexbuf.lex_curr_p 
    and supplier = I.lexer_lexbuf_to_supplier lexer lexbuf
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
        invoke_rc_pre_parser loc s lexer buffer;
        Queue.push Rc_pre_parser.ARG_END end
      | (loc, s) :: rest -> begin
        invoke_rc_pre_parser loc s lexer buffer;
        Queue.push Rc_pre_parser.ARG_SEP;
        push_all rest end
      | nil               ->
        Queue.push Rc_pre_parser.ARG_END
      in 
      push_all args;
      Lazy.from_fun compute_buffer
    end

}