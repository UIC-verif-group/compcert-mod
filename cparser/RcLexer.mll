{
open Lexing
open Rc_pre_parser
open Rc_pre_parser_aux

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

{

  let lexer : lexbuf -> Rc_pre_parser.token = 
    fun lexbuf -> assert false

  let invoke_pre_parser text lexer buffer = 
    let lexbuf = Lexing.from_string text in 
    lexbuf.lex_curr_p <- curr_p;
    let module I = Rc_pre_parser.MenhirInterpreter in 
    let checkpoint = Rc_pre_parser.Incremental.arg lexbuf.lex_curr_p 
    and supplier = I.lexer_lexbuf_to_supplier lexer lexbuf
    and succeed () = ()
    and fail checkpoint = 
      Diagnostics.fatal_error_raw "%s" (ErrorReports.report text !buffer checkpoint)
    in
    I.loop_handle succeed fail supplier checkpoint
  
  let compute_buffer tokens buffer transf = fun () ->
    let loop t = Buf_cons (t, Lazy.from_fun go) in
    loop (transf (Queue.pop tokens)) 

  let annot : RcAnno.decl -> RcAnno.arguments : buffer = 
    fun decl args -> begin 
      let tokens = Queue.create () in
      let buffer = ref ErrorReports.Zero in
      let compute_buffer = compute_buffer tokens buffer from_pre in
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
      | (loc, cs) :: nil  -> begin
        invoke_pre_parser loc cs lexer buffer;
        Queue.push Rc_pre_parser.ARG_END end
      | (loc, cs) :: rest -> begin
        invoke_pre_parser loc cs lexer buffer;
        Queue.push Rc_pre_parser.ARG_SEP;
        push_all rest end
      | nil               ->
        Queue.push Rc_pre_parser.ARG_END
      in 
      push_all args;
      Lazy.from_fun compute_buffer
    end

}