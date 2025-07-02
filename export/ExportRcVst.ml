
open Printf
open Commandline
open Clflags
open CommonOptions
open Driveraux
open Frontend
open Diagnostics

let tool_name = "RefinedVST Compatibility Layer"

(* Processing of a .c file *)

let process_c_file sourcename =
  ensure_inputfile_exists sourcename;
  let ofile = output_filename sourcename in
  let comment_annot = 
    let lines = RcExtra.read_lines sourcename in 
    RcCommentAnnot.parse_annots lines
  in
  let preproname = if !option_dprepro then
      Driveraux.output_filename sourcename ~suffix:".i"
    else
      Driveraux.tmp_file ".i" in
  preprocess sourcename preproname;
  let set_dest dst opt ext =
    dst := if !opt then Some (output_filename sourcename ~suffix:ext)
      else None in
  set_dest Cprint.destination option_dparse ".parsed.c";
  set_dest PrintCsyntax.destination option_dcmedium ".compcert.c";
  set_dest PrintClight.destination option_dclight ".light.c";
  let cs = parse_c_file sourcename preproname in
  let loc = file_loc sourcename in 
  let clight = 
    match SimpleExpr.transl_program csyntax with 
    | Errors.OK p ->
      begin match SimplLocals.transf_program p with 
      | Errors.OK p' ->
          if !option_normalize
          then Clightnorm.norm_program p'
          else p'
      | Errors.Error msg ->
        fatal_error loc "%a" print_error msg
      end
    | Errors.Error msg ->
      fatal_error loc "%a" print_error msg in 
  PrintClight.print_if_2 clight;
  let oc = open_out ofile in 
  ExportClight.print_program (Format.formatter_of_out_channel oc)
                             clight sourcename !option_normalize;
  close_out oc
  compile_c_file sourcename preproname ofile

let usage_string =
  version_string tool_name ^
{|Usage: rcvstgen [options] <source files>
Recognized source files:
  .c             C source file
  .i or .p       C source file that should not be preprocessed
Processing options:
  -normalize     Normalize the generated Clight code w.r.t. loads in expressions
  -canonical-idents  Use canonical numbers to represent identifiers  (default)
  -short-idents  Use small, non-canonical numbers to represent identifiers
  -o <dir>      Generate outputs in <dir>
|} ^
prepro_help ^
language_support_help ^
{|Tracing options:
  -dprepro       Save C file after preprocessing in <file>.i
  -dparse        Save C file after parsing and elaboration in <file>.parsed.c
  -dc            Save generated Compcert C in <file>.compcert.c
  -dclight       Save generated Clight in <file>.light.c
  -dall          Save all generated intermediate files in <file>.<ext>
|} ^
  general_help ^
  warning_help

let print_usage_and_exit () = 
  printf "%s" usage_string; exit 0

let set_all opts () = List.iter (fun r -> r := true) opts
let unset_all opts () = List.iter (fun r -> r := false) opts
  
let actions : ((string -> unit) * string) list ref = ref []

let push_action fn arg =
  actions := (fn, arg) :: !actions

let perform_actions () = 
  let rec perform = function
    | [] -> ()
    | (fn,arg) :: rem -> fn arg; perform rem
  in perform (List.rev !actions)

let num_input_files = ref 0

let cmdline_actions = 
  [
  (* Getting help *)
  Exact "-help", Unit print_usage_and_exit;
  Exact "--help", Unit print_usage_and_exit;]
 @ version_options tool_name @
(* Processing options *)
 [
  (* Exact "--root", String(fun s) *)
  Exact "-normalize", Set option_normalize;
  Exact "-canonical-idents", Set Camlcoq.use_canonical_atoms;
  Exact "-short-idents", Unset Camlcoq.use_canonical_atoms;
  Exact "-o", String(fun s -> option_o := Some s);
  Prefix "-o", Self (fun s -> let s = String.sub s 2 ((String.length s) - 2) in
                              option_o := Some s);]
(* Preprocessing options *)
  @ prepro_actions @
(* Tracing options *)
  [ Exact "-dprepro", Set option_dprepro;
   Exact "-dparse", Set option_dparse;
   Exact "-dc", Set option_dcmedium;
   Exact "-dclight", Set option_dclight;
   Exact "-dall", Self (fun _ ->
       option_dprepro := true;
       option_dparse := true;
       option_dcmedium := true;
       option_dclight := true;);
 ]
  @ general_options
(* Diagnostic options *)
  @ warning_options
  @ language_support_options @
(* Catch options that are not handled *)
  [Prefix "-", Self (fun s ->
     fatal_error no_loc "Unknown option `%s'" s);
(* File arguments *)
  Suffix ".c", Self (fun s ->
      incr num_input_files; push_action process_c_file s);
  ]

let _ = 
try
  Gc.set { (Gc.get()) with
              Gc.minor_heap_size = 524288; (* 512k *)
              Gc.major_heap_increment = 4194304 (* 4M *)
         };
  Printexc.record_backtrace true;
  Camlcoq.use_canonical_atoms := true;
  Frontend.init ();
  parse_cmdline cmdline_actions;
  if !option_o <> None && !num_input_files >= 2 then
    fatal_error no_loc "Ambiguous '-o' option (multiple source files)";
  if !num_input_files = 0 then
    fatal_error no_loc "no input file";
  perform_actions ()
with
  | Sys_error msg
  | CmdError msg -> error no_loc "%s" msg; exit 2
  | Abort -> exit 2
  | e -> crash e
