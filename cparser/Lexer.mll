(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Jacques-Henri Jourdan, INRIA Paris-Rocquencourt            *)
(*             Xavier Leroy, Collège de France and Inria               *)
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

{
open Lexing
open Pre_parser
open Pre_parser_aux

module Rc_pp_aux = Rc_pre_parser_aux

module SSet = Set.Make(String)

let lexicon : (string, Cabs.loc -> token) Hashtbl.t = Hashtbl.create 17
let ignored_keywords : SSet.t ref = ref SSet.empty

let reserved_keyword loc id =
  Diagnostics.fatal_error (loc.Cabs.filename, loc.Cabs.lineno)
    "illegal use of reserved keyword `%s'" id

let () =
  List.iter (fun (key, builder) -> Hashtbl.add lexicon key builder)
    [ 
      ("_Alignas", fun loc -> ALIGNAS loc);
      ("_Alignof", fun loc -> ALIGNOF loc);
      ("_Bool", fun loc -> UNDERSCORE_BOOL loc);
      ("_Generic", fun loc -> GENERIC loc);
      ("_Complex", fun loc -> reserved_keyword loc "_Complex");
      ("_Float16", fun loc -> FLOAT16 loc);
      ("_Imaginary", fun loc -> reserved_keyword loc "_Imaginary");
      ("_Static_assert", fun loc -> STATIC_ASSERT loc);
      ("__alignof", fun loc -> ALIGNOF loc);
      ("__alignof__", fun loc -> ALIGNOF loc);
      ("__asm", fun loc -> ASM loc);
      ("__asm__", fun loc -> ASM loc);
      ("__attribute", fun loc -> ATTRIBUTE loc);
      ("__attribute__", fun loc -> ATTRIBUTE loc);
      ("__builtin_va_arg", fun loc -> BUILTIN_VA_ARG loc);
      ("__builtin_offsetof", fun loc -> BUILTIN_OFFSETOF loc);
      ("__const", fun loc -> CONST loc);
      ("__const__", fun loc -> CONST loc);
      ("__inline", fun loc -> INLINE loc);
      ("__inline__", fun loc -> INLINE loc);
      ("__packed__", fun loc -> PACKED loc);
      ("__restrict", fun loc -> RESTRICT loc);
      ("__restrict__", fun loc -> RESTRICT loc);
      ("__signed", fun loc -> SIGNED loc);
      ("__signed__", fun loc -> SIGNED loc);
      ("__volatile", fun loc -> VOLATILE loc);
      ("__volatile__", fun loc -> VOLATILE loc);
      ("asm", fun loc -> ASM loc);
      ("auto", fun loc -> AUTO loc);
      ("break", fun loc -> BREAK loc);
      ("case", fun loc -> CASE loc);
      ("char", fun loc -> CHAR loc);
      ("const", fun loc -> CONST loc);
      ("continue", fun loc -> CONTINUE loc);
      ("default", fun loc -> DEFAULT loc);
      ("do", fun loc -> DO loc);
      ("double", fun loc -> DOUBLE loc);
      ("else", fun loc -> ELSE loc);
      ("enum", fun loc -> ENUM loc);
      ("extern", fun loc -> EXTERN loc);
      ("float", fun loc -> FLOAT loc);
      ("for", fun loc -> FOR loc);
      ("goto", fun loc -> GOTO loc);
      ("if", fun loc -> IF loc);
      ("inline", fun loc -> INLINE loc);
      ("_Noreturn", fun loc -> NORETURN loc);
      ("int", fun loc -> INT loc);
      ("long", fun loc -> LONG loc);
      ("register", fun loc -> REGISTER loc);
      ("restrict", fun loc -> RESTRICT loc);
      ("return", fun loc -> RETURN loc);
      ("short", fun loc -> SHORT loc);
      ("signed", fun loc -> SIGNED loc);
      ("sizeof", fun loc -> SIZEOF loc);
      ("static", fun loc -> STATIC loc);
      ("struct", fun loc -> STRUCT loc);
      ("switch", fun loc -> SWITCH loc);
      ("typedef", fun loc -> TYPEDEF loc);
      ("union", fun loc -> UNION loc);
      ("unsigned", fun loc -> UNSIGNED loc);
      ("void", fun loc -> VOID loc);
      ("volatile", fun loc -> VOLATILE loc);
      ("while", fun loc -> WHILE loc)];
  if Configuration.system <> "diab" then
    (* We can ignore the __extension__ GCC keyword. *)
    ignored_keywords := SSet.add "__extension__" !ignored_keywords

let init_ctx = SSet.of_list (List.map fst CBuiltins.builtins.C.builtin_typedefs)

let types_context : SSet.t ref = ref init_ctx

let _ =
  (* See comments in pre_parser_aux.ml *)
  save_context := begin fun () ->
    let save = !types_context in
    fun () -> types_context := save
  end;

  declare_varname := begin fun id ->
    types_context := SSet.remove id !types_context
  end;

  declare_typename := begin fun id ->
    types_context := SSet.add id !types_context
  end

let init filename channel : Lexing.lexbuf =
  let lb = Lexing.from_channel channel in
  lb.lex_curr_p <- {lb.lex_curr_p with pos_fname = filename; pos_lnum = 1};
  lb

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

(* Error reporting *)

let fatal_error lb fmt =
  Diagnostics.fatal_error
    (lb.lex_curr_p.pos_fname,lb.lex_curr_p.pos_lnum) fmt

let error lb fmt =
  Diagnostics.error
    (lb.lex_curr_p.pos_fname,lb.lex_curr_p.pos_lnum) fmt

let warning lb kind fmt =
  Diagnostics.warning
      (lb.lex_curr_p.pos_fname,lb.lex_curr_p.pos_lnum) kind fmt

(* Simple character escapes *)

let convert_escape = function
  | 'a' -> 7L  (* bell *)
  | 'b' -> 8L  (* backspace  *)
  | 'e' -> 27L (* escape (GCC extension) *)
  | 'f' -> 12L (* form feed *)
  | 'n' -> 10L (* new line *)
  | 'r' -> 13L (* carriage return *)
  | 't' -> 9L  (* horizontal tab *)
  | 'v' -> 11L (* vertical tab *)
  | c   -> Int64.of_int (Char.code c)

(* Encodings for character and string literals *)

let encoding_of = function
  | "" -> Cabs.EncNone
  | "L" -> Cabs.EncWide
  | "u" -> Cabs.EncU16
  | "U" -> Cabs.EncU32
  | "u8" -> Cabs.EncUTF8
  | _ -> assert false

let combine_encodings loc e1 e2 =
  if e1 = Cabs.EncNone then e2
  else if e2 = Cabs.EncNone then e1
  else if e1 = e2 then e1
  else Diagnostics.fatal_error
           Cabs.(loc.filename, loc.lineno)
           "unsupported non-standard concatenation of string literals"

(* Handling of characters and escapes in string and char constants *)

type chr = Chr of int | Esc of int64

let utf8_check lexbuf min x = 
  if x > 0x10FFFF || (x >= 0xD800 && x <= 0xDFFF) then 
    warning lexbuf Diagnostics.Invalid_UTF8 "Wrong Unicode value U+%X" x;
  if x < min then 
    warning lexbuf Diagnostics.Invalid_UTF8
            "Overlong UTF-8 encoding for Unicode value U+%X" x

let check_utf8 lexbuf min x = begin 
  utf8_check lexbuf min x; Chr x end

let check_universal_character lexbuf x =
  if x > 0x10FFFF
  || x >= 0xD800 && x <= 0xDFFF
  || x < 0xA0 && x <> 0x24 && x <> 0x40 && x <> 0x60
  then begin
    error lexbuf "Wrong universal character name U+%X" x; Chr 0
  end else
    Chr x

let add_char_utf8 x accu =
  if x <= 0x007F then
    Int64.of_int x :: accu
  else if x <= 0x07FF then
    Int64.of_int (0x80 lor (x land 0x3F)) ::
    Int64.of_int (0xC0 lor (x lsr 6)) ::
    accu
  else if x <= 0xFFFF then
    Int64.of_int (0x80 lor (x land 0x3F)) ::
    Int64.of_int (0x80 lor ((x lsr 6) land 0x3F)) ::
    Int64.of_int (0xE0 lor (x lsr 12)) ::
    accu
  else
    Int64.of_int (0x80 lor (x land 0x3F)) ::
    Int64.of_int (0x80 lor ((x lsr 6) land 0x3F)) ::
    Int64.of_int (0x80 lor ((x lsr 12) land 0x3F)) ::
    Int64.of_int (0xF0 lor (x lsr 18)) ::
    accu

let add_char_utf16 x accu =
  if x <= 0xFFFF then
    Int64.of_int x :: accu
  else begin
    let x = x - 0x10000 in
    Int64.of_int (0xDC00 lor (x land 0x3FF)) ::
    Int64.of_int (0xD800 lor (x lsr 10)) ::
    accu
  end

let add_char enc c accu =
  match c, enc with
  | Esc x, _ -> (* Escapes are never encoded *)
      x :: accu
  | Chr x, (Cabs.EncNone | Cabs.EncUTF8) -> (* Characters are encoded in UTF8 *)
      add_char_utf8 x accu
  | Chr x, Cabs.EncU16 -> (* Characters are encoded in UTF16 *)
      add_char_utf16 x accu
  | Chr x, Cabs.EncU32 -> (* Characters are not encoded *)
      Int64.of_int x :: accu
  | Chr x, Cabs.EncWide -> (* Depends on size of wchar_t *)
      if Cutil.sizeof_ikind (Cutil.wchar_ikind ()) = 2
      then add_char_utf16 x accu
      else Int64.of_int x :: accu

module Buffer = struct 
  include Buffer 

  let add_chars cs buf = 
    let rec go = begin function 
    | c :: tl ->
      add_char c buf
    | nil     ->
      ()
    end

  let default_sized () = 
    Buffer.create 100
end

}

(* Identifiers *)
let digit = ['0'-'9']
let hexadecimal_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let nondigit = ['_' 'a'-'z' 'A'-'Z']

let hex_quad = hexadecimal_digit hexadecimal_digit
                 hexadecimal_digit hexadecimal_digit
let universal_character_name =
    "\\u" (hex_quad as n)
  | "\\U" (hex_quad hex_quad as n)

let identifier_nondigit =
    nondigit
(*| universal_character_name*)
  | '$'

let identifier = identifier_nondigit (identifier_nondigit|digit)*

let pat_rc_decl_many_arg =
  ( "parameters"
  | "refined_by"
  | "exists"
  | "let"
  | "constraints"
  | "args"
  | "requires"
  | "ensures"
  | "inv_vars"
  | "annot_args"
  | "tactics"
  | "lemmas" ) as decl

let pat_rc_decl_one_arg =
  ( "typedef"
  | "size"
  | "tagged_union"
  | "union_tag"
  | "field"
  | "global"
  | "returns"
  | "manual_proof"
  | "annot"
  | "unfold_order" ) as decl

let pat_rc_decl_zero_arg =
  ( "immovable"
  | "asrt"
  | "trust_me"
  | "skip"
  | "block"
  | "full_block"
  | "inlined" ) as decl

(* Whitespaces *)
let whitespace_char_no_newline = [' ' '\t'  '\011' '\012' '\r']

(* Integer constants *)
let nonzero_digit = ['1'-'9']
let decimal_constant = nonzero_digit digit*

let octal_digit = ['0'-'7']
let octal_constant = '0' octal_digit*

let hexadecimal_prefix = "0x" | "0X"
let hexadecimal_constant =
  hexadecimal_prefix hexadecimal_digit+

let unsigned_suffix = ['u' 'U']
let long_suffix = ['l' 'L']
let long_long_suffix = "ll" | "LL"
let integer_suffix =
    unsigned_suffix long_suffix?
  | unsigned_suffix long_long_suffix
  | long_suffix unsigned_suffix?
  | long_long_suffix unsigned_suffix?

let integer_constant =
    decimal_constant integer_suffix?
  | octal_constant integer_suffix?
  | hexadecimal_constant integer_suffix?

(* Floating constants *)
let sign = ['-' '+']
let digit_sequence = digit+
let floating_suffix = ['f' 'l' 'F' 'L'] as suffix

let fractional_constant =
    (digit_sequence as intpart)? '.' (digit_sequence as frac)
  | (digit_sequence as intpart) '.'
let exponent_part =
    'e' ((sign? digit_sequence) as expo)
  | 'E' ((sign? digit_sequence) as expo)
let decimal_floating_constant =
    fractional_constant exponent_part? floating_suffix?
  | (digit_sequence as intpart) exponent_part floating_suffix?

let hexadecimal_digit_sequence = hexadecimal_digit+
let hexadecimal_fractional_constant =
    (hexadecimal_digit_sequence as intpart)? '.' (hexadecimal_digit_sequence as frac)
  | (hexadecimal_digit_sequence as intpart) '.'
let binary_exponent_part =
    'p' ((sign? digit_sequence) as expo)
  | 'P' ((sign? digit_sequence) as expo)
let hexadecimal_floating_constant =
    hexadecimal_prefix hexadecimal_fractional_constant
        binary_exponent_part floating_suffix?
  | hexadecimal_prefix (hexadecimal_digit_sequence as intpart)
        binary_exponent_part floating_suffix?

(* Preprocessing numbers *)
let preprocessing_number =
  '.'? ['0'-'9']
  (['0'-'9' 'A'-'Z' 'a'-'z' '_' '.'] | ['e' 'E' 'p' 'P']['+' '-'])*

(* Character and string constants *)
let simple_escape_sequence =
  '\\' ( ['\''  '\"'  '?'  '\\'  'a'  'b'  'e'  'f'  'n'  'r'  't'  'v'] as c)
let octal_escape_sequence =
  '\\' ((octal_digit
         | octal_digit octal_digit
         | octal_digit octal_digit octal_digit) as n)
let hexadecimal_escape_sequence = "\\x" (hexadecimal_digit+ as n)

rule initial = parse
  | "[[rc::"                      { let loc = loc_of_lb lexbuf in 
                                    RCATTR (rc_decl lexbuf, loc) }
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline +  { initial lexbuf }
  | "/*"                          { multiline_comment lexbuf; initial lexbuf }
  | "//"                          { singleline_comment lexbuf; initial lexbuf }
  | integer_constant as s         { CONSTANT (Cabs.CONST_INT s, loc_of_lb lexbuf) }
  | decimal_floating_constant     { CONSTANT (Cabs.CONST_FLOAT
                                      {Cabs.isHex_FI = false;
                                       Cabs.integer_FI = intpart;
                                       Cabs.fraction_FI = frac;
                                       Cabs.exponent_FI = expo;
                                       Cabs.suffix_FI =
                                         match suffix with
                                         | None -> None
                                         | Some c -> Some (String.make 1 c) },
                                      loc_of_lb lexbuf) }
  | hexadecimal_floating_constant { CONSTANT (Cabs.CONST_FLOAT
                                      {Cabs.isHex_FI = true;
                                       Cabs.integer_FI = intpart;
                                       Cabs.fraction_FI = frac;
                                       Cabs.exponent_FI = Some expo;
                                       Cabs.suffix_FI =
                                         match suffix with
                                           | None -> None
                                           | Some c -> Some (String.make 1 c) },
                                      loc_of_lb lexbuf)}
  | preprocessing_number as s     { error lexbuf "invalid numerical constant '%s'@ These characters form a preprocessor number, but not a constant" s;
                                    CONSTANT (Cabs.CONST_INT "0", loc_of_lb lexbuf) }
  | (""|"L"|"u"|"U") as e "'"     { let enc = encoding_of e in
                                    let start_p = lexbuf.lex_start_p in 
                                    char_literal start_p [] lexbuf }
  | (""|"L"|"u"|"U"|"u8") as e "\""
                                  { let enc = encoding_of e in
                                    let start_p = lexbuf.lex_start_p in  
                                    string_literal start_p enc [] lexbuf }
  | "..."                         { ELLIPSIS(loc_of_lb lexbuf) }
  | "+="                          { ADD_ASSIGN(loc_of_lb lexbuf) }
  | "-="                          { SUB_ASSIGN(loc_of_lb lexbuf) }
  | "*="                          { MUL_ASSIGN(loc_of_lb lexbuf) }
  | "/="                          { DIV_ASSIGN(loc_of_lb lexbuf) }
  | "%="                          { MOD_ASSIGN(loc_of_lb lexbuf) }
  | "|="                          { OR_ASSIGN(loc_of_lb lexbuf) }
  | "&="                          { AND_ASSIGN(loc_of_lb lexbuf) }
  | "^="                          { XOR_ASSIGN(loc_of_lb lexbuf) }
  | "<<="                         { LEFT_ASSIGN(loc_of_lb lexbuf) }
  | ">>="                         { RIGHT_ASSIGN(loc_of_lb lexbuf) }
  | "<<"                          { LEFT(loc_of_lb lexbuf) }
  | ">>"                          { RIGHT(loc_of_lb lexbuf) }
  | "=="                          { EQEQ(loc_of_lb lexbuf) }
  | "!="                          { NEQ(loc_of_lb lexbuf) }
  | "<="                          { LEQ(loc_of_lb lexbuf) }
  | ">="                          { GEQ(loc_of_lb lexbuf) }
  | "="                           { EQ(loc_of_lb lexbuf) }
  | "<"                           { LT(loc_of_lb lexbuf) }
  | ">"                           { GT(loc_of_lb lexbuf) }
  | "++"                          { INC(loc_of_lb lexbuf) }
  | "--"                          { DEC(loc_of_lb lexbuf) }
  | "->"                          { PTR(loc_of_lb lexbuf) }
  | "+"                           { PLUS(loc_of_lb lexbuf) }
  | "-"                           { MINUS(loc_of_lb lexbuf) }
  | "*"                           { STAR(loc_of_lb lexbuf) }
  | "/"                           { SLASH(loc_of_lb lexbuf) }
  | "%"                           { PERCENT(loc_of_lb lexbuf) }
  | "!"                           { BANG(loc_of_lb lexbuf) }
  | "&&"                          { ANDAND(loc_of_lb lexbuf) }
  | "||"                          { BARBAR(loc_of_lb lexbuf) }
  | "&"                           { AND(loc_of_lb lexbuf) }
  | "|"                           { BAR(loc_of_lb lexbuf) }
  | "^"                           { HAT(loc_of_lb lexbuf) }
  | "?"                           { QUESTION(loc_of_lb lexbuf) }
  | ":"                           { COLON(loc_of_lb lexbuf) }
  | "~"                           { TILDE(loc_of_lb lexbuf) }
  | "{"|"<%"                      { LBRACE(loc_of_lb lexbuf) }
  | "}"|"%>"                      { RBRACE(loc_of_lb lexbuf) }
  | "["|"<:"                      { LBRACK(loc_of_lb lexbuf) }
  | "]"|":>"                      { RBRACK(loc_of_lb lexbuf) }
  | "("                           { LPAREN(loc_of_lb lexbuf) }
  | ")"                           { RPAREN(loc_of_lb lexbuf) }
  | ";"                           { SEMICOLON(loc_of_lb lexbuf) }
  | ","                           { COMMA(loc_of_lb lexbuf) }
  | "."                           { DOT(loc_of_lb lexbuf) }
  | identifier as id              {
    if SSet.mem id !ignored_keywords then
      initial lexbuf
    else
      try Hashtbl.find lexicon id (loc_of_lb lexbuf)
      with Not_found -> PRE_NAME id }
  | eof                           { EOF }
  | _ as c                        { fatal_error lexbuf "invalid symbol %C" c }

and initial_linebegin = parse
  | '\n'                          { new_line lexbuf; initial_linebegin lexbuf }
  | whitespace_char_no_newline    { initial_linebegin lexbuf }
  | '#'                           { hash lexbuf }
  | ""                            { initial lexbuf }

and rc_decl = parse 
  | pat_rc_decl_zero_arg          { let decl = 
                                      let to_decl = Rc_pp_aux.decl_of_string decl in
                                      to_decl (loc_of_lb lexbuf) 
                                    in
                                    let args = Rc_pp_aux.Zero in 
                                    rc_clos_end decl args lexbuf }
  | pat_rc_decl_one_arg "(\""
                                  { let decl = 
                                      let to_decl = Rc_pp_aux.decl_of_string decl in
                                      to_decl (loc_of_lb lexbuf) 
                                    in
                                    let args = 
                                      let buf = Buffer.default_sized () in
                                      let start_p = lexbuf.lex_curr_p in 
                                      Rc_pp_aux.One (rc_literal start_p buf lexbuf)
                                    in 
                                    rc_open_end decl args lexbuf }
  | pat_rc_decl_many_arg "(\""
                                  { let decl = 
                                      let to_decl = Rc_pp_aux.decl_of_string decl in 
                                      to_decl (loc_of_lb lexbuf) 
                                    in
                                    let args =
                                      let buf = Buffer.default_sized () in 
                                      let start_p = lexbuf.lex_curr_p in 
                                      rc_rest [rc_literal start_p buf lexbuf] lexbuf 
                                    in 
                                    rc_open_end decl args lexbuf }
and rc_rest args = parse
  | "," whitespace_char_no_newline * "\""  
                                  { let buf = Bytes.default_sized () in
                                    let start_p = lexbuf.lex_curr_p in  
                                    rc_rest ((rc_literal start_p buf lexbuf) :: args) lexbuf }
  | ""                            { Rc_pp_aux.Many (List.rev args) }

and rc_open_end decl args = parse 
  | ")"                           { rc_clos_end decl args lexbuf }
and rc_clos_end decl args = parse
  | "]]"                          { RcLexer.annot decl args }

and char = parse
  | universal_character_name
      { try
          check_universal_character lexbuf (int_of_string ("0x" ^ n))
        with Failure _ ->
          error lexbuf "overflow in universal character name";
          Chr 0
      }
  | hexadecimal_escape_sequence
      { try
          Esc (Int64.of_string ("0x" ^ n))
        with Failure _ ->
          error lexbuf "overflow in hexadecimal escape sequence";
          Esc 0L
      }
  | octal_escape_sequence
      { Esc (Int64.of_string  ("0o" ^ n)) }
  | simple_escape_sequence
      { Esc (convert_escape c) }
  | "\\u" | "\\U"
      { error lexbuf "incomplete universal character name";
        Chr 0 }
  | '\\' (_ as c)
      { error lexbuf "incorrect escape sequence '\\%c'" c;
        Esc (Int64.of_int (Char.code c)) }
  | ['\x00'-'\x7F'] as c1
      { Chr (Char.code c1) }
  | (['\xC0'-'\xDF'] as c1) (['\x80'-'\xBF'] as c2)
      { check_utf8 lexbuf 0x80
          ( (Char.code c1 land 0b00011111) lsl 6
          + (Char.code c2 land 0b00111111)) }
  | (['\xE0'-'\xEF'] as c1) (['\x80'-'\xBF'] as c2) (['\x80'-'\xBF'] as c3)
      { check_utf8 lexbuf 0x800
          ( (Char.code c1 land 0b00001111) lsl 12
          + (Char.code c2 land 0b00111111) lsl 6
          + (Char.code c3 land 0b00111111) ) }
  | (['\xF0'-'\xF7'] as c1) (['\x80'-'\xBF'] as c2) (['\x80'-'\xBF'] as c3) (['\x80'-'\xBF'] as c4)
     { check_utf8 lexbuf 0x10000
          ( (Char.code c1 land 0b00000111) lsl 18
          + (Char.code c2 land 0b00111111) lsl 12
          + (Char.code c3 land 0b00111111) lsl 6
          + (Char.code c4 land 0b00111111) ) }
  | _ as c
     { warning lexbuf Diagnostics.Invalid_UTF8
               "Invalid UTF8 encoding: byte 0x%02x" (Char.code c);
       Esc (Int64.of_int (Char.code c)) (* re-encode as-is *)
     }

and rc_char buf = parse
  | ['\x00'-'\x7F'] as c1
      { Buffer.add_chars [c1] buf }
  | (['\xC0'-'\xDF'] as c1) (['\x80'-'\xBF'] as c2)
      { let i = (Char.code c1 land 0b00011111) lsl 6 + 
                (Char.code c2 land 0b00111111) in 
        utf8_check lexbuf 0x80 i;
        Buffer.add_chars [c1; c2] buf }
  | (['\xE0'-'\xEF'] as c1) (['\x80'-'\xBF'] as c2) (['\x80'-'\xBF'] as c3)
      { let i = (Char.code c1 land 0b00001111) lsl 12 +
                (Char.code c2 land 0b00111111) lsl 6 + 
                (Char.code c3 land 0b00111111) in 
        utf8_check lexbuf 0x800 i;
        Buffer.add_chars [c1; c2; c3] buf }
  | (['\xF0'-'\xF7'] as c1) (['\x80'-'\xBF'] as c2) (['\x80'-'\xBF'] as c3) (['\x80'-'\xBF'] as c4)
      { let i = (Char.code c1 land 0b00000111) lsl 18 + 
                (Char.code c2 land 0b00111111) lsl 12 + 
                (Char.code c3 land 0b00111111) lsl 6 + 
                (Char.code c4 land 0b00111111) in
        utf8_check lexbuf 0x800 i;
        Buffer.add_chars [c1; c2; c3; c4] buf }
  | _ as c
     { fatal_error lexbuf Diagnostics.Invalid_UTF8
          "Invalid UTF8 encoding: byte 0x%02x" (Char.code c) }

and char_literal start_p accu = parse
  | '\''       { let chars = List.rev accu in 
                 CONSTANT (Cabs.CONST_CHAR(enc, chars), loc_of_start_p start_p) }
  | '\n' | eof { fatal_error lexbuf "missing terminating \"'\" character" }
  | ""         { let c = char lexbuf in char_literal start_p (add_char Cabs.EncU32 c accu) lexbuf }

and string_literal start_p enc accu = parse
  | '\"'       { let chars = List.rev accu in
                 STRING_LITERAL(enc, chars, loc_of_start_p start_p) }
  | '\n' | eof { fatal_error lexbuf "missing terminating '\"' character" }
  | ""         { let c = char lexbuf in string_literal start_p enc (add_char enc c accu) lexbuf }

and rc_literal start_p buf = parse
  | '\"'       { (loc_of_start_p start_p, Buffer.contents buf) }
  | eof { fatal_error lexbuf "missing terminating '\"' character" }
  | ""         { rc_char buf; rc_literal start_p buf lexbuf }

(* We assume gcc -E syntax but try to tolerate variations. *)
and hash = parse
  | whitespace_char_no_newline +
    (digit + as n)
    whitespace_char_no_newline *
    "\"" ([^ '\n' '\"']* as file) "\""
    [^ '\n']* '\n'
      { let n =
          try
            int_of_string n
          with Failure _ ->
            warning lexbuf Diagnostics.Unnamed "invalid line number";
            lexbuf.lex_curr_p.pos_lnum
        in
        lexbuf.lex_curr_p <- {
          lexbuf.lex_curr_p with
            pos_fname = file;
            pos_lnum = n;
            pos_bol = lexbuf.lex_curr_p.pos_cnum
        };
        initial_linebegin lexbuf }
  | whitespace_char_no_newline *
    "pragma"
    whitespace_char_no_newline +
    ([^ '\n']* as s) '\n'
      { new_line lexbuf; PRAGMA (s, loc_of_lb lexbuf) }
  | [^ '\n']* '\n'
      { warning lexbuf Diagnostics.Unnamed "unrecognized '#' line";
        new_line lexbuf; initial_linebegin lexbuf }
  | [^ '\n']* eof
      { fatal_error lexbuf "unexpected end of file" }
  | _ as c
      { fatal_error lexbuf "invalid symbol %C" c }

(* Multi-line comment terminated by "*/" *)
and multiline_comment = parse
  | "*/"   { () }
  | eof    { error lexbuf "unterminated comment" }
  | '\n'   { new_line lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { new_line lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }

{
  open Parser.MenhirLibParser.Inter

  (* This is the main entry point to the lexer. *)

  let lexer : lexbuf -> Pre_parser.token =
    fun lexbuf ->
      if lexbuf.lex_curr_p.pos_cnum = lexbuf.lex_curr_p.pos_bol then
        initial_linebegin lexbuf
      else
        initial lexbuf

  (* [lexer tokens buffer] is a new lexer, which wraps [lexer], and also: 1-
     records the token stream into the FIFO queue [tokens] and 2- records the
     start and end positions of the last two tokens in the two-place buffer
     [buffer] and 3- duplicates identifier tokens into PRE_NAME and
     VAR/TYPE_NAME. *)

  let lexer tokens buffer : lexbuf -> Pre_parser.token =
    let curr_id = ref None in
    types_context := init_ctx;
    fun lexbuf ->
      match !curr_id with
      | Some id ->
        curr_id := None;
        let loc = loc_of_lb lexbuf in
        let token =
          if SSet.mem id !types_context then Pre_parser.TYPEDEF_NAME (id, ref TypedefId, loc)
          else Pre_parser.VAR_NAME (id, ref VarId, loc)
        in
        Queue.push token tokens;
        token
      | None ->
        let token = lexer lexbuf in
        begin match token with
        | PRE_NAME id -> curr_id := Some id
        | _ -> Queue.push token tokens
        end;
        let startp = lexbuf.lex_start_p
        and endp = lexbuf.lex_curr_p in
        buffer := ErrorReports.update !buffer (startp, endp);
        token

  (* [invoke_pre_parser] is in charge of calling the pre_parser. It uses
     the incremental API, which allows us to do our own error handling. *)

  let invoke_pre_parser filename text lexer buffer =
    let lexbuf = Lexing.from_string text in
    lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename; pos_lnum = 1};
    let module I = Pre_parser.MenhirInterpreter in
    let checkpoint = Pre_parser.Incremental.translation_unit_file lexbuf.lex_curr_p
    and supplier = I.lexer_lexbuf_to_supplier lexer lexbuf
    and succeed () = ()
    and fail checkpoint =
      Diagnostics.fatal_error_raw "%s" (ErrorReports.report text !buffer checkpoint)
    in
    I.loop_handle succeed fail supplier checkpoint

  (* [tokens_stream filename text] runs the pre_parser and produces a stream
     of (appropriately classified) tokens. *)

  let tokens_stream filename text : buffer =
    let tokens = Queue.create () in
    let buffer = ref ErrorReports.Zero in
    invoke_pre_parser filename text (lexer tokens buffer) buffer;
    let rec compute_buffer () =
      let loop t = Buf_cons (t, Lazy.from_fun compute_buffer) in
      match Queue.pop tokens with
      | Pre_parser.ADD_ASSIGN loc -> loop (Parser.ADD_ASSIGN loc)
      | Pre_parser.AND loc -> loop (Parser.AND loc)
      | Pre_parser.ANDAND loc -> loop (Parser.ANDAND loc)
      | Pre_parser.AND_ASSIGN loc -> loop (Parser.AND_ASSIGN loc)
      | Pre_parser.AUTO loc -> loop (Parser.AUTO loc)
      | Pre_parser.BANG loc -> loop (Parser.BANG loc)
      | Pre_parser.BAR loc -> loop (Parser.BAR loc)
      | Pre_parser.BARBAR loc -> loop (Parser.BARBAR loc)
      | Pre_parser.UNDERSCORE_BOOL loc -> loop (Parser.UNDERSCORE_BOOL loc)
      | Pre_parser.BREAK loc -> loop (Parser.BREAK loc)
      | Pre_parser.BUILTIN_VA_ARG loc -> loop (Parser.BUILTIN_VA_ARG loc)
      | Pre_parser.BUILTIN_OFFSETOF loc -> loop (Parser.BUILTIN_OFFSETOF loc)
      | Pre_parser.CASE loc -> loop (Parser.CASE loc)
      | Pre_parser.CHAR loc -> loop (Parser.CHAR loc)
      | Pre_parser.COLON loc -> loop (Parser.COLON loc)
      | Pre_parser.COMMA loc -> loop (Parser.COMMA loc)
      | Pre_parser.CONST loc -> loop (Parser.CONST loc)
      | Pre_parser.CONSTANT (cst, loc) -> loop (Parser.CONSTANT (cst, loc))
      | Pre_parser.CONTINUE loc -> loop (Parser.CONTINUE loc)
      | Pre_parser.DEC loc -> loop (Parser.DEC loc)
      | Pre_parser.DEFAULT loc -> loop (Parser.DEFAULT loc)
      | Pre_parser.DIV_ASSIGN loc -> loop (Parser.DIV_ASSIGN loc)
      | Pre_parser.DO loc -> loop (Parser.DO loc)
      | Pre_parser.DOT loc -> loop (Parser.DOT loc)
      | Pre_parser.DOUBLE loc -> loop (Parser.DOUBLE loc)
      | Pre_parser.ELLIPSIS loc -> loop (Parser.ELLIPSIS loc)
      | Pre_parser.ELSE loc -> loop (Parser.ELSE loc)
      | Pre_parser.ENUM loc -> loop (Parser.ENUM loc)
      | Pre_parser.EOF -> loop (Parser.EOF ())
      | Pre_parser.EQ loc -> loop (Parser.EQ loc)
      | Pre_parser.EQEQ loc -> loop (Parser.EQEQ loc)
      | Pre_parser.EXTERN loc -> loop (Parser.EXTERN loc)
      | Pre_parser.FLOAT loc -> loop (Parser.FLOAT loc)
      | Pre_parser.FLOAT16 loc -> loop (Parser.FLOAT16 loc)
      | Pre_parser.FOR loc -> loop (Parser.FOR loc)
      | Pre_parser.GENERIC loc -> loop (Parser.GENERIC loc)
      | Pre_parser.GEQ loc -> loop (Parser.GEQ loc)
      | Pre_parser.GOTO loc -> loop (Parser.GOTO loc)
      | Pre_parser.GT loc -> loop (Parser.GT loc)
      | Pre_parser.HAT loc -> loop (Parser.HAT loc)
      | Pre_parser.IF loc -> loop (Parser.IF_ loc)
      | Pre_parser.INC loc -> loop (Parser.INC loc)
      | Pre_parser.INLINE loc -> loop (Parser.INLINE loc)
      | Pre_parser.INT loc -> loop (Parser.INT loc)
      | Pre_parser.LBRACE loc -> loop (Parser.LBRACE loc)
      | Pre_parser.LBRACK loc -> loop (Parser.LBRACK loc)
      | Pre_parser.LEFT loc -> loop (Parser.LEFT loc)
      | Pre_parser.LEFT_ASSIGN loc -> loop (Parser.LEFT_ASSIGN loc)
      | Pre_parser.LEQ loc -> loop (Parser.LEQ loc)
      | Pre_parser.LONG loc -> loop (Parser.LONG loc)
      | Pre_parser.LPAREN loc -> loop (Parser.LPAREN loc)
      | Pre_parser.LT loc -> loop (Parser.LT loc)
      | Pre_parser.MINUS loc -> loop (Parser.MINUS loc)
      | Pre_parser.MOD_ASSIGN loc -> loop (Parser.MOD_ASSIGN loc)
      | Pre_parser.MUL_ASSIGN loc -> loop (Parser.MUL_ASSIGN loc)
      | Pre_parser.NEQ loc -> loop (Parser.NEQ loc)
      | Pre_parser.NORETURN loc -> loop (Parser.NORETURN loc)
      | Pre_parser.OR_ASSIGN loc -> loop (Parser.OR_ASSIGN loc)
      | Pre_parser.PACKED loc -> loop (Parser.PACKED loc)
      | Pre_parser.PERCENT loc -> loop (Parser.PERCENT loc)
      | Pre_parser.PLUS loc -> loop (Parser.PLUS loc)
      | Pre_parser.PTR loc -> loop (Parser.PTR loc)
      | Pre_parser.QUESTION loc -> loop (Parser.QUESTION loc)
      | Pre_parser.RBRACE loc -> loop (Parser.RBRACE loc)
      | Pre_parser.RBRACK loc -> loop (Parser.RBRACK loc)
      | Pre_parser.REGISTER loc -> loop (Parser.REGISTER loc)
      | Pre_parser.RESTRICT loc -> loop (Parser.RESTRICT loc)
      | Pre_parser.RETURN loc -> loop (Parser.RETURN loc)
      | Pre_parser.RIGHT loc -> loop (Parser.RIGHT loc)
      | Pre_parser.RIGHT_ASSIGN loc -> loop (Parser.RIGHT_ASSIGN loc)
      | Pre_parser.RPAREN loc -> loop (Parser.RPAREN loc)
      | Pre_parser.SEMICOLON loc -> loop (Parser.SEMICOLON loc)
      | Pre_parser.SHORT loc -> loop (Parser.SHORT loc)
      | Pre_parser.SIGNED loc -> loop (Parser.SIGNED loc)
      | Pre_parser.SIZEOF loc -> loop (Parser.SIZEOF loc)
      | Pre_parser.SLASH loc -> loop (Parser.SLASH loc)
      | Pre_parser.STAR loc -> loop (Parser.STAR loc)
      | Pre_parser.STATIC loc -> loop (Parser.STATIC loc)
      | Pre_parser.STATIC_ASSERT loc -> loop (Parser.STATIC_ASSERT loc)
      | Pre_parser.STRING_LITERAL (enc, str, loc) ->
          (* Merge consecutive string literals *)
          let rec doConcat enc str =
            match Queue.peek tokens with
            | Pre_parser.STRING_LITERAL (enc', str', loc') ->
               ignore (Queue.pop tokens);
               let (enc'', str'') = doConcat enc' str' in
               if str'' <> []
               then (combine_encodings loc enc enc'', str @ str'')
               else (enc, str)
            | _ -> (enc, str)
            | exception Queue.Empty -> (enc, str)
          in
          let (enc', str') = doConcat enc str in
          loop (Parser.STRING_LITERAL ((enc', str'), loc))
      | Pre_parser.STRUCT loc -> loop (Parser.STRUCT loc)
      | Pre_parser.SUB_ASSIGN loc -> loop (Parser.SUB_ASSIGN loc)
      | Pre_parser.SWITCH loc -> loop (Parser.SWITCH loc)
      | Pre_parser.TILDE loc -> loop (Parser.TILDE loc)
      | Pre_parser.TYPEDEF loc -> loop (Parser.TYPEDEF loc)
      | Pre_parser.TYPEDEF_NAME (id, typ, loc)
      | Pre_parser.VAR_NAME (id, typ, loc) ->
          begin match !typ with
          | VarId -> loop (Parser.VAR_NAME (id, loc))
          | TypedefId -> loop (Parser.TYPEDEF_NAME (id, loc))
          | OtherId -> loop (Parser.OTHER_NAME (id, loc))
          end
      | Pre_parser.UNION loc -> loop (Parser.UNION loc)
      | Pre_parser.UNSIGNED loc -> loop (Parser.UNSIGNED loc)
      | Pre_parser.VOID loc -> loop (Parser.VOID loc)
      | Pre_parser.VOLATILE loc -> loop (Parser.VOLATILE loc)
      | Pre_parser.WHILE loc -> loop (Parser.WHILE loc)
      | Pre_parser.XOR_ASSIGN loc -> loop (Parser.XOR_ASSIGN loc)
      | Pre_parser.ALIGNAS loc -> loop (Parser.ALIGNAS loc)
      | Pre_parser.ALIGNOF loc -> loop (Parser.ALIGNOF loc)
      | Pre_parser.ATTRIBUTE loc -> loop (Parser.ATTRIBUTE loc)
      | Pre_parser.RCANNO (a, loc) -> loop (Parser.RCANNO (a, loc))
      | Pre_parser.ASM loc -> loop (Parser.ASM loc)
      | Pre_parser.PRAGMA (s, loc) -> loop (Parser.PRAGMA (s, loc))
      | Pre_parser.PRE_NAME _ -> assert false
    in
    Lazy.from_fun compute_buffer

}
