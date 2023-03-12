(* Scanning of preprocessing directives *)

{
(* START OF HEADER *)

(* Dependencies *)

module Region = SourceLoc.Region
module Pos    = SourceLoc.Pos

(* UTILITIES *)

let sprintf = Printf.sprintf

let (let*) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result =
  fun r f ->
    match r with
      Ok x         -> f x
    | Error _ as e -> e

(* ERRORS *)

type error = Region.t * Error.t

let mk_Error1 region error = Stdlib.Error (region, error)

let mk_Error2 lexbuf = mk_Error1 @@ Region.from_lexbuf lexbuf

let missing_space lexbuf =
  let region      = Region.from_lexbuf lexbuf in
  let zero_length = Region.empty region#start
  in mk_Error1 zero_length Error.Missing_space

(* COMPOUNDS *)

(* STRING PROCESSING *)

(* The value of [mk_string p] ("make string") is a string containing
   the characters in the list [p], in reverse order. For instance,
   [mk_string ['c';'b';'a'] = "abc"]. *)

let mk_string (p : char list) : string =
  let len   = List.length p in
  let bytes = Bytes.make len ' ' in
  let rec fill i = function
    [] -> bytes
  | char::l -> Bytes.set bytes i char; fill (i-1) l
  in fill (len-1) p |> Bytes.to_string

(* DIRECTIVES *)

type file_path   = string
type module_name = string
type message     = string
type variable    = string
type flag        = Push | Pop

(* Endings of preprocessing directives *)

type ending = [
  `EOL of State.t * string Region.reg
| `EOF of State.t * Region.t]

(* #include *)

type include_directive = <
  region           : Region.t;
  file_path        : file_path Region.reg;
  trailing_comment : string Region.reg option
>

let mk_include ?trailing_comment dir_region file_path =
  object
    method region           : Region.t = dir_region
    method file_path        : file_path Region.reg = file_path
    method trailing_comment : message Region.reg option = trailing_comment
  end

(* #if and #elif *)

type bool_expr = <
  region           : Region.t;
  expression       : E_AST.t;
  trailing_comment : string Region.reg option
>

type if_directive   = bool_expr
type elif_directive = bool_expr

let mk_bool_expr ?trailing_comment dir_region expr =
  object
    method region           : Region.t = dir_region
    method expression       : E_AST.t = expr
    method trailing_comment : message Region.reg option = trailing_comment
  end

(* #define and #undef *)

type symbol = <
  region           : Region.t;
  symbol           : variable Region.reg;
  trailing_comment : message Region.reg option
>

type define_directive = symbol
type undef_directive  = symbol

let mk_symbol ?trailing_comment dir_region sym =
  object
    method region           : Region.t = dir_region
    method symbol           : variable Region.reg = sym
    method trailing_comment : message Region.reg option = trailing_comment
  end

(* #error *)

type error_directive = Region.t * string Region.reg

(* Linemarkers (line directives) *)

type line_directive = <
  region    : Region.t;
  linenum   : int Region.reg;
  file_path : string Region.reg;
  flag      : flag Region.reg option
>

let mk_line_directive dir_region linenum file_path flag =
  object
    method region    : Region.t = dir_region
    method linenum   : int Region.reg = linenum
    method file_path : string Region.reg = file_path
    method flag      : flag Region.reg option = flag
  end

(* ALL DIRECTIVES *)

type t =
  PP_Include    of include_directive
| PP_If         of if_directive
| PP_Elif       of elif_directive
| PP_Else       of Region.t
| PP_Endif      of Region.t
| PP_Define     of define_directive
| PP_Undef      of undef_directive
| PP_Error      of error_directive
| PP_Linemarker of line_directive

(* PROJECTIONS *)

(* Regions *)

let to_region = function
  PP_Include    d -> d#region
| PP_If         d -> d#region
| PP_Elif       d -> d#region
| PP_Define     d -> d#region
| PP_Undef      d -> d#region
| PP_Linemarker d -> d#region
| PP_Else       r
| PP_Endif      r
| PP_Error  (r,_) -> r

(* Printing the abstract syntax *)

let project = function
  PP_Include incl ->
    let dir_region = incl#region
    and file_reg   = incl#file_path.Region.value
    and trailer    = match incl#trailing_comment with
                       Some comment -> comment.Region.value
                     | None         -> "" in
    dir_region, sprintf "PP_Include (%S, %S)" file_reg trailer

| PP_If if_dir ->
    let dir_region = if_dir#region
    and ast        = E_AST.to_string if_dir#expression
    and trailer    = match if_dir#trailing_comment with
                       Some comment -> comment.Region.value
                     | None         -> "" in
    dir_region, sprintf "PP_If (%S, %S)" ast trailer

| PP_Elif elif_dir ->
    let dir_region = elif_dir#region
    and ast        = E_AST.to_string elif_dir#expression
    and trailer    = match elif_dir#trailing_comment with
                       Some comment -> comment.Region.value
                     | None         -> "" in
    dir_region, sprintf "PP_Elif (%S, %S)" ast trailer

| PP_Else region ->  region, "PP_Else"

| PP_Endif region -> region, "PP_Endif"

| PP_Define sym ->
    let dir_region = sym#region
    and sym_reg    = sym#symbol.Region.value
    and trailer    = match sym#trailing_comment with
                       Some comment -> comment.Region.value
                     | None         -> "" in
    dir_region, sprintf "PP_Define (%s, %S)" sym_reg trailer

| PP_Undef sym ->
    let dir_region = sym#region
    and sym_reg    = sym#symbol.Region.value
    and trailer    = match sym#trailing_comment with
                       Some comment -> comment.Region.value
                     | None         -> "" in
    dir_region, sprintf "PP_Undef (%s, %S)" sym_reg trailer

| PP_Error (dir_region, msg_reg) ->
    dir_region, sprintf "PP_Error %S" msg_reg.Region.value

| PP_Linemarker linemarker ->
    let open Region in
    let dir_region = linemarker#region
    and {value=linenum; _} = linemarker#linenum
    and file_path = linemarker#file_path.value in
    let flag_lex =
      match linemarker#flag with
        Some {value=Push; _} -> "Push"
      | Some {value=Pop; _} -> "Pop"
      | None -> "None" in
    dir_region, sprintf "PP_Linemarker (%d, %S, %s)"
                        linenum file_path flag_lex

let to_string ~offsets mode directive =
  let region, string = project directive in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str string

(* Note how the returned region is relative to the directive in the
   input file, whereas the returned string (field [value]) is the
   pretty-printed version. *)

let to_lexeme = function
  PP_Include incl ->
    let dir_region = incl#region
    and file_reg   = incl#file_path in
    let value      = sprintf "#include %S" file_reg.value
    and region     = Region.cover dir_region file_reg.region
    in Region.{value; region}

| PP_If if_dir ->
    let dir_region = if_dir#region
    and ast = if_dir#expression in
    let value  = sprintf "#if %s" (E_AST.to_string ast)
    and region = Region.cover dir_region (E_AST.to_region ast)
    in Region.{value; region}

| PP_Elif elif_dir ->
    let dir_region = elif_dir#region
    and ast = elif_dir#expression in
    let value  = sprintf "#elif %s" (E_AST.to_string ast)
    and region = Region.cover dir_region (E_AST.to_region ast)
    in Region.{value; region}

| PP_Else region ->
    Region.{value = sprintf "#else"; region}

| PP_Endif region ->
    Region.{value = sprintf "#endif"; region}

| PP_Define sym ->
    let dir_region = sym#region
    and sym_reg    = sym#symbol in
    let value      = sprintf "#define %s" sym_reg.value
    and region     = Region.cover dir_region sym_reg.region
    in Region.{value; region}

| PP_Undef sym ->
    let dir_region = sym#region
    and sym_reg    = sym#symbol in
    let value      = sprintf "#undef %s" sym_reg.value
    and region     = Region.cover dir_region sym_reg.region
    in Region.{value; region}

| PP_Error (dir_region, msg_reg) ->
    let msg_str =
      if String.(msg_reg.value = "") then ""
      else " " ^ msg_reg.value in
    let value  = sprintf "#error%s" msg_str
    and region = Region.cover dir_region msg_reg.region
    in Region.{value; region}

| PP_Linemarker linemarker ->
    let open Region in
    let {value=linenum; region=start} = linemarker#linenum
    and file_path = linemarker#file_path.value in
    let flag_lex, stop =
      match linemarker#flag with
        Some {value=Push; region} -> " 1", region
      | Some {value=Pop; region} -> " 2", region
      | None -> "", start in
    let value =
      sprintf "# %d %S%s" linenum file_path flag_lex
    and region = Region.cover start stop
    in Region.{value; region}

(* END OF HEADER *)
}

(* REGULAR EXPRESSIONS *)

let nl      = '\n' | '\r' | "\r\n"
let blank   = ' ' | '\t'
let digit   = ['0'-'9']
let small   = ['a'-'z']
let capital = ['A'-'Z']
let letter  = small | capital
let ident   = letter (letter | '_' | digit)*
let natural = digit | digit+ digit (* Linemarkers *)
let flag    = '1' | '2' (* Linemarkers *)

(* RULES *)

(* Scanning the rest of the line right after "#include" *)

rule scan_include hash_pos state = parse
  blank+ { let state, _ = state#sync lexbuf in
           scan_include hash_pos state lexbuf }
| '"'    { let state, quote = state#sync lexbuf in
           let* state, file =
             scan_in_string quote [] state lexbuf in
           let* state, trailing_comment, ending =
             trailing_comment state lexbuf in
           let region =
             Region.make ~start:hash_pos ~stop:state#pos in
           let dir = mk_include ?trailing_comment region file
           in Ok (state, dir, PP_Include dir, ending) }
| _      { mk_Error2 lexbuf Error.Missing_filename }

and scan_in_string opening acc state = parse
  '"'    { let state, quote = state#sync lexbuf in
           let region = Region.(cover opening.region quote.region)
           and value  = mk_string acc
           in Ok (state, Region.{value; region})          }
| nl     { mk_Error2 lexbuf Error.Newline_in_string       }
| eof    { Error.Unterminated_string opening.Region.value
           |> mk_Error1 opening.Region.region             }
| ['\000' - '\031'] | ['\128' - '\255'] as c
           (* Control characters and 7-bit ASCII allowed *)
         { Error.Invalid_character_in_string c
           |> mk_Error2 lexbuf                            }
| _ as c { let state, _ = state#sync lexbuf in
           scan_in_string opening (c::acc) state lexbuf   }

and trailing_comment state = parse
  blank+ { let state, _ = state#sync lexbuf
           in trailing_comment state lexbuf }
| "//" ['\032'-'\255']* {
        let state, comment = state#sync lexbuf in
        let* state, ending = clear_line state lexbuf
        in Ok (state, Some comment, ending) }
| nl  { let state', reg = state#newline lexbuf
        in Ok (state, None, `EOL (state', reg)) }
| eof { let state', reg = state#sync lexbuf
        in Ok (state, None, `EOF (state', reg.Region.region)) }
| _   { mk_Error2 lexbuf Error.Unexpected_argument }

and clear_line state = parse
  nl     { let state', reg = state#newline lexbuf
           in Ok (state, `EOL (state', reg)) }
| eof    { let state', reg = state#sync lexbuf
           in Ok (state, `EOF (state', reg.Region.region)) }
| blank+ { let state, _ = state#sync lexbuf
           in clear_line state lexbuf }
| _      { mk_Error2 lexbuf Error.Unexpected_argument }


(* Scanning the rest of the line right after "#define" or "#undef" *)

and scan_def_undef mk_dir hash_pos state = parse
  blank+ {
    let state, _ = state#sync lexbuf in
    let* state, symbol = symbol state lexbuf in
    let* state, trailing_comment, ending =
      trailing_comment state lexbuf in
    let region =
      Region.make ~start:hash_pos ~stop:state#pos in
    let symbol = mk_symbol ?trailing_comment region symbol
    in Ok (state, symbol, mk_dir symbol, ending) }
| nl | eof { mk_Error2 lexbuf Error.Missing_symbol }
| _ { missing_space lexbuf }

and symbol state = parse
  ident { Ok (state#sync lexbuf)                }
| _     { mk_Error2 lexbuf Error.Invalid_symbol }

(* Discarding the rest of the line (see scan_linemarker, #else,
   #endif) *)

and skip_line state = parse
  nl  { let state', reg = state#newline lexbuf
        in state, `EOL (state', reg) }
| eof { let state', reg = state#sync lexbuf
        in state, `EOF (state', reg.Region.region) }
| _   { let state, _ = state#sync lexbuf
        in skip_line state lexbuf }

(* Scanning the rest of the line right after "#error" *)

and scan_message state = parse
  blank+ { let state, _ = state#sync lexbuf
           in Ok (in_message state#pos [] state lexbuf) }
| nl     { (* No message specified *)
           let state', reg = state#newline lexbuf in
           let msg         = Region.{reg with value = ""}
           in Ok (state, msg, `EOL (state', reg)) }
| eof    { (* No message specified *)
           let state', reg = state#sync lexbuf in
           let msg         = Region.{reg with value = ""}
           in Ok (state, msg, `EOF (state', reg.Region.region)) }
| _      { missing_space lexbuf }

and in_message start acc state = parse
  nl     { let region      = Region.make ~start ~stop:state#pos in
           let msg         = Region.{region; value = mk_string acc} in
           let state', reg = state#newline lexbuf
           in state, msg, `EOL (state', reg) }
| eof    { let region      = Region.make ~start ~stop:state#pos in
           let msg         = Region.{region; value = mk_string acc} in
           let state', reg = state#sync lexbuf
           in state, msg, `EOF (state', reg.Region.region) }
| _ as c { let state, _ = state#sync lexbuf
           in in_message start (c::acc) state lexbuf }

(* Scanning the rest of the line right after "#" and a natural number
   (linemarkers). See function [scan_linemarker] at the end of the
   trailer. *)

and scan_file_path state = parse
  blank+ { let state, _ = state#sync lexbuf in
           scan_file_path state lexbuf }
| '"'    { let state, quote = state#sync lexbuf in
           scan_in_string quote [] state lexbuf    }
| _      { mk_Error2 lexbuf Error.Missing_filename }

and scan_flag state = parse
  blank+ { scan_flag (fst @@ state#sync lexbuf) lexbuf }
| nl     { (* No flag *)
           let state', reg = state#newline lexbuf
           in Ok (state, None, `EOL (state', reg)) }
| eof    { let state', reg = state#sync lexbuf
           in Ok (state, None, `EOF (state', reg.Region.region)) }
| "1"    { let state, flag   = state#sync lexbuf in
           let flag          = Region.{flag with value = Push} in
           let state, ending = skip_line state lexbuf
           in Ok (state, Some flag, ending) }
| "2"    { let state, flag   = state#sync lexbuf in
           let flag          = Region.{flag with value = Pop} in
           let state, ending = skip_line state lexbuf
           in Ok (state, Some flag, ending) }
| _      { mk_Error2 lexbuf Error.Invalid_flag }

{
(* START OF TRAILER *)

  (* Scanning conditional directives #if and #elif *)

  let scan_expr mk_dir hash_pos state lexbuf =
    match E_Parser.expr (E_Lexer.scan state) lexbuf with
      exception E_Parser.Error ->
        mk_Error2 lexbuf Error.Parse_error
    | state, ast, trailing_comment, ending ->
        let stop   = state#pos in
        let region = Region.make ~start:hash_pos ~stop in
        let bool_expr =
          mk_bool_expr ?trailing_comment region ast in
        Ok (state, bool_expr, mk_dir bool_expr, ending)

  let scan_if   = scan_expr (fun bool_expr -> PP_If   bool_expr)
  let scan_elif = scan_expr (fun bool_expr -> PP_Elif bool_expr)

  (* Scanning conditional directive #else *)

  let scan_else hash_pos state lexbuf =
    let state, ending = skip_line state lexbuf in
    let region = Region.make ~start:hash_pos ~stop:state#pos
    in state, PP_Else region, ending

  (* Scanning conditional directive #endif *)

  let scan_endif hash_pos state lexbuf =
    let state, ending = skip_line state lexbuf in
    let region = Region.make ~start:hash_pos ~stop:state#pos
    in state, PP_Endif region, ending

  (* Scanning environment directive #define *)

  let scan_define = scan_def_undef (fun sym -> PP_Define sym)

  (* Scanning environment directive #undef *)

  let scan_undef = scan_def_undef (fun sym -> PP_Undef sym)

  (* Scanning #error *)

  let scan_error hash_pos state lexbuf =
    let* state, msg, ending = scan_message state lexbuf in
    let region = Region.make ~start:hash_pos ~stop:state#pos
    in Ok (state, msg, PP_Error (region, msg), ending)

  (* Scanning the rest of the line right after "# <n>" (linemarkers) *)

  let scan_linemarker hash_pos linenum state lexbuf =
    let* state, file         = scan_file_path state lexbuf in
    let* state, flag, ending = scan_flag state lexbuf in
    let value   = int_of_string linenum.Region.value in
    let linenum = Region.{linenum with value}
    and region  =
      Region.make ~start:hash_pos ~stop:state#pos in
    let line_directive =
      mk_line_directive region linenum file flag in
    let linemarker = PP_Linemarker line_directive
    in Ok (state, line_directive, linemarker, ending)

(* END OF TRAILER *)
}
