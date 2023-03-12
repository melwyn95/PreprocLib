{
(* START OF HEADER *)

(* Dependencies *)

module Lexbuf = Utilities.Lexbuf
module Region = SourceLoc.Region

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

(* Raising an exception for errors *)

let fail state region error =
  let value = Error.to_string error in
  let msg   = Region.{value; region} in
  List.iter close_in state#chans;
  raise (Error.Error (state#out, msg))

let stop state lexbuf = fail state @@ Region.from_lexbuf lexbuf

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

(* RULES *)

rule scan state = parse
  blank+  { scan (fst @@ state#sync lexbuf) lexbuf }
| "//"    { let state, opening = state#sync lexbuf
            in comment opening [] state lexbuf     }
| nl      { E_Parser.EOL   (state#newline lexbuf)  }
| eof     { E_Parser.EOL   (state#sync lexbuf)     }
| "true"  { E_Parser.True  (state#sync lexbuf)     }
| "false" { E_Parser.False (state#sync lexbuf)     }
| ident   { E_Parser.Ident (state#sync lexbuf)     }
| '('     { E_Parser.LPAR  (state#sync lexbuf)     }
| ')'     { E_Parser.RPAR  (state#sync lexbuf)     }
| "||"    { E_Parser.OR    (state#sync lexbuf)     }
| "&&"    { E_Parser.AND   (state#sync lexbuf)     }
| "=="    { E_Parser.EQ    (state#sync lexbuf)     }
| "!="    { E_Parser.NEQ   (state#sync lexbuf)     }
| "!"     { E_Parser.NOT   (state#sync lexbuf)     }
| _ as c  { stop state lexbuf (Error.Invalid_character c) }

and comment opening acc state = parse
  nl | eof { let ()      = Lexbuf.rollback lexbuf in
             let value   = opening.Region.value ^ mk_string acc
             and start   = opening.Region.region#start in
             let region  = Region.make start state#pos in
             let comment = Region.{region; value}
             in E_Parser.COMMENT (state, comment) }
| _ as c   { let state, _ = state#sync lexbuf in
             comment opening (c::acc) state lexbuf }

{
(* START OF TRAILER *)

let string_of ctor (args : State.t * string Region.reg) =
  let state, Region.{value; region} = args in
  let pos = state#pos#compact `Point
  and reg = region#compact `Point in
  Printf.sprintf "%s (%s,%s,%S)" ctor pos reg value

let string_of_token =
  let open E_Parser in
  function
    Ident   args -> string_of "Ident"   args
  | True    args -> string_of "True"    args
  | False   args -> string_of "False"   args
  | OR      args -> string_of "OR"      args
  | AND     args -> string_of "AND"     args
  | EQ      args -> string_of "EQ"      args
  | NEQ     args -> string_of "NEQ"     args
  | NOT     args -> string_of "NOT"     args
  | LPAR    args -> string_of "LPAR"    args
  | RPAR    args -> string_of "RPAR"    args
  | COMMENT args -> string_of "COMMENT" args
  | EOL     args -> string_of "EOL"     args

let state_of =
  let open E_Parser in
  function
    Ident   (state, _)
  | True    (state, _)
  | False   (state, _)
  | OR      (state, _)
  | AND     (state, _)
  | EQ      (state, _)
  | NEQ     (state, _)
  | NOT     (state, _)
  | LPAR    (state, _)
  | RPAR    (state, _)
  | COMMENT (state, _)
  | EOL     (state, _) -> state

let scan (state : State.t) =
  let last_state = ref state in
  fun lexbuf ->
    let token = scan !last_state lexbuf in
    let () = last_state := state_of token
    in token

(* END OF TRAILER *)
}
