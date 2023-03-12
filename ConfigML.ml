(* Static configuration for ML *)

type block_comment_delimiters = <opening : string; closing : string>
type line_comment_delimiter   = string (* Opening of a line comment *)
type string_delimiter         = string
type verbatim_delimiters      = <opening : string; closing : string>

let block =
  object
    method opening = "(*"
    method closing = "*)"
  end

let block    = Some block
let line     = None
let string   = Some "\""
let file_ext = Some ".ml"

let verbatim =
  object
    method opening = "{|"
    method closing = "|}"
  end

let verbatim = Some verbatim
