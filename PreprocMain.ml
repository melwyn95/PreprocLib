(* Building a preprocessor for ML *)

(* Dependencies *)

module Lexbuf = Utilities.Lexbuf
module Std    = Utilities.Std

(* Local dependencies *)

module CLI      = PreprocLib.CLI
module ParamsML = CLI.Make (ConfigML)
module Main     = PreprocLib.TopAPI.Make (ParamsML)

(* Entry point *)

let run () =
  match Main.check_cli () with
    Main.Ok ->
      let file   = Option.value ParamsML.Options.input ~default:"" in
      let std, _ = Main.preprocess (Lexbuf.File file)
      in begin
           Printf.printf  "%s%!" (Std.string_of std.out);
           Printf.eprintf "%s%!" (Std.string_of std.err)
         end
  | Info  msg -> Printf.printf "%s%!" msg (* Note the absence of "\n" *)
  | Error msg -> Printf.eprintf "%s\n%!" msg

let () = run ()
