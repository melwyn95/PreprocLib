module Parameters = CLI.Make (Config)
module Main       = TopAPI.Make (Parameters)

open Main

let run () =
  match check_cli () with
    Main.Ok ->
      let file   = Option.value Parameters.Options.input ~default:"" in
      let std, _ = preprocess (Lexbuf.File file)
      in begin
           Printf.printf  "%s%!" (Std.string_of std.out);
           Printf.eprintf "%s%!" (Std.string_of std.err)
         end
  | Info  msg -> Printf.printf "%s%!" msg (* Note the absence of "\n" *)
  | Error msg -> Printf.eprintf "%s\n%!" msg

let () = run ()
