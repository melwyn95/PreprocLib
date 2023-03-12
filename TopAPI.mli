(* This module is meant to be used by clients of the library to create
   standalone preprocessors tailored to their conventions. It builds
   upon [LowAPI] by adding support for reading the CLI and specifying
   the source as one value of type [Lexbuf.input]. By contrast,
   [LowAPI] exports a preprocessor for each type of input. *)

(* Dependencies *)

module Lexbuf = Utilities.Lexbuf
module Std    = Utilities.Std

(* Functor *)

module type S =
  sig
    (* Checking the CLI *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    val check_cli : unit -> cli_status

    (* Running the preprocessor *)

    val preprocess : Lexbuf.input -> Std.t * LowAPI.result
  end

module Make (Parameters : CLI.PARAMETERS) : S
