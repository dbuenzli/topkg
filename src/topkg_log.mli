(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Log

    Abridged [logs]. See {!Topkg.Log} for documentation. *)

(** {1 Log} *)

open Topkg_result

type level = App | Error | Warning | Info | Debug

val level : unit -> level option
val set_level : level option -> unit
val level_to_string : level option -> string
val level_of_string : string -> (level option, [`Msg of string]) r

type 'a msgf =
  (?header:string -> ('a, Format.formatter, unit) format -> 'a) -> unit

val msg : level -> 'a msgf -> unit
val app : 'a msgf -> unit
val err : 'a msgf -> unit
val warn : 'a msgf -> unit
val info : 'a msgf -> unit
val debug : 'a msgf -> unit

val on_error_msg : ?level:level -> use:(unit -> 'a) -> 'a result -> 'a
val err_count : unit -> int
val warn_count : unit -> int
