(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text processing helpers.

    See {!Topkg_care.Text}. *)

(** {1 Text} *)

open Bos_setup

type flavour = [ `Markdown | `Asciidoc ]

val flavour_of_fpath : Fpath.t -> flavour option
val head : ?flavour:flavour -> string -> (string * string) option
val header_title : ?flavour:flavour -> string -> string

val change_log_last_entry :
  ?flavour:flavour -> string -> (string * (string * string)) option

val change_log_file_last_entry :
  Fpath.t -> ((string * (string * string)), R.msg) result

val split_uri : ?rel:bool -> string -> (string * string * string) option

val find_pager : don't:bool -> (Cmd.t option, R.msg) result
val edit_file : Fpath.t -> (int, R.msg) result

module Pp : sig
  val name : string Fmt.t
  val version : string Fmt.t
  val commit : string Fmt.t
  val dirty : unit Fmt.t
  val path : Fpath.t Fmt.t
  val status : [`Ok | `Fail] Fmt.t
end
