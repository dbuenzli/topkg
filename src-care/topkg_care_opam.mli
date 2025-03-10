(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** opam interaction.

    See {!Topkg_care.Opam}. *)

(** {1 opam} *)

open Bos_setup

val cmd : Cmd.t
val ensure_publish : unit -> (unit, R.msg) result
val submit : ?msg:string -> opam_file:Fpath.t -> (unit, R.msg) result
val ocaml_base_packages : String.set

module File : sig
  val field_names : String.set
  val fields : Fpath.t -> ((string list) String.map , R.msg) result
  val deps : ?opts:bool -> (string list) String.map -> String.set
end

module Descr : sig
  type t = string * string
  val of_string : string -> (t, R.msg) result
  val to_string : t -> string
  val to_opam_fields : t -> string
  val of_readme :
    ?flavour:Topkg_care_text.flavour -> string -> (t, R.msg) result
  val of_readme_file : Fpath.t -> (t, R.msg) result
end

module Url : sig
  type t = string
  val v : uri:string -> checksum:string -> t
  val with_distrib_file : uri:string -> Fpath.t -> (t, R.msg) result
  val to_opam_section : t -> string
end
