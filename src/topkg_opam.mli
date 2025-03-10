(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** opam helpers.

    See also {!Topkg.Private.Opam}. *)

open Topkg_result

module File : sig
  type t = (string * string list) list
  val codec : t Topkg_codec.t
  val fields : Topkg_fpath.t -> t result
end

(** opam install file.

    A module to generate opam install files.

    {b Reference}.
    {{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}
    Syntax and semantics} of opam install files. *)
module Install : sig

  (** {1 opam install files} *)

  type field =
    [ `Bin
    | `Doc
    | `Etc
    | `Lib
    | `Lib_root
    | `Libexec
    | `Libexec_root
    | `Man
    | `Misc
    | `Sbin
    | `Share
    | `Share_root
    | `Stublibs
    | `Toplevel
    | `Unknown of string ]
  (** The type for opam install file fields. *)

  type move
  (** The type for file moves. *)

  val move : ?maybe:bool -> ?dst:Topkg_fpath.t -> Topkg_fpath.t -> move
  (** [move ~maybe ~dst src] moves [src] to [dst], where [dst] is a
      path relative to the directory corresponding to the
      {{!field}field}.  If [maybe] is [true] (defaults to [false]),
      then [src] may not exist, otherwise an install error will occur
      if the file doesn't exist. *)

  type t = [ `Header of string option ] * (field * move) list
  (** The type for opam install files. An optional starting header
      comment and a list of field moves. *)

  val to_string : t -> string
  (** [to_string t] is [t] as syntactically valid opam install file. *)
end
