(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Topkg interprocess communication.

    See {!Topkg.Private.Ipc} for documentation. *)

open Topkg_result

(** {1 Interprocess communication} *)

type 'a t

val v : ?answer:Topkg_fpath.t -> Topkg_cmd.t -> 'a Topkg_codec.t -> 'a t
val cmd : 'a t -> Topkg_cmd.t
val codec : 'a t -> 'a Topkg_codec.t
val answer : 'a t -> Topkg_fpath.t

val pkg : unit -> Topkg_pkg.t t
val lint_custom : unit -> Topkg_result.R.msg Topkg_result.result list option t
val distrib_prepare :
  dist_build_dir:string -> name:string -> version:string -> opam:string ->
  opam_adds:string -> Topkg_fpath.t list result t

val write_answer : Topkg_cmd.t -> Topkg_pkg.t -> unit Topkg_result.result
