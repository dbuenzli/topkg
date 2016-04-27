(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Package descriptions. *)

open Topkg_result

type t

val empty : t
val v :
  ?delegate:Topkg_cmd.t ->
  ?std_files:Topkg_std_files.t ->
  ?lint:Topkg_lint.t ->
  ?distrib:Topkg_distrib.t ->
  ?build:Topkg_build.t -> string ->
  Topkg_install.t list -> t

val name : t -> string
val delegate : t -> Topkg_cmd.t option
val std_files : t -> Topkg_std_files.t
val distrib : t -> Topkg_distrib.t
val build : t -> Topkg_build.t
val install : t -> Topkg_install.t

val codec : t Topkg_codec.t

(* Derived accessors *)

val build_dir : t -> Topkg_fpath.t
val readme : t -> Topkg_fpath.t
val change_log : t -> Topkg_fpath.t
val license : t -> Topkg_fpath.t
val opam : name:string -> t -> Topkg_fpath.t

(* Distrib *)

val distrib_uri : t -> string option
val distrib_prepare_pin : t -> unit result
val distrib_prepare :
  t -> dist_build_dir:Topkg_fpath.t -> name:string -> version:string ->
  opam:Topkg_fpath.t -> Topkg_fpath.t list result

(* Build *)

val run_build : t -> int result

(* Lint *)

val lint_custom : t -> (unit -> R.msg result list) option
val lint_files : t -> Topkg_fpath.t list option
val lint_metas : t -> Topkg_fpath.t list option
val lint_opams : t -> (Topkg_fpath.t * string list option) list option

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
