(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Entry point for [pkg.ml] files. *)

(** {1 Main} *)

open Topkg_result

val describe :
  ?delegate:Topkg_cmd.t ->
  ?readmes:Topkg_pkg.std_file list ->
  ?licenses:Topkg_pkg.std_file list ->
  ?change_logs:Topkg_pkg.std_file list ->
  ?metas:Topkg_pkg.meta_file list ->
  ?opams:Topkg_pkg.opam_file list ->
  ?lint_files:Topkg_fpath.t list option ->
  ?lint_custom:(unit -> R.msg result list) ->
  ?distrib:Topkg_distrib.t ->
  ?publish:Topkg_publish.t ->
  ?build:Topkg_build.t ->
  string -> (Topkg_conf.t -> Topkg_install.t list result) -> unit

val disable : unit -> unit
