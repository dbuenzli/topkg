(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Package descriptions.

    See {!Topkg_care.Pkg} *)

open Bos_setup

(** {1 Package} *)

type t

val v :
  ?name:string ->
  ?version:string ->
  ?delegate:Cmd.t ->
  ?build_dir:Fpath.t ->
  ?opam:Fpath.t ->
  ?opam_descr:Fpath.t ->
  ?readme:Fpath.t ->
  ?change_log:Fpath.t ->
  ?license:Fpath.t ->
  ?distrib_uri:string ->
  ?distrib_file:Fpath.t ->
  ?publish_msg:string ->
  ?publish_artefacts:[ `Distrib | `Doc | `Alt of string] list ->
  Fpath.t -> t

val pkg_file : t -> Fpath.t
val name : t -> (string, R.msg) result
val version : t -> (string, R.msg) result
val delegate : t -> (Cmd.t, R.msg) result
val build_dir : t -> (Fpath.t, R.msg) result
val opam : t -> (Fpath.t, R.msg) result
val opam_field : t -> string -> (string list option, R.msg) result
val opam_field_hd : t -> string -> (string option, R.msg) result
val opam_fields : t -> (string list String.map, R.msg) result
val opam_descr : t -> (Topkg_care_opam.Descr.t * bool, R.msg) result
val readmes : t -> (Fpath.t list, R.msg) result
val readme : t -> (Fpath.t, R.msg) result
val change_logs : t -> (Fpath.t list, R.msg) result
val change_log : t -> (Fpath.t, R.msg) result
val licenses : t -> (Fpath.t list, R.msg) result
val distrib_uri : ?raw:bool -> t -> (string, R.msg) result
val distrib_file : t -> (Fpath.t, R.msg) result
val publish_msg : t -> (string, R.msg) result
val publish_artefacts : t ->
  ([ `Distrib | `Doc | `Alt of string] list, R.msg) result

(** {1 Test} *)

val test :
  t -> dir:Fpath.t -> args:Cmd.t ->
  out:(OS.Cmd.run_out -> ('a, R.msg) result) -> ('a, R.msg) result

(** {1 Build} *)

val build :
  t -> dir:Fpath.t -> args:Cmd.t ->
  out:(OS.Cmd.run_out -> ('a, R.msg) result) -> ('a, R.msg) result

(** {1 Clean} *)

val clean :
  t -> dir:Fpath.t -> args:Cmd.t ->
  out:(OS.Cmd.run_out -> ('a, R.msg) result) -> ('a, R.msg) result

(** {1 Distrib} *)

val distrib_filename : ?opam:bool -> t -> (Fpath.t, R.msg) result
val distrib_archive : t -> keep_dir:bool -> (Fpath.t, R.msg) result

(** {1 Lint} *)

type lint = [ `Custom | `Std_files | `Meta | `Opam | `Deps ]
val lint_all : lint list
val lint :
  ?ignore_pkg:bool -> t -> dir:Fpath.t -> lint list -> (int, R.msg) result
