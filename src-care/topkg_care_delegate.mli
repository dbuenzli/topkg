(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Package delegate.

    See {!Topkg_care.Delegate} for documentation. *)

open Bos_setup

(** {1 Publish} *)

val publish_distrib :
  Topkg_care_pkg.t -> msg:string -> archive:Fpath.t ->
  (unit, R.msg) result

val publish_doc :
  Topkg_care_pkg.t -> msg:string -> docdir:Fpath.t ->
  (unit, R.msg) result

val publish_alt :
  Topkg_care_pkg.t -> kind:string -> msg:string -> archive:Fpath.t ->
  (unit, R.msg) result

val publish_in_git_branch :
  remote:string -> branch:string ->
  name:string -> version:string -> docdir:Fpath.t ->
  dir:Fpath.t -> (unit, R.msg) result

(** {1 Delegate} *)

val issue_list : Topkg_care_pkg.t -> (unit, R.msg) result
val issue_show : Topkg_care_pkg.t -> id:string -> (unit, R.msg) result

val issue_open :
  Topkg_care_pkg.t -> title:string -> body:string -> (unit, R.msg) result

val issue_close :
  Topkg_care_pkg.t -> id:string -> msg:string -> (unit, R.msg) result
