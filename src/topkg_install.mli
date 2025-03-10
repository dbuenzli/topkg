(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Package install. *)

(** {1 Install} *)

type t

val nothing : t
val flatten : t list -> t
val to_build :
  ?header:string ->
  Topkg_conf.t ->
  Topkg_conf.os -> t list ->
  (Topkg_fpath.t list * Topkg_opam.Install.t * Topkg_test.t list option)

type field =
  ?force:bool -> ?built:bool -> ?cond:bool -> ?exts:Topkg_fexts.t ->
  ?dst:string -> string -> t

val bin : ?auto:bool -> field
val doc : field
val etc : field
val lib : field
val lib_root : field
val libexec : ?auto:bool -> field
val libexec_root : ?auto:bool -> field
val man : field
val misc : field
val sbin : ?auto:bool -> field
val share : field
val share_root : field
val stublibs : field
val toplevel : field
val unknown : string -> field

val test :
  ?run:bool -> ?dir:Topkg_fpath.t -> ?args:Topkg_cmd.t -> ?auto:bool -> field

val mllib :
  ?field:field -> ?cond:bool -> ?cma:bool -> ?cmxa:bool -> ?cmxs:bool ->
  ?api:string list -> ?dst_dir:Topkg_fpath.t -> Topkg_fpath.t -> t

val clib :
  ?dllfield:field ->
  ?libfield:field ->
  ?cond:bool -> ?lib_dst_dir:Topkg_fpath.t -> Topkg_fpath.t -> t

val codec : t Topkg_codec.t
