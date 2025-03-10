(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Topkg test description. *)

type t

val v :
  Topkg_fpath.t -> args:Topkg_cmd.t -> run:bool ->
  dir:Topkg_fpath.t option -> t

val exec : t -> Topkg_fpath.t
val args : t -> Topkg_cmd.t
val run : t -> bool
val dir : t -> Topkg_fpath.t option
val codec : t Topkg_codec.t
