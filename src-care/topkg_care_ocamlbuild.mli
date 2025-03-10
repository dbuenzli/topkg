(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [ocamlbuild] helpers.

    See {!Topkg_care.OCamlbuild}. *)

(** {1 OCamlbuild} *)

open Bos_setup

val cmd : Cmd.t
val package_tags : ?roots:bool -> Fpath.t -> (String.set, R.msg) result
