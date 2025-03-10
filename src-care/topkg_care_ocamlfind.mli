(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [ocamlfind] helpers.

    See {!Topkg_care.OCamlfind}. *)

(** {1 OCamlfind} *)

open Bos_setup

val cmd : Cmd.t
val base_packages : String.set
