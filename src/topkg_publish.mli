(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {1 Distribution publication description}

    See {!section:Topkg.Pkg.publish}. *)

(** {1 Distribution} *)

open Topkg_result

(* Publication *)

type artefact = [`Distrib | `Doc | `Alt of string]
type t

val v : ?artefacts:artefact list -> unit -> t
val artefacts : t -> artefact list
val codec : t Topkg_codec.t
