(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command lines

    See {!Topkg.Cmd} for documentation. *)

(** {1 Command lines} *)

type t

val v : string -> t
val empty : t
val is_empty : t -> bool
val ( % ) : t -> string -> t
val ( %% ) : t -> t -> t
val add_arg : t -> string -> t
val add_args : t -> t -> t
val on : bool -> t -> t
val p : Topkg_fpath.t -> string

val equal : t -> t -> bool
val compare : t -> t -> int

val to_rev_list : t -> string list
val to_list : t -> string list
val of_list : ?slip:string -> string list -> t
val dump : Format.formatter -> t -> unit
