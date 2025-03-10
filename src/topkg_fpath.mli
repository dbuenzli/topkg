(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** File system paths.

    See {!Topkg.Fpath}. *)

(** {1 File system paths} *)

type t = string
val append : t -> t -> t
val ( // ) : t -> t -> t

val is_dir_path : t -> bool
val is_file_path : t -> bool

val basename : t -> string
val dirname : t -> string

val get_ext : t -> string
val has_ext : string -> t -> bool
val rem_ext : t -> t
