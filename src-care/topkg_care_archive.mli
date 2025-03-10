(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Archive creation.

    See {!Topkg_care.Archive}. *)

open Bos_setup

(** {1 Ustar archives} *)

(** ustar encoder.

    {b References}.
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html#tag_20_92_13_06}ustar Interchange Format} in POSIX 1003.1, 2013. *)
module Tar : sig

  (** {1 Ustar encoder} *)

  type ptime = int
  (** The type for POSIX times in seconds since the epoch. *)

  type t
  (** The type for ustar archives. *)

  val empty : t
  (** [empty] is the empty ustar archive. *)

  val add :
    t -> Fpath.t -> mode:int -> mtime:ptime -> [`Dir | `File of string ] ->
    (t, R.msg) result
  (** [add a f mode mtime kind] adds to archive [a] an element of
      type [kind] with file path [f], permission mode [mode] and modificaton
      time [mtime]. *)

  val to_string : t -> string
  (** [to_string a] is the byte serialization of the archive [a]. *)
end

val tar :
  Fpath.t -> exclude_paths:Fpath.set -> root:Fpath.t -> mtime:int ->
  (string, R.msg) result

(** {1 Bzip2 compression and unarchiving} *)

val ensure_bzip2 : unit -> (unit, R.msg) result
val bzip2 : string -> dst:Fpath.t -> (unit, R.msg) result
val ensure_tar : unit -> (unit, R.msg) result
val untbz : ?clean:bool -> Fpath.t -> (Fpath.t, R.msg) result
