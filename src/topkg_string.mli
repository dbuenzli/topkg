(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Strings.

    See {!Topkg.String} for documentation. *)

val strf : ('a, Format.formatter, unit, string) format4 -> 'a

include module type of String

val head : string -> char option

val is_prefix : affix:string -> string -> bool
val is_suffix : affix:string -> string -> bool
val for_all : (char -> bool) -> string -> bool
val exists : (char -> bool) -> string -> bool

val find_byte : ?start:int -> char -> string -> int option

val trim : string -> string
val cut : ?rev:bool -> sep:char -> string -> (string * string) option
val cuts : ?empty:bool -> sep:char -> string -> string list

val with_index_range : ?first:int -> ?last:int -> string -> string

val uppercase_ascii : string -> string

val parse_version : string -> (int * int * int * string option) option
val drop_initial_v : string -> string

val pp_text : Format.formatter -> string -> unit
