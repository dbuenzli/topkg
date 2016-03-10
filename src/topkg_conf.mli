(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Package configuration *)

open Topkg_result

val bool : ?quiet:bool -> ?absent:(unit -> bool result) -> string -> bool
val string : ?quiet:bool -> ?absent:(unit -> string result) -> string ->
  string

type os = [ `Build_os | `Host_os ]
val tool : string -> os -> Topkg_cmd.t

module OCaml : sig
  type t
  val v : os -> t
  val find : string -> t -> string option
  val version : t -> int * int * int * string option
  val ext_obj : t -> string
  val ext_asm : t -> string
  val ext_lib : t -> string
  val ext_dll : t -> string
  val ext_exe : t -> string
  val native : t -> bool
  val native_dynlink : t -> bool
  val dump : Format.formatter -> t -> unit
end

val vcs : bool
val installer : bool
val build : [ `Dev | `Distrib | `Pin ]

(* TODO *)

val get : unit -> (string * [ `Bool of bool | `String of string ]) list
val cmd :
  [`Build | `Ipc of Topkg_cmd.t | `Unknown of string list | `Help ]

val warn_unused : unit -> unit

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
