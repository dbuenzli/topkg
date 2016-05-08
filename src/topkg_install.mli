(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Package install. *)

type file = string * Topkg_fexts.ext
type field_move =
  { field : Topkg_opam.Install.field;
    built : bool;
    src : file;
    dst : file; }

type t = field_move list

type field =
  ?built:bool -> ?cond:bool -> ?exts:Topkg_fexts.t ->
  ?dst:string -> string -> t

val lib : field
val libexec : ?auto:bool -> field
val bin : ?auto:bool -> field
val sbin : ?auto:bool -> field
val toplevel : field
val share : field
val share_root : field
val etc : field
val doc : field
val stublibs : field
val misc : field
val man : field
val mllib :
  ?field:field -> ?cond:bool -> ?api:string list -> ?dst_dir:Topkg_fpath.t ->
  Topkg_fpath.t -> t

val to_instructions :
  ?header:string ->
  Topkg_conf.t ->
  Topkg_conf.os -> t -> (Topkg_fpath.t list * Topkg_opam.Install.t)

val codec : t Topkg_codec.t

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
