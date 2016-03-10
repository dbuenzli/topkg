(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Distribution creation.

    See {!Topkg_care.Distrib}. *)

open Rresult
open Bos

(** {1 Ustar archives} *)

val tar :
  Fpath.t -> exclude_paths:Topkg.fpath list -> root:Fpath.t -> mtime:int ->
  (string, R.msg) result

(** {1 Bzip2 compression} *)

val ensure_bzip2 : unit -> (unit, R.msg) result
val bzip2 : string -> dst:Fpath.t -> (unit, R.msg) result

(** {1 Distribution} *)

type det

val build_dir : det -> Fpath.t
val name : det -> string
val commit_ish : det -> Topkg.Vcs.commit_ish
val version : det -> string
val rootname : ?opam:bool -> det -> Fpath.t
val determine :
  pkg_file:Fpath.t -> build_dir:Topkg.fpath option ->
  name:string option -> commit_ish:Topkg.Vcs.commit_ish option ->
  version:string option -> (det, R.msg) Result.result

val clone_repo : det -> Topkg.Vcs.t -> (Fpath.t, R.msg) result
val prepare_repo :
  det -> dist_pkg_file:Fpath.t -> dir:Fpath.t ->
  (int * Topkg.fpath list, R.msg) result

val archive_path : det -> Fpath.t
val archive :
  det -> keep_dir:bool -> dir:Fpath.t -> exclude_paths:Topkg.fpath list ->
  mtime:int -> (Fpath.t, R.msg) result

val unarchive : ?clean:bool -> Fpath.t -> (Fpath.t, R.msg) result

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
