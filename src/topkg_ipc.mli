(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Topkg interprocess communication.

    See {!Topkg.Private.Ipc} for documentation. *)

open Topkg_result

(** {1 Interprocess communication} *)

type 'a t

val v : Topkg_cmd.t -> 'a Topkg_codec.t -> 'a t
val cmd : 'a t -> Topkg_cmd.t
val codec : 'a t -> 'a Topkg_codec.t

val delegate : (string option * Topkg_std_files.t) t
val std_files : Topkg_std_files.t t
val lint : custom:bool -> (Topkg_std_files.t * Topkg_lint.t) t
val distrib_commit_ish : Topkg_vcs.commit_ish result t
val distrib_determine :
  build_dir:Topkg_fpath.t option -> name:string option ->
  commit_ish:Topkg_vcs.commit_ish option -> version:string option ->
  (Topkg_fpath.t * string * Topkg_vcs.commit_ish * string) result t

val distrib_prepare :
  name:string -> version:string -> Topkg_fpath.t list result t

val answer : Topkg_cmd.t -> Topkg_pkg.t -> int Topkg_result.result

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
