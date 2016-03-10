(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {1 Distribution description} *)

open Topkg_result

(* Watermarks *)

type watermark_def =
[ `String of string | `Name | `Version | `Vcs of [ `Commit_id ]
| `Opam of string * string * string ]

type watermark = string * watermark_def

val define_watermarks :
  name:string -> version:string -> watermark list -> (string * string) list

val default_watermarks : watermark list
val default_files_to_watermark : unit -> Topkg_fpath.t list result

val watermark_file : (string * string) list -> Topkg_fpath.t -> unit result
val watermark_files :
  (string * string) list -> Topkg_fpath.t list -> unit result

(* Distribution *)

val default_commit_ish : unit -> string result
val default_version : commit_ish:string -> string result
val default_exclude_paths : unit -> Topkg_fpath.t list result

type t

val v :
  ?commit_ish:(unit -> string result) ->
  ?version:(commit_ish:string -> string result) ->
  ?watermarks:watermark list ->
  ?files_to_watermark:(unit -> Topkg_fpath.t list result) ->
  ?massage:(unit -> unit result) ->
  ?exclude_paths:(unit -> Topkg_fpath.t list result) ->
  unit -> t

val commit_ish : t -> string result
val version : t -> commit_ish:string -> string result
val watermarks : t -> watermark list
val files_to_watermark : t -> Topkg_fpath.t list result
val massage : t -> unit result
val exclude_paths : t -> Topkg_fpath.t list result

val run_prepare :
  name:string -> version:string -> t -> Topkg_fpath.t list result

val run_prepare_pin : name:string -> t -> unit result

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
