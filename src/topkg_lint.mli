(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Package distribution linting.

    See also {!val:Pkg.lint}. *)

open Topkg_result

(** {1 Lint} *)

type t
(** The type for package distribution linting descriptions. *)

val v :
  ?custom:(unit -> R.msg result list) ->
  ?files:Topkg_fpath.t list option ->
  ?meta:bool ->
  ?opam:bool ->
  ?deps_excluding:string list option ->
  unit -> t

val custom : t -> (unit -> R.msg result list) option
(** [custom l] is [l]'s custom linting process. *)

val files : t -> Topkg_fpath.t list option
(** [files l] is [l]'s file existence linting descriptions. *)

val meta : t -> bool
(** [meta l] is [true] if ocamlfind META files mentioned in
    {!Topkg_std_files} should be linted. *)

val opam : t -> bool
(** [opam l] is [true] if the package's OPAM files should be linted. *)

val deps_excluding : t -> string list option
(** [deps_excluding l] is [l]'s dependency linting description. *)

val codec : t Topkg_codec.t
(** [codec] is a codec for linting descriptions. *)

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
