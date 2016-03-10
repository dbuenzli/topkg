(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type t =
  { custom_fun : (unit -> R.msg result list) option;
    custom : (* result of [custom_fun] *) R.msg result list option option;
    files : string list option;
    meta : bool;
    opam : bool;
    deps_excluding : string list option; }

let v
    ?custom:(custom_fun = None) ?(files = Some []) ?(meta = true) ?(opam = true)
    ?(deps_excluding = Some []) ()
  =
  let custom = match custom_fun with None -> None | Some _ -> Some None in
  { custom_fun; custom; files; meta; opam; deps_excluding; }

let custom l = l.custom
let files l = l.files
let meta l = l.meta
let opam l = l.opam
let deps_excluding l = l.deps_excluding

let run_custom l =
  let custom = match l.custom_fun with
  | None -> None
  | Some tests -> Some (Some (tests ()))
  in
  { l with custom }

(* Codec *)

let codec =
  let custom = Topkg_codec.(option @@ option @@ list @@ result_error_msg msg) in
  let string_list_option = Topkg_codec.(option @@ list string) in
  let custom = Topkg_codec.(with_kind "custom" @@ custom) in
  let files = Topkg_codec.(with_kind "files" @@ string_list_option) in
  let meta = Topkg_codec.(with_kind "meta" @@ bool) in
  let opam = Topkg_codec.(with_kind "opam" @@ bool) in
  let deps = Topkg_codec.(with_kind "deps_excluding" @@ string_list_option) in
  let fields =
    (fun t -> t.custom, t.files, t.meta, t.opam, t.deps_excluding),
    (fun (custom, files, meta, opam, deps_excluding) ->
      { custom; custom_fun = None; files; meta; opam; deps_excluding; })
  in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind:"package lint" fields
                 (t5 custom files meta opam deps))

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
