(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type t =
  { custom : (unit -> R.msg result list) option;
    files : string list option;
    meta : bool;
    opam : bool;
    deps_excluding : string list option; }

let v
    ?custom ?(files = Some []) ?(meta = true) ?(opam = true)
    ?(deps_excluding = Some []) ()
  =
  { custom; files; meta; opam; deps_excluding; }

let custom l = l.custom
let files l = l.files
let meta l = l.meta
let opam l = l.opam
let deps_excluding l = l.deps_excluding

(* Codec *)

let codec =
  let string_list_option = Topkg_codec.(option @@ list string) in
  let custom =
    let stub () = invalid_arg "not executable outside package definition" in
    let kind = "custom" in
    let enc = function None -> "\x00" | Some _ -> "\x01" in
    let dec = function
    | "\x00" -> None | "\x01" -> Some stub | s -> Topkg_codec.err ~kind s in
    Topkg_codec.v ~kind ~enc ~dec
  in
  let files = Topkg_codec.(with_kind "files" @@ string_list_option) in
  let meta = Topkg_codec.(with_kind "meta" @@ bool) in
  let opam = Topkg_codec.(with_kind "opam" @@ bool) in
  let deps = Topkg_codec.(with_kind "deps_excluding" @@ string_list_option) in
  let fields =
    (fun l -> l.custom, l.files, l.meta, l.opam, l.deps_excluding),
    (fun (custom, files, meta, opam, deps_excluding) ->
       { custom; files; meta; opam; deps_excluding; })
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
