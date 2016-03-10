(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type file = Topkg_fpath.t * bool
type t =
  { readme : file;
    license : file;
    change_log : file;
    meta : file list;
    opam : file list; }

let v
  ?(readme = ("README.md", true))
  ?(license = ("LICENSE.md", true))
  ?(change_log = ("CHANGES.md", true))
  ?(meta = [("pkg/META", true)])
  ?(opam = [("opam", true)])
  ()
    =
    { readme; license; change_log; meta; opam }

let readme std = std.readme
let license std = std.license
let change_log std = std.change_log
let meta std = std.meta
let opam std = std.opam
let files std =
  fst (std.readme) :: fst (std.license) :: fst (std.change_log) ::
  List.(rev_append (rev_map fst std.meta) (rev (rev_map fst std.opam)))

let install std =
  let add field (file, auto) acc = if auto then field file :: acc else acc in
  add Topkg_install.doc std.readme @@
  add Topkg_install.doc std.license @@
  add Topkg_install.doc std.change_log @@
  List.fold_right (add Topkg_install.lib) std.meta @@
  List.fold_right (add Topkg_install.lib) std.opam @@
  []

let codec =
  let file = Topkg_codec.(pair string bool) in
  let readme = Topkg_codec.(with_kind "readme" @@ file) in
  let license = Topkg_codec.(with_kind "license" @@ file) in
  let change_log = Topkg_codec.(with_kind "change_log" @@ file) in
  let meta = Topkg_codec.(with_kind "meta" @@ list file) in
  let opam = Topkg_codec.(with_kind "opam" @@ list file) in
  let fields =
    (fun s -> s.readme, s.license, s.change_log, s.meta, s.opam),
    (fun (readme, license, change_log, meta, opam) ->
       { readme; license; change_log; meta; opam })
  in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind:"package standard files" fields
                 (t5 readme license change_log meta opam))

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
