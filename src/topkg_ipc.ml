(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type 'a t =
  { cmd : Topkg_cmd.t;
    codec : 'a Topkg_codec.t }

let v cmd codec = { cmd; codec }
let cmd ipc = ipc.cmd
let codec ipc = ipc.codec

let error_args args =
  R.error_msgf "IPC: %a, unknown arguments"
    Topkg_cmd.dump (Topkg_cmd.of_list args)

(* Package description. Description functions raise Invalid_argument
   at the other end. *)

let pkg = v Topkg_cmd.(v "pkg") Topkg_pkg.codec
let answer_pkg p = Topkg_codec.write Topkg_os.File.dash Topkg_pkg.codec p

(* Run custom lint IPC *)

let lint_custom_codec = Topkg_codec.(option @@ list @@ result_error_msg @@ msg)
let lint_custom =
  let cmd = Topkg_cmd.(v "lint" % "custom") in
  v cmd lint_custom_codec

let answer_lint_custom p =
  let custom_run = match (Topkg_pkg.lint_custom p) with
  | None -> None
  | Some custom -> Some (custom ())
  in
  Topkg_codec.write Topkg_os.File.dash lint_custom_codec custom_run

(* Distrib prepare IPC *)

let distrib_prepared_codec =
  Topkg_codec.version 0 @@
  Topkg_codec.(with_kind "prepared" @@ result_error_msg (list fpath))

let distrib_prepare ~dist_build_dir ~name ~version ~opam =
  let cmd =
    Topkg_cmd.(v "distrib" % "prepare" %
               "dist-build-dir" % dist_build_dir % "name" % name %
               "version" % version % "opam" % opam)
  in
  v cmd distrib_prepared_codec

let answer_distrib_prepare p ~dist_build_dir ~name ~version ~opam =
  Topkg_codec.write Topkg_os.File.dash distrib_prepared_codec @@
  Topkg_pkg.distrib_prepare p ~dist_build_dir ~name ~version ~opam

(* IPC answer *)

let _answer p = function
| "pkg" :: [] -> answer_pkg p
| "lint" :: "custom" :: [] -> answer_lint_custom p
| "distrib" :: "prepare" ::
  "dist-build-dir" :: dist_build_dir :: "name" :: name ::
  "version" :: version :: "opam" :: opam :: [] ->
    answer_distrib_prepare p ~dist_build_dir ~name ~version ~opam
| args ->
    error_args args

let answer cmd p = match Topkg_cmd.to_list cmd with
| "ipc" :: _ :: req -> _answer p req >>= fun () -> Ok 0
| args -> error_args args

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
