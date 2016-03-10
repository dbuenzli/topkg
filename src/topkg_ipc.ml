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

(* Delegate IPC *)

let delegate_codec =
  Topkg_codec.version 0 @@
  Topkg_codec.(with_kind "delegate"
                 (pair (option string) Topkg_std_files.codec))

let delegate =
  let cmd = Topkg_cmd.(v "delegate") in
  v cmd delegate_codec

let answer_delegate p =
  let delegate = Topkg_pkg.delegate p in
  let std = Topkg_pkg.std_files p in
  Topkg_codec.write Topkg_os.File.dash delegate_codec (delegate, std)

(* Standard files IPC *)

let std_files =
  let cmd = Topkg_cmd.(v "std-files") in
  v cmd Topkg_std_files.codec

let answer_std_files p =
  let std = Topkg_pkg.std_files p in
  Topkg_codec.write Topkg_os.File.dash Topkg_std_files.codec std

(* Lint IPC *)

let lint_codec = Topkg_codec.(pair Topkg_std_files.codec Topkg_lint.codec)
let lint ~custom =
  let cmd = Topkg_cmd.(v "lint" %% on custom (v "run-custom")) in
  v cmd lint_codec

let answer_lint p ~custom =
  let std_files = Topkg_pkg.std_files p in
  let lint = Topkg_pkg.lint p in
  let lint = if custom then Topkg_lint.run_custom lint else lint in
  Topkg_codec.write Topkg_os.File.dash lint_codec (std_files, lint)

(* Distrib commit-ish IPC *)

let distrib_commit_ish_codec =
  Topkg_codec.version 0 @@
  Topkg_codec.(with_kind "commit-ish" (result_error_msg string))

let distrib_commit_ish =
  let cmd = Topkg_cmd.(v "distrib" % "commit-ish") in
  v cmd distrib_commit_ish_codec

let answer_distrib_commit_ish p =
  let commit_ish = Topkg_distrib.commit_ish (Topkg_pkg.distrib p) in
  Topkg_codec.write Topkg_os.File.dash distrib_commit_ish_codec commit_ish

(* Distrib determine IPC *)

let distrib_determine_codec =
  Topkg_codec.version 0 @@
  Topkg_codec.(with_kind "determine" @@
               result_error_msg (t4 string string string string))

let distrib_determine ~build_dir ~name ~commit_ish ~version =
  (* I'm sure these empty strings are a bad idea. *)
  let build_dir = match build_dir with None -> "" | Some b -> b in
  let name = match name with None -> "" | Some n -> n in
  let commit_ish = match commit_ish with None -> "" | Some c -> c in
  let version = match version with None -> "" | Some v -> v in
  let cmd = Topkg_cmd.(v "distrib" % "determine" %
                       "build-dir" % build_dir % "name" % name %
                       "commit-ish" % commit_ish % "version" % version)
  in
  v cmd distrib_determine_codec

let answer_distrib_determine p ~build_dir ~name ~commit_ish ~version =
  let bdir = match build_dir with
  | "" (* None *) -> Topkg_build.dir (Topkg_pkg.build p)
  | b -> b
  in
  let name = match name with
  | "" (* None *) -> Topkg_pkg.name p
  | n -> n
  in
  let commit_ish = match commit_ish with
  | "" (* None *) -> Topkg_distrib.commit_ish (Topkg_pkg.distrib p)
  | c -> Ok c
  in
  let ret =
    commit_ish >>= fun commit_ish ->
    let version = match version with
    | "" (* None *) -> Topkg_distrib.version (Topkg_pkg.distrib p) ~commit_ish
    | v -> Ok v
    in
    version >>= fun version -> Ok (bdir, name, commit_ish, version)
  in
  Topkg_codec.write Topkg_os.File.dash distrib_determine_codec ret

(* Distrib prepare IPC *)

let prepared_codec =
  Topkg_codec.version 0 @@
  Topkg_codec.(with_kind "prepared" @@ result_error_msg (list string))

let distrib_prepare ~name ~version =
  let cmd =
    Topkg_cmd.(v "distrib" % "prepare" % "name" % name % "version" % version)
  in
  v cmd prepared_codec

let answer_prepare p ~name ~version =
  let d = Topkg_pkg.distrib p in
  let prepared = Topkg_distrib.run_prepare ~name ~version d in
  Topkg_codec.write Topkg_os.File.dash prepared_codec prepared

(* IPC answer *)

let _answer p = function
| "delegate" :: [] ->
    answer_delegate p
| "std-files" :: [] ->
    answer_std_files p
| "lint" :: [] ->
    answer_lint p ~custom:false
| "lint" :: "run-custom" :: [] ->
    answer_lint p ~custom:true
| "distrib" :: "commit-ish" :: [] ->
    answer_distrib_commit_ish p
| "distrib" :: "determine" :: "build-dir" :: build_dir ::
  "name" :: name :: "commit-ish" :: commit_ish :: "version" :: version :: [] ->
    answer_distrib_determine p ~build_dir ~name ~commit_ish ~version
| "distrib" :: "prepare" :: "name" :: name :: "version" :: version :: [] ->
    answer_prepare p ~name ~version
| args ->
    error_args args

let answer cmd p = match Topkg_cmd.to_list cmd with
| "ipc" :: _ :: req -> _answer p req >>= fun () -> Ok 0
| args -> error_args args >>= fun () -> Ok 0

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
