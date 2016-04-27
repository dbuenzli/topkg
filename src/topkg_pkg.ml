(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type t =
  { name : string;
    delegate : Topkg_cmd.t option;
    std_files : Topkg_std_files.t;
    lint : Topkg_lint.t;
    distrib : Topkg_distrib.t;
    build : Topkg_build.t;
    install : Topkg_install.t; }

let v
    ?delegate
    ?(std_files = Topkg_std_files.v ())
    ?(lint = Topkg_lint.v ())
    ?(distrib = Topkg_distrib.v ())
    ?(build = Topkg_build.v ())
    name installs
  =
  let std = Topkg_std_files.install std_files in
  let installs = List.rev_append std installs in
  let install = List.sort compare (List.flatten installs) in
  { name; delegate; std_files; lint; distrib; build; install }

let empty = v "" []

let name p = p.name
let delegate p = p.delegate
let std_files p = p.std_files
let distrib p = p.distrib
let build p = p.build
let install p = p.install

let build_dir p = Topkg_build.dir p.build
let readme p = fst (Topkg_std_files.readme p.std_files)
let change_log p = fst (Topkg_std_files.change_log p.std_files)
let license p = fst (Topkg_std_files.license p.std_files)
let opam ~name p =
  let has_name o = Topkg_fpath.(basename @@ rem_ext @@ fst o) = name in
  let opams = Topkg_std_files.opam p.std_files in
  match try Some (List.find has_name opams) with Not_found -> None with
  | Some opam -> fst opam
  | None ->
      Topkg_log.warn
        (fun m -> m "No opam file for %s, using 'opam'" p.name);
      "opam"

let codec =
  let name = Topkg_codec.(with_kind "name" @@ string) in
  let delegate = Topkg_codec.(with_kind "delegate" @@ option cmd) in
  let std = Topkg_codec.(with_kind "std_files" @@ Topkg_std_files.codec) in
  let lint = Topkg_codec.(with_kind "lint" @@ Topkg_lint.codec) in
  let distrib = Topkg_codec.(with_kind "distrib" @@ Topkg_distrib.codec) in
  let build = Topkg_codec.(with_kind "build" @@ Topkg_build.codec) in
  let install = Topkg_codec.(with_kind "install" @@ Topkg_install.codec) in
  let fields =
    (fun p -> (p.name, p.delegate, p.std_files, p.lint, p.distrib),
              (p.build, p.install)),
    (fun ((name, delegate, std_files, lint, distrib), (build, install)) ->
       { name; delegate; std_files; lint; distrib; build; install })
  in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind: "package" fields
                 (pair
                    (t5 name delegate std lint distrib)
                    (pair build install)))

(* Distrib *)

let distrib_prepare p ~dist_build_dir ~name ~version ~opam =
  let d = distrib p in
  let ws = Topkg_distrib.watermarks d in
  let ws_defs = Topkg_distrib.define_watermarks ws ~name ~version ~opam in
  Topkg_os.Dir.set_current dist_build_dir
  >>= fun () -> Topkg_distrib.files_to_watermark d ()
  >>= fun files -> Topkg_distrib.watermark_files ws_defs files
  >>= fun () -> Topkg_distrib.massage d ()
  >>= fun () -> Topkg_distrib.exclude_paths d ()

let distrib_prepare_pin p =
  let name = name p in
  let opam = opam ~name p in
  let d = distrib p in
  let ws = Topkg_distrib.watermarks d in
  Topkg_vcs.get ()
  >>= fun repo -> Topkg_vcs.describe ~dirty:true repo
  >>= fun version -> Ok(Topkg_distrib.define_watermarks ws ~name ~version ~opam)
  >>= fun ws_defs -> Topkg_distrib.files_to_watermark d ()
  >>= fun files -> Topkg_distrib.watermark_files ws_defs files
  >>= fun () -> Topkg_distrib.massage d ()

let distrib_uri p = Topkg_distrib.uri p.distrib

(* Build *)

let run_build p =
  let header = p.name in
  let conf = Topkg_conf.OCaml.v `Host_os in
  let targets, install =
    Topkg_install.to_instructions ~header ~bdir:(Topkg_build.dir p.build)
      conf p.install
  in
  let prepare =
    match Topkg_build.prepare_on_pin p.build && Topkg_conf.build = `Pin with
    | false -> Ok ()
    | true ->
        (distrib_prepare_pin p)
        |> R.reword_error_msg ~replace:true
          (fun e -> R.msgf "Pin distribution preparation failed: %s" e)
  in
  prepare
  >>= fun () ->
  (Topkg_build.pre p.build Topkg_conf.build)
  |> R.reword_error_msg ~replace:true
    (fun e -> R.msgf "Pre-build hook failed: %s" e)
  >>= fun () ->
  let build_cmd =
    (Topkg_build.cmd p.build) Topkg_conf.build `Host_os
      (Topkg_build.dir p.build)
  in
  Topkg_os.Cmd.run Topkg_cmd.(build_cmd %% of_list targets)
  >>= fun () ->
  let install_file = p.name ^ ".install" in
  let install = Topkg_opam.Install.to_string install in
  Topkg_os.File.write install_file install
  >>= fun () ->
  (Topkg_build.post p.build Topkg_conf.build)
  |> R.reword_error_msg ~replace:true
    (fun e -> R.msgf "Post-build hook failed: %s" e)
  >>= fun () -> Ok 0

(* Lint *)

let lint_custom p = Topkg_lint.custom p.lint

let lint_files p = match Topkg_lint.files p.lint with
| None (* disabled *) -> None
| Some fs -> Some (List.rev_append (Topkg_std_files.files p.std_files) fs)

let lint_metas p =
  if not (Topkg_lint.meta p.lint) then None else
  Some (List.map fst (Topkg_std_files.meta p.std_files))

let lint_opams p =
  if not (Topkg_lint.opam p.lint) then None else
  let add_file f = fst f, Topkg_lint.deps_excluding p.lint (* FIXME *) in
  Some (List.map add_file (Topkg_std_files.opam p.std_files))

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
