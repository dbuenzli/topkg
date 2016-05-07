(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type std_file = Topkg_fpath.t * bool
let std_file ?(install = true) file = file, install

type meta_file = std_file * bool
let meta_file ?(lint = true) ?install file =
  std_file ?install file, lint

type opam_file = std_file * bool * string list option
let opam_file ?(lint = true) ?(lint_deps_excluding = Some []) ?install file =
  std_file ?install file, lint, lint_deps_excluding

let install_std_files readme license change_log metas opams =
  let add field (p, install) acc = if install then field p :: acc else acc in
  add Topkg_install.doc readme @@
  add Topkg_install.doc license @@
  add Topkg_install.doc change_log @@
  List.fold_right (fun (m, _) -> add Topkg_install.lib m) metas @@
  List.fold_right (fun (o, _, _) -> add Topkg_install.lib o) opams @@
  []

type t =
  { name : string;
    delegate : Topkg_cmd.t option;
    readme : std_file;
    license : std_file;
    change_log : std_file;
    metas : meta_file list;
    opams : opam_file list;
    lint_files : Topkg_fpath.t list option;
    lint_custom :(unit -> R.msg result list) option;
    distrib : Topkg_distrib.t;
    build : Topkg_build.t;
    install : Topkg_install.t; }

let v
    ?delegate
    ?(readme = ("README.md", true))
    ?(license = ("LICENSE.md", true))
    ?(change_log = ("CHANGES.md", true))
    ?(metas = [meta_file "pkg/META"])
    ?(opams = [opam_file "opam"])
    ?(lint_files = Some [])
    ?lint_custom
    ?(distrib = Topkg_distrib.v ())
    ?(build = Topkg_build.v ())
    name installs
  =
  let std_files = install_std_files readme license change_log metas opams in
  let installs = List.rev_append std_files installs in
  let install = List.sort compare (List.flatten installs) in
  { name; delegate; readme; license; change_log; metas; opams; lint_files;
    lint_custom; distrib; build; install }

let empty = v "" []

let name p = p.name
let delegate p = p.delegate
let readme p = fst p.readme
let change_log p = fst p.change_log
let license p = fst p.license
let distrib p = p.distrib
let build p = p.build
let install p = p.install

let std_files p =
  fst (p.readme) :: fst (p.license) :: fst (p.change_log) ::
  List.(rev_append (rev_map (fun (m, _) -> fst m) p.metas)
    (rev (rev_map (fun (o, _, _) -> fst o) p.opams)))

let build_dir p = Topkg_build.dir p.build
let opam ~name p =
  let has_name (o, _, _) = Topkg_fpath.(basename @@ rem_ext @@ fst o) = name in
  let opams = p.opams in
  match try Some (List.find has_name opams) with Not_found -> None with
  | Some (opam, _, _) -> fst opam
  | None ->
      Topkg_log.warn
        (fun m -> m "No opam file for %s, using 'opam'" p.name);
      "opam"

let codec =
  let string_list_option = Topkg_codec.(option @@ list string) in
  let std_file = Topkg_codec.(pair string bool) in
  let meta_file = Topkg_codec.(pair std_file bool) in
  let opam_file = Topkg_codec.(t3 std_file bool (string_list_option)) in
  (* fields *)
  let name = Topkg_codec.(with_kind "name" @@ string) in
  let delegate = Topkg_codec.(with_kind "delegate" @@ option cmd) in
  let readme = Topkg_codec.(with_kind "readme" @@ std_file) in
  let license = Topkg_codec.(with_kind "license" @@ std_file) in
  let change_log = Topkg_codec.(with_kind "change_log" @@ std_file) in
  let metas = Topkg_codec.(with_kind "metas" @@ list meta_file) in
  let opams = Topkg_codec.(with_kind "opams" @@ list opam_file) in
  let lint_files = Topkg_codec.(with_kind "lint_files" @@ string_list_option) in
  let lint_custom =
    let stub () = invalid_arg "not executable outside package definition" in
    let kind = "lint_custom" in
    let enc = function None -> "\x00" | Some _ -> "\x01" in
    let dec = function
    | "\x00" -> None | "\x01" -> Some stub | s -> Topkg_codec.err ~kind s in
    Topkg_codec.v ~kind ~enc ~dec
  in
  let distrib = Topkg_codec.(with_kind "distrib" @@ Topkg_distrib.codec) in
  let build = Topkg_codec.(with_kind "build" @@ Topkg_build.codec) in
  let install = Topkg_codec.(with_kind "install" @@ Topkg_install.codec) in
  let fields =
    (fun p -> (p.name, p.delegate, p.readme, p.license, p.change_log),
              (p.metas, p.opams, p.lint_files, p.lint_custom, p.distrib),
              (p.build, p.install)),
    (fun ((name, delegate, readme, license, change_log),
          (metas, opams, lint_files, lint_custom, distrib),
          (build, install)) ->
       { name; delegate; readme; license; change_log;
         metas; opams; lint_files; lint_custom; distrib;
         build; install })
  in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind: "package" fields
                 (t3
                    (t5 name delegate readme license change_log)
                    (t5 metas opams lint_files lint_custom distrib)
                    (t2 build install)))
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

let lint_custom p = p.lint_custom

let lint_files p = match p.lint_files with
| None (* disabled *) -> None
| Some fs -> Some (List.rev_append (std_files p) fs)

let lint_metas p =
  List.map (fun ((p, _), lint) -> (p, lint)) p.metas

let lint_opams p =
  List.map (fun ((p, _), lint, lint_deps) -> (p, lint, lint_deps)) p.opams

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
