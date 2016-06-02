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

let std_files_installs readmes licenses change_logs metas opams =
  let add field (p, install) acc = if install then field p :: acc else acc in
  List.fold_right (add Topkg_install.doc) readmes @@
  List.fold_right (add Topkg_install.doc) licenses @@
  List.fold_right (add Topkg_install.doc) change_logs @@
  List.fold_right (fun (m, _) -> add Topkg_install.lib m) metas @@
  List.fold_right (fun (o, _, _) -> add Topkg_install.lib o) opams @@
  []

type t =
  { name : string;
    delegate : Topkg_cmd.t option;
    readmes : std_file list;
    licenses : std_file list;
    change_logs : std_file list;
    metas : meta_file list;
    opams : opam_file list;
    lint_files : Topkg_fpath.t list option;
    lint_custom :(unit -> R.msg result list) option;
    distrib : Topkg_distrib.t;
    build : Topkg_build.t;
    installs : Topkg_conf.t -> Topkg_install.t list result; }

let v
    ?delegate
    ?(readmes = [("README.md", true)])
    ?(licenses = [("LICENSE.md", true)])
    ?(change_logs = [("CHANGES.md", true)])
    ?(metas = [meta_file "pkg/META"])
    ?(opams = [opam_file "opam"])
    ?(lint_files = Some [])
    ?lint_custom
    ?(distrib = Topkg_distrib.v ())
    ?(build = Topkg_build.v ())
    name installs
  =
  { name; delegate; readmes; licenses; change_logs; metas; opams; lint_files;
    lint_custom; distrib; build; installs }

let empty = v "" (fun _ -> Ok [])
let with_name_and_build_dir p name build_dir =
  let build = Topkg_build.with_dir p.build build_dir in
  { p with name; build }

let name p = p.name
let delegate p = p.delegate
let readmes p = List.map fst p.readmes
let change_logs p = List.map fst p.change_logs
let licenses p = List.map fst p.licenses
let distrib p = p.distrib
let build p = p.build
let install p c =
  p.installs c >>= fun installs ->
  let std_files =
    std_files_installs p.readmes p.licenses p.change_logs p.metas p.opams
  in
  Ok (List.rev_append std_files installs)

let std_files p =
  List.map fst p.readmes @ List.map fst p.licenses @
  List.map fst p.change_logs @
  List.(rev_append (rev_map (fun (m, _) -> fst m) p.metas)
    (rev (rev_map (fun (o, _, _) -> fst o) p.opams)))

let build_dir p = Topkg_build.dir p.build
let opam ~name p =
  let has_name (o, _, _) = Topkg_fpath.(basename @@ rem_ext @@ fst o) = name in
  let opams = p.opams in
  match try Some (List.find has_name opams) with Not_found -> None with
  | Some (opam, _, _) -> fst opam
  | None ->
      if name <> p.name then
        Topkg_log.warn
          (fun m -> m "No opam file for %s, using 'opam'" p.name);
      "opam"

let codec =
  let stub _ = invalid_arg "not executable outside package definition" in
  let string_list_option = Topkg_codec.(option @@ list string) in
  let std_file = Topkg_codec.(pair string bool) in
  let meta_file = Topkg_codec.(pair std_file bool) in
  let opam_file = Topkg_codec.(t3 std_file bool (string_list_option)) in
  (* fields *)
  let name = Topkg_codec.(with_kind "name" @@ string) in
  let delegate = Topkg_codec.(with_kind "delegate" @@ option cmd) in
  let readmes = Topkg_codec.(with_kind "readmes" @@ list std_file) in
  let licenses = Topkg_codec.(with_kind "licenses" @@ list std_file) in
  let change_logs = Topkg_codec.(with_kind "change_logs" @@ list std_file) in
  let metas = Topkg_codec.(with_kind "metas" @@ list meta_file) in
  let opams = Topkg_codec.(with_kind "opams" @@ list opam_file) in
  let lint_files = Topkg_codec.(with_kind "lint_files" @@ string_list_option) in
  let lint_custom =
    let kind = "lint_custom" in
    let enc = function None -> "\x00" | Some _ -> "\x01" in
    let dec = function
    | "\x00" -> None | "\x01" -> Some stub | s -> Topkg_codec.err ~kind s in
    Topkg_codec.v ~kind ~enc ~dec
  in
  let distrib = Topkg_codec.(with_kind "distrib" @@ Topkg_distrib.codec) in
  let build = Topkg_codec.(with_kind "build" @@ Topkg_build.codec) in
  let fields =
    (fun p -> (p.name, p.delegate, p.readmes, p.licenses, p.change_logs),
              (p.metas, p.opams, p.lint_files, p.lint_custom, p.distrib),
              (p.build)),
    (fun ((name, delegate, readmes, licenses, change_logs),
          (metas, opams, lint_files, lint_custom, distrib),
          (build)) ->
       { name; delegate; readmes; licenses; change_logs;
         metas; opams; lint_files; lint_custom; distrib;
         build; installs = stub })
  in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind: "package" fields
                 (t3
                    (t5 name delegate readmes licenses change_logs)
                    (t5 metas opams lint_files lint_custom distrib)
                    build))
(* Distrib *)

let distrib_uri p = Topkg_distrib.uri p.distrib
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

(* Build *)

let write_opam_install_file p install =
  let file = p.name ^ ".install" in
  let install = Topkg_opam.Install.to_string install in
  Topkg_os.File.write file install
  >>| fun () -> Topkg_log.app (fun m -> m "Wrote OPAM install file %s" file)

let run_build_hook kind hook c =
  (hook c)
  |> R.reword_error_msg ~replace:true
    (fun e -> R.msgf "%s-build hook failed: %s" kind e)

let build p ~dry_run c os =
  install p c
  >>= fun is -> Ok (Topkg_install.to_build ~header:p.name c os is)
  >>= fun (targets, install) -> match dry_run with
  | true -> write_opam_install_file p install >>= fun () -> Ok 0
  | false ->
      let build_cmd = (Topkg_build.cmd p.build) c os in
      let is_pin = Topkg_conf.build_context c = `Pin in
      let prepare = match Topkg_build.prepare_on_pin p.build && is_pin with
      | false -> Ok ()
      | true ->
          (distrib_prepare_pin p)
          |> R.reword_error_msg ~replace:true
            (fun e -> R.msgf "Pin distribution preparation failed: %s" e)
      in
      prepare
      >>= fun () -> run_build_hook "Pre" (Topkg_build.pre p.build) c
      >>= fun () -> Topkg_os.Cmd.run Topkg_cmd.(build_cmd %% of_list targets)
      >>= fun () -> write_opam_install_file p install
      >>= fun () -> run_build_hook "Post" (Topkg_build.post p.build) c
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
