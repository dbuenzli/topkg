(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let find_repo () =
  Topkg.Vcs.get ()
  >>= fun r -> Topkg.Vcs.is_dirty r
  >>= fun dirty ->
  if dirty then Logs.warn (fun m -> m "The source repository is dirty");
  Ok r

let lint_distrib ~dist_pkg_file ~dir =
  let skip _ = false in
  Logs.app (fun m -> m "Linting distrib in %a" Fpath.pp dir);
  Topkg_care.Lint.distrib ~ignore_pkg:false ~pkg_file:dist_pkg_file ~dir ~skip

let build_distrib ~dist_pkg_file ~dir =
  let pp_status ppf = function
  | `Ok -> Fmt.(brackets @@ styled_unit `Green " OK ") ppf ()
  | `Fail -> Fmt.(brackets @@ styled_unit `Red "FAIL") ppf ()
  in
  Logs.app (fun m -> m "Building distrib in %a" Fpath.pp dir);
  let args = [ "installer"; "false"; "vcs"; "false" ] in
  let out = OS.Cmd.out_string in
  Topkg_care.Build.pkg ~pkg_file:dist_pkg_file ~dir ~args ~out >>= function
  | (_, (_, `Exited 0)) ->
      Logs.app (fun m -> m "%a distrib builds" pp_status `Ok); Ok 0
  | (stdout, _) ->
      Logs.app (fun m -> m "%s@\n%a distrib builds" stdout pp_status `Fail);
      Ok 1

let check_archive ar ~dist_pkg_file ~skip_lint ~skip_build =
  Topkg_care.Distrib.unarchive ~clean:true ar
  >>= fun dir -> (if skip_lint then Ok 0 else lint_distrib ~dist_pkg_file ~dir)
  >>= fun c0 -> (if skip_build then Ok 0 else build_distrib ~dist_pkg_file ~dir)
  >>= fun c1 -> match c0 + c1 with
  | 0 -> OS.Dir.delete ~recurse:true dir >>= fun () -> Ok 0
  | n -> Ok 1

let log_det d =
  Logs.app @@ fun m ->
    m "@[<v>@[Distribution for %a@ %a@]@, commit %a@]@]"
      Fmt.(styled `Bold string) (Topkg_care.Distrib.name d)
      Fmt.(styled `Cyan string) (Topkg_care.Distrib.version d)
      Fmt.(styled `Yellow string) (Topkg_care.Distrib.commit_ish d)

let distrib () pkg_file det dist_pkg_file keep_dir skip_lint skip_build =
  begin
    Topkg_care.Distrib.ensure_bzip2 ()
    >>= fun () -> det ~pkg_file
    >>= fun det -> log_det det; find_repo ()
    >>= fun repo -> Topkg_care.Distrib.clone_repo det repo
    >>= fun dir -> Topkg_care.Distrib.prepare_repo det ~dir ~dist_pkg_file
    >>= fun (mtime, exclude_paths) ->
    Topkg_care.Distrib.archive det ~keep_dir ~dir ~exclude_paths ~mtime
    >>= fun archive ->
    Logs.app (fun m -> m "Wrote archive %a"
                 Fmt.(styled `Bold Fpath.pp) archive);
    check_archive archive ~dist_pkg_file ~skip_lint ~skip_build
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let keep_build_dir =
  let doc = "Keep the distribution build directory after successful archival."
  in
  Arg.(value & flag & info ["keep-build-dir"] ~doc)

let skip_lint =
  let doc = "Do not lint the archive distribution." in
  Arg.(value & flag & info ["skip-lint"] ~doc)

let skip_build =
  let doc = "Do not try to build the distribution from the archive." in
  Arg.(value & flag & info ["skip-build"] ~doc)

let doc = "create a package distribution archive"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) command creates a package distribution
        archive in the build directory of the package.  The generated
        archive should be bit-wise reproducible. There are however a few
        caveats, see the section about this further down.";
    `P "Once the archive is created it is unpacked in the build directory,
        linted and the package is built using the package description
        contained in the archive. The build will use the default package
        configuration so it may fail in the current environment
        without this necessarily implying an actual problem with the
        distribution; one should still worry about it though.
        These checks can be prevented by using the $(b,--skip-lint) and
        $(b,--skip-build) options.";
    `S "OPTIONS";
  ] @ Cli.common_opts_man @ [
    `S "PACKAGE DESCRIPTION FILE CIRCUS";
    `P "The current package description, the one specified with the
        $(b,--pkg-file) option, is only used to determine
        the build directory, the package name, the commit-ish and version
        string to base the distribution on. All these tidbits
        can be overriden on the command line if needed.";
    `P "After this, the package description file present in the checkout of
        the commit-ish and specified with the $(b,--dist-pkg-file) option
        is used to prepare the distribution. It is this package description
        that will define the watermarks, massaging hook and paths to exclude
        from the archive.";
    `P "More detailed information about the archive creation process and its
        customization can be found in topkg's API documentation.";
    `S "REPRODUCIBLE DISTRIBUTION ARCHIVES";
    `P "Given the package name, the commit-ish and the version string,
        the $(b,$(tname)) command should always generate the same archive.";
    `P "More precisely, files are added to the archive using a well defined
        order on path names. Their file permissions are those determined by the
        commit-ish repository checkout and their modification times are set
        to the commit date of the commit-ish (note that git-log(1) shows
        the author date which may not coincide). No other file metadata is
        recorded.";
    `P "This should ensure that the resulting archive is bit-wise
        identical regardless of the context in which it is
        created. However this may fail for one or more of the
        following reasons:";
    `I ("Non-reproducible distribution massage", "The package
         distribution massaging hook relies on external factors
         that are not captured by the source repository checkout.
         For example external data files, environment variables, etc.");
    `I ("File paths with non US-ASCII characters",
        "If these paths are encoded in UTF-8, different file systems
         may return the paths with different Unicode normalization
         forms which could yield different byte serializations in the
         archive (note that this could be lifted at the cost of a
         dependency on Uunf).");
    `I ("The bzip2 utility", "The archive is compressed using the bzip2 utility.
         Reproducibility relies on bzip2 to be a reproducible function
         across platforms.");
    `I ("Topkg changes", "Topkg could change its distribution procedure in
         the future, for example to correct bugs.");
    `S "ENVIRONMENT VARIABLES";
    `I ("$(i,TOPKG_BZIP2)", "The bzip2 tool to use to compress the
        archive. Gets the archive on stdin and must output the result on
        standard out.");
    `I ("$(i,TOPKG_TAR)", "The tar tool to use to unarchive a tbz
        archive (archive creation itself is handled by topkg).");
  ] @ Cli.see_also ~cmds:["topkg-lint"]

let cmd =
  let info = Term.info "distrib" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure distrib $ Cli.setup $ Cli.pkg_file $
                Cli.distrib_determine $ Cli.dist_pkg_file $
                keep_build_dir $ skip_lint $ skip_build)
  in
  (t, info)

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
