(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let find_descr std_files opam_file descr_file = match descr_file with
| Some d -> OS.File.read d >>= fun c -> Topkg_care.Opam.Descr.of_string c
| None ->
    let descr_file = Fpath.(parent opam_file / "descr") in
    OS.File.exists descr_file >>= function
    | true ->
        Logs.info (fun m -> m "Found OPAM descr file %a" Fpath.pp descr_file);
        OS.File.read descr_file >>= fun c -> Topkg_care.Opam.Descr.of_string c
    | false ->
        Fpath.of_string (fst (Topkg.Private.Std_files.readme std_files))
        >>= fun readme ->
        Logs.info (fun m -> m "Extracting OPAM descr from %a" Fpath.pp readme);
        OS.File.read readme
        >>= fun c -> Topkg_care.Opam.Descr.of_readme_md c

let descr ~pkg_file opam_file descr_file =
  Topkg_care.Std_files.of_pkg_file ~pkg_file
  >>= fun std_files -> Topkg_care.Std_files.find_opam_file std_files
  >>= fun opam_file -> find_descr std_files opam_file descr_file
  >>= fun descr ->
  Logs.app (fun m -> m "%s" (Topkg_care.Opam.Descr.to_string descr));
  Ok 0

let pkg_dir det opam_pkg_dir = match opam_pkg_dir with
| Some d -> d
| None ->
      Fpath.(Topkg_care.Distrib.(build_dir det // rootname ~opam:true det))

let pkg_url dist_file uri =
  try
    let checksum = Digest.(to_hex @@ file (Fpath.to_string dist_file)) in
    Ok (strf "archive: \"%s\"\nchecksum: \"%s\"" uri checksum)
  with Failure msg | Sys_error msg -> R.error_msg msg



let pkg ~pkg_file det opam_pkg_dir opam_file descr_file dist_file =
  failwith "TODO"
(*
  det ~pkg_file
  >>= fun det -> Ok (pkg_dir det opam_pkg_dir)
  >>= fun dir -> Topkg_care.Std_files.of_pkg_files ~pkg_file
  >>= fun std_files -> Cli.find_opam_file std_files opam_file
  >>= fun opam_file -> OS.File.read opam_file
  >>= fun opam -> find_descr std_files opam_file descr_file
  >>= fun descr -> Cli.find_dist_file det dist_file
  >>= fun dist_file -> pkg_url dist_file "TODO"
  >>= fun url -> OS.Dir.create dir
  >>= fun _ ->
  OS.File.write Fpath.(dir / "descr") (Topkg_care.Opam.Descr.to_string descr)
  >>= fun () -> OS.File.write Fpath.(dir / "opam") opam
  >>= fun () -> OS.File.write Fpath.(dir / "url") url
  >>= fun () ->
  Logs.app (fun m -> m "Wrote OPAM package %a" Fmt.(styled `Bold Fpath.pp) dir);
  Ok 0
*)

let ensure_opam_publish () =
  let opam_publish =
    OS.Env.(value "TOPKG_OPAM_PUBLISH" cmd ~absent:(Cmd.v "opam-publish"))
  in
  OS.Cmd.must_exist opam_publish

let submit ~pkg_file det opam_pkg_dir =
  ensure_opam_publish ()
  >>= fun opam_publish -> det ~pkg_file
  >>= fun det -> Ok (pkg_dir det opam_pkg_dir)
  >>= fun dir -> OS.Dir.exists dir
  >>= function
  | false ->
      Logs.err (fun m -> m "Package@ %a@ does@ not@ exist. Did@ you@ forget@ \
                            to@ invoke $(topkg opam pkg) ?" Fpath.pp dir);
      Ok 1
  | true ->
      Logs.app (fun m -> m "Submitting %a" Fmt.(styled `Bold Fpath.pp) dir);
      OS.Cmd.run Cmd.(opam_publish % "submit" % p dir) >>= fun () -> Ok 0

let field ~pkg_file opam_file field = match field with
| None -> Logs.err (fun m -> m "Missing FIELD positional argument"); Ok 1
| Some field ->
    failwith "TODO"
(*
    Topkg_care.Std_files.of_pkg_file ~pkg_file
    >>= fun std_files -> Cli.find_opam_file std_files opam_file
    >>= fun opam_file -> Topkg_care.Opam.File.fields opam_file
    >>= fun fields -> match String.Map.find field fields with
    | Some v -> Logs.app (fun m -> m "%s" (String.concat ~sep:" " v)); Ok 0
    | None ->
        Logs.err (fun m ->
            m "%a: field %s is undefined" Fpath.pp opam_file field); Ok 1
*)

let opam () pkg_file cmd det opam_pkg_dir opam_file descr_file dist_file fname =
  begin match cmd with
  | `Descr -> descr ~pkg_file opam_file descr_file
  | `Pkg -> pkg ~pkg_file det opam_pkg_dir opam_file descr_file dist_file
  | `Submit -> submit ~pkg_file det opam_pkg_dir
  | `Field -> field ~pkg_file opam_file fname
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let cmd =
  let cmd = [ "descr", `Descr; "pkg", `Pkg; "submit", `Submit;
              "field", `Field ] in
  let doc = strf "The command to perform. $(docv) must be one of %s."
      (Arg.doc_alts_enum cmd)
  in
  let cmd = Arg.enum cmd in
  Arg.(required & pos 0 (some cmd) None & info [] ~doc ~docv:"COMMAND")

let field =
  let doc = "the field to output ($(b,field) sub-command)" in
  Arg.(value & pos 1 (some string) None & info [] ~doc ~docv:"FIELD")

let opam_pkg_dir =
  let doc = "Directory to use to write the OPAM package. If absent the
             sub-directory $NAME.$VERSION of the build directory (see
             option $(b,--build-dir) is used with $NAME and $VERSION
             respectively determined as mentioned in $(b,--pkg-name) and
             $(b,pkg-version)."
  in
  let docv = "DIR" in
  Arg.(value & opt (some Cli.path_arg) None & info ["opam-pkg-dir"] ~doc ~docv)

let descr_file =
  let doc = "OPAM descr file to use. If absent uses an existing
             'descr' file in the directory of the opam file (see
             $(b,--opam-file)).  If there is no such file a descr is
             extracted from the README as follows: the package
             synopsis is extracted from the README's first line by
             parsing the pattern '$(NAME) $(SEP) $(SYNOPSIS)', the
             next line is skipped, all the lines until the next one
             that start with a '#' (markdown section) are the long
             description. A few lines are filtered out: lines that
             start with either 'Home page:', 'Contact:' or
             '%%VERSION%'."
  in
  let docv = "FILE" in
  Arg.(value & opt (some Cli.path_arg) None & info ["descr-file"] ~doc ~docv)

let doc = "interaction with OPAM and the OCaml OPAM repository"
let man =
  [
    `S "SYNOPSIS";
    `P "$(b,$(mname)) $(b,$(tname)) [$(i,OPTION)]... $(i,COMMAND)";
    `S "DESCRIPTION";
    `P "The $(b,$(tname)) command provides a few sub-commands to interact with
        OPAM and the OCaml OPAM repository.";
    `S "SUB-COMMANDS";
    `I ("$(b,descr)",
        "extract and print an OPAM descr file. This is used by the $(b,pkg)
         sub-command. See the $(b,--descr-file) option for details.");
    `I ("$(b,pkg)",
        "create an OPAM package description for a distribution.
         The sub-command needs a distribution archive to operate, see
         topkg-distrib(1) or the $(b,--dist-file) option.");
    `I ("$(b,submit)",
        "submits a package created with the sub-command $(b,pkg) the OCaml
         OPAM repository. Requires the $(b,opam-publish) tool to be
         installed.");
    `I ("$(b,field) $(i,FIELD)",
        "outputs the field $(i,FIELD) of the opam file specified by
         $(b,--opam-file)$.");
    `S "ARGUMENTS";
    `S "OPTIONS";
  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
    `I ("$(i,TOPKG_OPAM_PUBLISH)", "The opam-publish tool to use to submit
         packages.")
  ] @ Cli.see_also ~cmds:["topkg-distrib"]

let cmd =
  let info = Term.info "opam" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure opam $ Cli.setup $ Cli.pkg_file $ cmd $
                Cli.distrib_determine $ opam_pkg_dir $ Cli.opam_file $
                descr_file $ Cli.dist_file $ field)
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
