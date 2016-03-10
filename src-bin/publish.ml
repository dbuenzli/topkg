(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring
open Bos

let absolute path = OS.Dir.current () >>| fun cwd -> Fpath.(cwd // path)

let get_kind = function
| Some k -> Ok k
| None -> R.error_msgf "No alternate artefact KIND specified"

let get_dist_file det = function
| Some f -> f
| None -> Topkg_care.Distrib.archive_path det

let get_msg = function
| Some msg -> Ok msg
| None ->
    let change_log = Fpath.v "CHANGES.md" (* FIXME lookup *) in
    OS.File.read change_log >>= fun text ->
    let flavour = Topkg_care.Text.flavour_of_fpath change_log in
    match Topkg_care.Change_log.last_version ?flavour text with
    | None -> R.error_msgf "%a: Could not parse change log." Fpath.pp change_log
    | Some (_, (header, changes)) -> Ok (strf "%s\n%s" header changes)

let gen_doc dir =
  let do_doc () =
    OS.Cmd.run Cmd.(v "topkg" % "doc")
    >>| fun () -> Fpath.(dir / "_build" / "doc" / "api.docdir")
  in
  OS.Dir.with_current dir do_doc ()

let publish_doc ~del det dist_file msg =
  let name = Topkg_care.Distrib.name det in
  let version = Topkg_care.Distrib.version det in
  let dist_file = get_dist_file det dist_file in
  OS.File.must_exist dist_file
  >>= fun _ -> get_msg msg
  >>= fun msg -> Topkg_care.Distrib.unarchive ~clean:true dist_file
  >>= fun dir -> gen_doc dir
  >>= fun docdir -> absolute docdir
  >>= fun docdir ->
  Topkg_care.Delegate.publish_doc ~del ~name ~version ~msg ~docdir

let publish_distrib ~del det dist_file msg =
  let name = Topkg_care.Distrib.name det in
  let version = Topkg_care.Distrib.version det in
  let dist_file = get_dist_file det dist_file in
  OS.File.must_exist dist_file
  >>= fun _ -> get_msg msg
  >>= fun msg -> absolute dist_file
  >>= fun archive ->
  Topkg_care.Delegate.publish_distrib ~del ~name ~version ~msg ~archive

let publish_alt ~del det dist_file kind msg =
  let name = Topkg_care.Distrib.name det in
  let version = Topkg_care.Distrib.version det in
  let dist_file = get_dist_file det dist_file in
  OS.File.must_exist dist_file
  >>= fun _ -> get_msg msg
  >>= fun msg -> absolute dist_file
  >>= fun archive -> get_kind kind
  >>= fun kind ->
  Topkg_care.Delegate.publish_alt ~del ~kind ~name ~version ~msg ~archive

let publish () pkg_file det opam_file del artefact msg dist_file kind =
  begin
    let todo = match artefact with None -> [`Doc; `Distrib] | Some a -> [a] in
    det ~pkg_file
    >>= fun det -> Topkg_care.Delegate.find ~pkg_file ~opam:opam_file ~del
    >>= function
    | None ->
        Logs.err (fun m -> m "%a" Topkg_care.Delegate.pp_not_found ()); Ok 1
    | Some del ->
        let do_doc = List.mem `Doc todo in
        let do_distrib = List.mem `Distrib todo in
        let do_alt = List.mem `Alt todo in
        (if do_doc then publish_doc ~del det dist_file msg else Ok 0)
        >>= fun ret ->
        (if do_distrib then publish_distrib ~del det dist_file msg else Ok 0)
        >>= fun ret ->
        (if do_alt then publish_alt ~del det dist_file kind msg else Ok 0)

  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let artefact =
  let artefact = [ "doc", `Doc; "distrib", `Distrib; "alt", `Alt] in
  let doc = strf "The artefact to publish. $(docv) must be one of %s. If
                  absent, determined by the package description."
      (Arg.doc_alts_enum artefact)
  in
  let artefact = Arg.enum artefact in
  Arg.(value & pos 0 (some artefact) None & info [] ~doc ~docv:"ARTEFACT")

let msg =
  let doc = "The publication message $(docv). Defaults to the change log
             of the last version found in the distribution archive."
  in
  let docv = "MSG" in
  Arg.(value & opt (some string) None & info ["m"; "message"] ~doc ~docv)

let kind =
  let doc = "For $(b,alt) artefacts, the artefact kind $(docv). The
             semantics of alternative artefact kinds is left to the
             delegate.";
  in
  Arg.(value & pos 1 (some string) None & info [] ~doc ~docv:"KIND")

let doc = "publish package distribution archives and derived artefacts"
let man =
  [ `S "SYNOPSIS";
    `P "$(b,$(mname)) $(b,$(tname)) [$(i,OPTION)]... [$(i,ARTEFACT)]...";
    `S "DESCRIPTION";
    `P "The $(b,$(tname)) command publishes package distribution archives
        and other artefacts via the package delegate. See topkg-delegate(7) for
        more details.";
    `P "Artefact publication always relies on a distribution archive having
        been generated before with topkg-distrib(1).";
    `S "ARTEFACTS";
    `I ("$(b,distrib)",
        "Publishes a distribution archive on the WWW.");
    `I ("$(b,doc)",
        "Publishes the documentation of a distribution archive on the WWW.");
    `I ("$(b,alt) $(i,KIND)",
        "Publishes the alternative artefact of kind $(i,KIND) of
         a distribution archive. The semantics of alternative artefacts
         is left to the delegate, it could be anything, an email,
         a pointless tweet, a feed entry etc. See topkg-delegate(7) for
         more details.");
    `S "ARGUMENTS";
    `S "OPTIONS";
  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
    `I ("$(i,TOPKG_DELEGATE)",
        "The package delegate to use, see topkg-delegate(7).");
  ] @ Cli.see_also ~cmds:["topkg-distrib"]

let cmd =
  let info = Term.info "publish" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure publish $ Cli.setup $ Cli.pkg_file $
                Cli.distrib_determine $ Cli.opam_file $ Cli.delegate $
                artefact $ msg $ Cli.dist_file $ kind)
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
