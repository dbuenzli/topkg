(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let absolute path = OS.Dir.current () >>| fun cwd -> Fpath.(cwd // path)

let get_kind = function
| Some k -> Ok k
| None -> R.error_msgf "No alternate artefact KIND specified"

let gen_doc dir =
  let do_doc () =
    OS.Cmd.run Cmd.(v "topkg" % "doc")
    >>| fun () -> Fpath.(dir / "_build" / "doc" / "api.docdir")
  in
  R.join @@ OS.Dir.with_current dir do_doc ()

let publish_doc pkg =
  Topkg_care.Pkg.distrib_file pkg
  >>= fun distrib_file -> Topkg_care.Pkg.publish_msg pkg
  >>= fun msg -> Topkg_care.Archive.untbz ~clean:true distrib_file
  >>= fun dir -> gen_doc dir
  >>= fun docdir -> absolute docdir
  >>= fun docdir -> Topkg_care.Delegate.publish_doc pkg ~msg ~docdir

let publish_distrib pkg =
  Topkg_care.Pkg.distrib_file pkg
  >>= fun distrib_file -> Topkg_care.Pkg.publish_msg pkg
  >>= fun msg -> absolute distrib_file
  >>= fun archive -> Topkg_care.Delegate.publish_distrib pkg ~msg ~archive

let publish_alt pkg kind =
  get_kind kind
  >>= fun kind -> Topkg_care.Pkg.distrib_file pkg
  >>= fun distrib_file -> Topkg_care.Pkg.publish_msg pkg
  >>= fun msg -> absolute distrib_file
  >>= fun archive -> Topkg_care.Delegate.publish_alt pkg ~kind ~msg ~archive

let publish ()
    pkg_file build_dir name version opam delegate change_log distrib_uri
    distrib_file publish_msg artefact kind
  =
  begin
    let pkg = Topkg_care.Pkg.v ?name ?version ?build_dir ?opam ?delegate
        ?change_log ?distrib_uri ?distrib_file ?publish_msg pkg_file
    in
    let todo = match artefact with None -> [`Doc; `Distrib] | Some a -> [a] in
    let publish_artefact acc artefact =
      acc >>= fun acc -> match artefact with
      | `Doc -> publish_doc pkg
      | `Distrib -> publish_distrib pkg
      | `Alt -> publish_alt pkg kind
    in
    List.fold_left publish_artefact (Ok ()) todo >>= fun () -> Ok 0
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
  let t = Term.(pure publish $ Cli.setup $ Cli.pkg_file $ Cli.build_dir $
                Cli.dist_name $ Cli.dist_version $ Cli.dist_opam $
                Cli.delegate $ Cli.change_log $ Cli.dist_uri $ Cli.dist_file $
                Cli.publish_msg $ artefact $ kind)
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
