(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let cmds =
  [ Bistro.cmd; Browse.cmd; Build.cmd; Clean.cmd; Distrib.cmd; Doc.cmd;
    Help.cmd; Ipc.cmd; Issue.cmd; Lint.cmd; Log.cmd; Opam.cmd;
    Publish.cmd; Run.cmd; Status.cmd; Tag.cmd; Test.cmd; ]

let main () = `Help (`Pager, None)

(* Command line interface *)

let doc = "Topkg package care"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let man =
  [ `S Manpage.s_description;
    `P "$(mname) takes care of topkg packages.";
    `P "Use '$(mname) help release' for help to release a package.";
    `Noblank;
    `P "Use '$(mname) help delegate' for help about the topkg delegate.";
    `Noblank;
    `P "Use '$(mname) help troubleshoot' for a few troubleshooting tips.";
    `Noblank;
    `P "Use '$(mname) help $(i,COMMAND)' for help about $(i,COMMAND).";
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information.";
    `S Manpage.s_authors;
    `P "Daniel C. Buenzli, $(i,http://erratique.ch)"; ]

let main =
  let default = Term.(ret (const main $ Cli.setup)) in
  let info = Cmd.info "topkg" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man in
  Cmd.group ~default info cmds

let main () =
  Topkg.Private.disable_main ();
  Cmd.eval' main

let () = if !Sys.interactive then () else exit (main ())
