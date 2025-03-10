(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bos_setup

let bistro () =
  begin
    let verb = Cmd.(v "--verbosity" % Logs.(level_to_string (level ()))) in
    let topkg = Cmd.(v "topkg") in
    OS.Cmd.run Cmd.(topkg % "distrib" %% verb)
    >>= fun () -> OS.Cmd.run Cmd.(topkg % "publish" %% verb)
    >>= fun () -> OS.Cmd.run Cmd.(topkg % "opam" %% verb % "pkg")
    >>= fun () -> OS.Cmd.run Cmd.(topkg % "opam" %% verb % "submit")
    >>= fun () -> Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let doc = "For when you are in a hurry or need to go for a drink"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let man_xrefs = [ `Main; `Cmd "distrib"; `Cmd "publish"; `Cmd "opam" ]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command (quick in Russian) is equivalent to invoke:";
  `Pre "\
topkg distrib       # Create the distribution archive
topkg publish       # Publish it on the WWW with its documentation
topkg opam pkg      # Create an opam package
topkg opam submit   # Submit it to OCaml's opam repository";
    `P "See topkg-release(7) for more information."; ]

let cmd =
  Cmd.v (Cmd.info "bistro" ~doc ~sdocs ~exits ~man ~man_xrefs) @@
  Term.(const bistro $ Cli.setup)
