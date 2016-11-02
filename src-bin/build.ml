(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let build_args pkg_name build_dir dry_run tests debug args =
  let on_some_use_opt opt to_arg = function
  | None -> Cmd.empty
  | Some value -> Cmd.(v opt % to_arg value)
  in
  let verb = Cli.propagate_verbosity_to_pkg_file () in
  let pkg_name = on_some_use_opt "--pkg-name" (fun x -> x) pkg_name in
  let build_dir = on_some_use_opt "--build-dir" Cmd.p build_dir in
  let dry_run = if dry_run then Cmd.(v "--dry-run") else Cmd.empty in
  let tests = on_some_use_opt "--tests" String.of_bool tests in
  let debug = on_some_use_opt "--debug" String.of_bool debug in
  Cmd.(verb %% dry_run %% pkg_name %% build_dir %% tests %% debug %%
       Cmd.of_list args)

let build () pkg_file pkg_name build_dir dry_run tests debug args =
  let pkg = Topkg_care.Pkg.v pkg_file in
  let args = build_args pkg_name build_dir dry_run tests debug args in
  let out = OS.Cmd.out_stdout in
  begin
    OS.Dir.current ()
    >>= fun dir -> Topkg_care.Pkg.build pkg ~dir ~args ~out
    >>| (function ((), (_, `Exited 0)) -> 0 | _ -> 1)
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let args =
  let doc = "Build configuration. Needs to be specified after a -- token
             so that the command line options do not get interpreted by
             $(b,topkg build) itself."
  in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"BUILD_CONF")

let pkg_name =
  let doc = "The name $(docv) of the package (and of the OPAM install file).
             This is equivalent to specify the same option after the -- token.
             If absent provided by the package description."
  in
  let docv = "PKG_NAME" in
  Arg.(value & opt (some string) None & info ["n"; "pkg-name"] ~doc ~docv)

let build_dir =
  let doc = "Specifies the build directory $(docv). This is equivalent to
             specify the same option after the -- token. If absent, provided
             by the package description."
  in
  let docv = "BUILD_DIR" in
  Arg.(value & opt (some Cli.path_arg) None & info ["build-dir"] ~doc ~docv)

let dry_run =
  let doc = "Do not run build instructions, only determine and write the OPAM
             install file. This is equivalent to specify the same option after
             the -- token."
  in
  Arg.(value & flag & info ["d"; "dry-run"] ~doc)

let tests =
  let doc = "Specifies whether tests should be built. If absent depends on the
             build context, true for development and false otherwise. This is
             equivalent to specify the same option after the -- token."
  in
  Arg.(value & opt (some bool) None  & info ["tests"] ~doc ~docv:"BOOL")

let debug =
  let doc = "Debug build. Specifies if debugging information should be
             saved in build artefacts. This is equivalent to specify the
             same option after the -- token."
  in
  let env = Arg.env_var "TOPKG_CONF_DEBUG" in
  Arg.(value & opt (some bool) None  & info ["debug"] ~env ~doc ~docv:"BOOL")

let doc = "Build the package"
let man =
  [ `S "SYNOPSIS";
    `P "$(mname) $(tname) [$(i,OPTION)]... [-- $(i,BUILD_CONF)...]";
    `S "DESCRIPTION";
    `P "The $(tname) command builds the package. This is equivalent to
        invoke:";
    `Pre "ocaml ./pkg/pkg.ml build $(i,BUILD_CONF)...";
  ] @ Cli.common_opts_man @ [
    `S "EXIT STATUS";
    `P "The $(tname) command exits with one of the following values:";
    `I ("0", "the build succeeded.");
    `I ("1", "the build failed.");
    `I (">1", "an error occured.");
    `S "ENVIRONMENT VARIABLES";
  ] @ Cli.see_also ~cmds:[]

let cmd =
  let info = Term.info "build" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure build $ Cli.setup $ Cli.pkg_file $ pkg_name $ build_dir $
                dry_run $ tests $ debug $ args)
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
