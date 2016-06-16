(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let test_args build_dir list args =
  let on_some_use_opt opt to_arg = function
  | None -> Cmd.empty
  | Some value -> Cmd.(v opt % to_arg value)
  in
  let verb = Cli.propagate_verbosity_to_pkg_file () in
  let build_dir = on_some_use_opt "--build-dir" Cmd.p build_dir in
  let list = if list then Cmd.(v "--list") else Cmd.empty in
  Cmd.(verb %% list %% build_dir %% Cmd.of_list args)

let test () pkg_file build_dir list args =
  let pkg = Topkg_care.Pkg.v pkg_file in
  let args = test_args build_dir list args in
  let out = OS.Cmd.out_stdout in
  begin
    OS.Dir.current ()
    >>= fun dir -> Topkg_care.Pkg.test pkg ~dir ~args ~out
    >>| (function ((), (_, `Exited 0)) -> 0 | _ -> 1)
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let args =
  let doc = "Tests and arguments to the tests. If options are being
             passed, needs to be specified after a -- token so that the
             command line options do not get interpreted by $(b,topkg test)
             itself. If arguments need to be specified for the test itself a
             second -- token is needed."
  in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"[TEST]... [-- ARG...]")

let build_dir =
  let doc = "Specifies the build directory $(docv). If absent, provided
             by the package description. This is equivalent to specify
             the same option after the first -- token"
  in
  let docv = "BUILD_DIR" in
  Arg.(value & opt (some Cli.path_arg) None & info ["build-dir"] ~doc ~docv)

let list =
  let doc = "Do not run the tests, list them. This is equivalent to
             specify the same option after the first -- token."
  in
  Arg.(value & flag & info ["l"; "list"] ~doc)

let doc = "Run built package tests"
let man =
  [ `S "SYNOPSIS";
    `P "$(b,$(mname)) $(b,$(tname)) [$(i,OPTION)]... [-- [$(i,TEST)]... \
        [-- [$(i,ARG)]...]]";
    `S "DESCRIPTION";
    `P "The $(b,$(tname)) command runs the tests that were built by
        topkg-build(1). This is equivalent to invoke:";
    `Pre "ocaml ./pkg/pkg.ml test [$(i,TEST)]... [-- [$(i,ARG)]...]";
    `P "The value for $(i,TEST) can be a full path to the test executable
        or simply the basename of the test executable with or without the file
        extension. The option $(b,--list) lists the tests that were built.";
    `P "Note that if you want to pass command line arguments to a test you
       need to specify the token -- twice. For example to pass 'arg' to a
       test 'mytest' use one of the following invocation:";
    `Pre "topkg test -- mytest -- arg"; `Noblank;
    `Pre "topkg test mytest -- -- arg";
  ] @ Cli.common_opts_man @ [
    `S "EXIT STATUS";
    `P "The $(b,$(tname)) command exits with one of the following values:";
    `I ("0", "the tests succeeded.");
    `I ("1", "the tests failed.");
    `I (">1", "an error occured.");
    `S "ENVIRONMENT VARIABLES";
  ] @ Cli.see_also ~cmds:[]

let cmd =
  let info = Term.info "test" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure test $ Cli.setup $ Cli.pkg_file $ build_dir $ list $ args)
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
