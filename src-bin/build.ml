(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)


open Astring
open Rresult
open Bos

let build () pkg_file args =
  let args = "installer" :: "false" :: args in
  let out = OS.Cmd.out_stdout in
  begin
    OS.Dir.current ()
    >>= fun dir -> Topkg_care.Build.pkg ~pkg_file ~dir ~args ~out
    >>= function ((), (_, `Exited 0)) -> Ok 0 | _ -> Ok 1
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let args =
  let doc = "Build configuration." in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"KEY VAL")

let doc = "build the package"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) builds the package. This is equivalent to
        invoke:";
    `Pre "  > ocaml ./pkg/pkg.ml build installer false ...";
  ] @ Cli.common_opts_man @ [
    `S "EXIT STATUS";
    `P "The $(b,$(tname)) command exits with one of the following values:";
    `I ("0", "the build succeeded.");
    `I ("1", "the build failed.");
    `I (">1", "an error occured.");
    `S "ENVIRONMENT VARIABLES";
  ] @ Cli.see_also ~cmds:[]

let cmd =
  let info = Term.info "build" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure build $ Cli.setup $ Cli.pkg_file $ args) in
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
