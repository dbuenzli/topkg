(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Command *)

let clean () pkg_file build_dir name =
  begin
    let p = Topkg_care.Pkg.v ?build_dir ?name pkg_file in
    Topkg_care.Pkg.name p
    >>= fun name -> Fpath.of_string (name ^ ".install")
    >>= fun install -> OS.File.delete install
    >>= fun () -> Topkg_care.Pkg.build_dir p
    >>= fun build_dir -> OS.Dir.delete ~recurse:true build_dir
    >>= fun () -> Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let doc = "Clean the package's build"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) command deletes the package's build directory
        and its OPAM install file."
  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
  ] @ Cli.see_also ~cmds:["topkg-build"]

let cmd =
  let info = Term.info "clean" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure clean $ Cli.setup $ Cli.pkg_file $ Cli.build_dir $
                Cli.pkg_name)
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
