(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let ocamlbuild = Cmd.(v "ocamlbuild" % "-classic-display" % "-use-ocamlfind"
                      % "-no-links")
let docflags = Cmd.(v "-docflags" % "-colorize-code,-charset,utf-8")
let build_dir = Fpath.v "_build" (* TODO *)

let copy_assets src_dir dst_dir =
  let copy_asset dst_dir file = match Fpath.get_ext file with
  | ".css" | ".svg" | ".svgz" | ".png" | ".jpeg" | ".gif" | ".woff" | ".ttf"
  | ".otf" | ".eot" ->
      begin OS.File.exists file >>= function
      | false -> Ok ()
      | true ->
          OS.File.read file
          >>= fun cont -> OS.File.write Fpath.(dst_dir / filename file) cont
      end
      |> Logs.on_error_msg ~use:(fun () -> ())
  | _ -> ()
  in
  OS.Dir.contents src_dir
  >>= fun files -> List.iter (copy_asset dst_dir) files; Ok ()

let do_doc dev build_dir =
  let doc_dir = Fpath.v "doc" in
  let odocl = Fpath.(doc_dir / (if dev then "dev.odocl" else "api.odocl")) in
  OS.File.must_exist odocl
  >>= fun _ -> Ok Fpath.(set_ext ".docdir" odocl / "index.html")
  >>= fun target -> OS.Cmd.run Cmd.(ocamlbuild %% docflags % p target)
  >>= fun () -> Ok Fpath.(build_dir // parent target)
  >>= fun dst_dir -> copy_assets doc_dir dst_dir
  >>= fun () -> Ok dst_dir

let do_browser_reload reload ~background ~browser dir =
  if not (reload || background) then Ok () else
  let prefix = true in
  OS.Dir.current ()
  >>= fun cwd -> Ok (strf "file://%s" Fpath.(to_string @@ cwd // dir))
  >>= fun uri -> Topkg_care.Browser.reload ~background ~prefix ?browser ~uri

let doc_cmd () pkg_file dev reload background browser =
  begin
    do_doc dev build_dir
    >>= fun docdir -> do_browser_reload reload ~background ~browser docdir
    >>= fun () ->
    Logs.app (fun m -> m "Generated %s doc in %a"
                 (if dev then "dev" else "API")
                 (Fmt.styled `Bold Fpath.pp) docdir);
    Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let reload_browser =
  let doc = "Open an URI of the documentation directory or reload an
             existing browser tab that holds a sub-page of the documentation."
  in
  Arg.(value & flag & info ["r"; "reload-browser"] ~doc)

let dev =
  let doc = "Build the development documentation." in
  Arg.(value & flag & info ["dev"] ~doc)

let doc = "build the package's API documentation"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) build the package's API documentation. Use
        the option $(b,-r) to open or refresh the documentation in
        a WWW browser (see $(b,--browser) for details).";
    `P "$(b,WARNING.) The way this command works is at the moment
        moment very ad-hoc and ocamlbuild specific. It will
        change in the future.";
    `P "Current support relies on having a doc/ directory at the root of the
        distribution. The ocamlbuild file doc/api.odocl defines the API
        documentation and the doc/dev.odocl the development documentation.
        The directory can also hold CSS, PNG, JPEG, GIF, SVG, WOFF, TTF, OTF
        files that are copied over to the generated documentation directory.";
  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
  ] @ Cli.see_also ~cmds:[]

let cmd =
  let info = Term.info "doc" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure doc_cmd $ Cli.setup $ Cli.pkg_file $ dev $
                reload_browser $ Topkg_care.Browser.Cli.background $
                Topkg_care.Browser.Cli.browser)
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
