(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Result

include Topkg_result

let strf = Topkg_string.strf
module String = Topkg_string

type fpath = string
module Fpath = Topkg_fpath
module Cmd = Topkg_cmd
module Log = Topkg_log
module OS = Topkg_os
module Vcs = Topkg_vcs

(* Package description *)

module Env = Topkg_conf
module Exts = Topkg_fexts
module Pkg = struct

  (* Distrib *)

  type watermark = Topkg_distrib.watermark
  type distrib = Topkg_distrib.t

  let distrib = Topkg_distrib.v
  let watermarks = Topkg_distrib.default_watermarks
  let files_to_watermark = Topkg_distrib.default_files_to_watermark
  let massage = Topkg_distrib.default_massage
  let exclude_paths = Topkg_distrib.default_exclude_paths

  (* Standard files *)

  type std_files = Topkg_std_files.t
  type std_file = Topkg_std_files.file
  let std_files = Topkg_std_files.v

  (* Linting *)

  type lint = Topkg_lint.t
  let lint = Topkg_lint.v

  (* Build *)

  type build = Topkg_build.t
  type build_context = Topkg_build.context
  let build = Topkg_build.v

  (* Install *)

  type install = Topkg_install.t
  type field = Topkg_install.field

  let lib = Topkg_install.lib
  let libexec = Topkg_install.libexec
  let bin = Topkg_install.bin
  let sbin = Topkg_install.sbin
  let toplevel = Topkg_install.toplevel
  let share = Topkg_install.share
  let share_root = Topkg_install.share_root
  let etc = Topkg_install.etc
  let doc = Topkg_install.doc
  let stublibs = Topkg_install.stublibs
  let misc = Topkg_install.misc
  let man = Topkg_install.man

  (* Package *)

  type t = Topkg_pkg.t

  let pr = Format.printf
  let pr_help () =
    pr "Usage example:@\n %s" Sys.argv.(0);
    List.iter (fun (k, v) -> match v with
    | `Bool b -> pr " %s %b" k b
    | `String s -> pr " %s %S" k s) (List.sort compare (Env.get ()));
    pr "@.";
    Ok 0

  let do_auto_main = ref true
  let disable_auto_main () = do_auto_main := false

  let auto_main pkg =
    begin
      disable_auto_main ();
      Log.info (fun m -> m "topkg %%VERSION%% auto main running");
      match Topkg_conf.cmd with
      | `Help -> pr_help ()
      | `Build -> Topkg_conf.warn_unused (); Topkg_pkg.run_build pkg
      | `Ipc cmd -> Topkg_ipc.answer cmd pkg
      | `Unknown args ->
          match args with
          | cmd :: _ -> R.error_msgf "Unknown command '%s'." cmd
          | [] -> R.error_msgf "Missing command."
    end
    |> Log.on_error_msg ~use:(fun () -> 1)

  let pkg = ref None

  let describe ?delegate ?std_files ?lint ?distrib ?build name installs =
    match !pkg with
    | Some _ -> invalid_arg "Topkg.Pkg.describe already called"
    | None ->
        let p =
          Topkg_pkg.v ?delegate ?std_files ?lint ?distrib ?build name installs
        in
        pkg := Some p;
        if !do_auto_main then exit (auto_main p) else ()

  let err_no_main () =
    if !do_auto_main && !pkg = None then
      Log.err begin fun m ->
        m "No@ package@ description@ found.@ A@ syntax@ error@ may@ have@ \
           occured@ or@ did@ you@ forget@ to@ call@ Topkg.Pkg.describe@ ?"
      end

  let () = at_exit err_no_main
end

module Private = struct
  let disable_auto_main = Pkg.disable_auto_main
  module Codec = Topkg_codec
  module Pkg = Topkg_pkg
  module Ipc = Topkg_ipc
  module Opam = Topkg_opam
end

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
