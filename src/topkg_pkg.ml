(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type t =
  { name : string;
    delegate : string option;
    std_files : Topkg_std_files.t;
    lint : Topkg_lint.t;
    distrib : Topkg_distrib.t;
    build : Topkg_build.t;
    install : Topkg_install.t; }

let v
    ?delegate
    ?(std_files = Topkg_std_files.v ())
    ?(lint = Topkg_lint.v ())
    ?(distrib = Topkg_distrib.v ())
    ?(build = Topkg_build.v ())
    name installs
  =
  let std = Topkg_std_files.install std_files in
  let installs = List.rev_append std installs in
  let install = List.sort compare (List.flatten installs) in
  { name; delegate; std_files; lint; distrib; build; install }

let name p = p.name
let delegate p = p.delegate
let std_files p = p.std_files
let lint p = p.lint
let distrib p = p.distrib
let build p = p.build
let install p = p.install

let run_build p =
  let header = p.name in
  let conf = Topkg_conf.OCaml.v `Host_os in
  let targets, install =
    Topkg_install.to_instructions ~header ~bdir:(Topkg_build.dir p.build)
      conf p.install
  in
  let prepare =
    match Topkg_build.prepare_on_pin p.build && Topkg_conf.build = `Pin with
    | false -> Ok ()
    | true ->
        Topkg_distrib.run_prepare_pin ~name:p.name p.distrib
        |> R.reword_error_msg ~replace:true
          (fun e -> R.msgf "Distribution preparation failed: %s" e)
  in
  prepare
  >>= fun () ->
  (Topkg_build.pre p.build Topkg_conf.build)
  |> R.reword_error_msg ~replace:true
    (fun e -> R.msgf "Pre-build hook failed: %s" e)
  >>= fun () ->
  let build_cmd =
    (Topkg_build.cmd p.build) Topkg_conf.build `Host_os
      (Topkg_build.dir p.build)
  in
  Topkg_os.Cmd.run Topkg_cmd.(build_cmd %% of_list targets)
  >>= fun () ->
  let install_file = p.name ^ ".install" in
  let install = Topkg_opam.Install.to_string install in
  Topkg_os.File.write install_file install
  >>= fun () ->
  (Topkg_build.post p.build Topkg_conf.build)
  |> R.reword_error_msg ~replace:true
    (fun e -> R.msgf "Post-build hook failed: %s" e)
  >>= fun () -> Ok 0



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
