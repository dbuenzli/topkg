(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bos_setup

let ocaml =
  Cmd.of_list @@ Topkg.Cmd.to_list @@ Topkg.Conf.tool "ocaml" `Build_os

let pkg_must_exist pkg_file = match OS.File.must_exist pkg_file with
| Ok _ -> Ok pkg_file
| Error _ ->
    let p = match OS.Dir.current () with
    | Error _ -> (* ignore *) pkg_file
    | Ok dir -> Fpath.(dir // pkg_file)
    in
    R.error_msgf "Not a package: no file %a" Fpath.pp p

let ask ~pkg_file ipc =
  let codec = Topkg.Private.Ipc.codec ipc in
  let verbosity = Logs.(level_to_string (level ())) in
  let ipc_cmd = Cmd.of_list @@ Topkg.Cmd.to_list @@ Topkg.Private.Ipc.cmd ipc in
  let cmd = Cmd.(ocaml % p pkg_file % "ipc" % verbosity %% ipc_cmd) in
  pkg_must_exist pkg_file >>= fun pkg_file ->
  begin
    OS.Cmd.run cmd
    >>= fun () -> Fpath.of_string (Topkg.Private.Ipc.answer ipc)
    >>= fun answer -> OS.File.read answer
    >>= fun data -> Topkg.Private.Codec.dec_result codec data
  end
  |> R.reword_error_msg
    (fun _ -> R.msgf "Failed to load package description %a" Fpath.pp pkg_file)
