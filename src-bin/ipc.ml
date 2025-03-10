(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bos_setup

let opam_fields file =
  begin
    Logs.info begin fun m ->
      m ~header:"IPC" "opam fields of %s with cwd %a" file
        (R.pp ~ok:Fpath.pp ~error:R.pp_msg) (OS.Dir.current ())
    end;
    Fpath.of_string file
    >>= fun file -> Topkg_care.Opam.File.fields file
    >>= fun fs -> Ok (String.Map.bindings fs)
    >>= fun fs -> Ok (Topkg.Private.(Codec.enc Opam.File.codec fs))
    >>= fun enc -> OS.File.(write dash enc)
  end
  |> R.reword_error_msg ~replace:true
    (fun msg -> R.msgf "ipc opam-fields: %s" msg)

let ipc_answer = function
| ["opam-fields"; file] -> opam_fields file
| args -> R.error_msgf "ipc: unknown arguments %a" Cmd.dump (Cmd.of_list args)

let ipc () args = match ipc_answer args with
| Ok () -> `Ok 0
| Error (`Msg msg) -> `Error (false, msg)

(* Command line interface *)

open Cmdliner

let args =
  let doc = "IPC call arguments" in
  Arg.(value (pos_all string [] & info [] ~doc ~docv:"ARG"))

let doc = "Interprocess communication with package description files"
let sdocs = Manpage.s_common_options
let exits = Cli.exits
let man_xrefs = [`Main]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command is used by package description files. It is
        undocumented." ]

let cmd =
  Cmd.v (Cmd.info "ipc" ~doc ~sdocs ~exits ~man ~man_xrefs) @@
  Term.(ret (const ipc $ Cli.setup $ args))
