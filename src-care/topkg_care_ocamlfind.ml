(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bos_setup

let cmd =
  Cmd.of_list @@ Topkg.Cmd.to_list @@ Topkg.Conf.tool "ocamlfind" `Host_os

let base_packages = String.Set.of_list
    [ "bigarray"; "bytes"; "compiler-libs"; "dynlink"; "graphics"; "num";
      "ocamldoc"; "stdlib"; "str"; "threads"; "unix" ]
