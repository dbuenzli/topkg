(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** IPC with package description files *)

(** {1 Asking packages} *)

open Bos_setup

val ocaml : Cmd.t
(** [ocaml] is a command for [ocaml] looked up using
      {!Topkg.Conf.tool}[ "ocaml" `Build_os]. *)

val ask : pkg_file:Fpath.t -> 'a Topkg.Private.Ipc.t -> ('a, R.msg) result
(** [ask pkg_file ipc] performs the IPC [ipc] with the package description
    file [pkg_file] using the interpreter {!ocaml}. *)
