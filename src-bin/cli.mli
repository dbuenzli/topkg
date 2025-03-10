(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Cmdliner} and common definitions for commands. *)

open Cmdliner
open Rresult

(** {1 Converters and options} *)

val path_arg : Fpath.t Arg.conv
(** [path_arg] is a path argument converter. *)

val pkg_file : Fpath.t Term.t
(** A [--pkg-file] option to specify the package description file to use. *)

val pkg_name : string option Term.t
(** A [--pkg-name] option to specify the opam package name. *)

val opam : Fpath.t option Term.t
(** An [--opam] option for defining an opam file. *)

val dist_name : string option Term.t
(** A [--dist-name] option to define the package name of the distribution. *)

val dist_version : string option Term.t
(** A [--dist-version] option to define the package version. *)

val dist_file : Fpath.t option Term.t
(** A [--dist-file] option to define the distribution archive file. *)

val dist_uri : string option Term.t
(** A [--dist-uri] option to define the distribution archive URI on the WWW. *)

val dist_opam : Fpath.t option Term.t
(** An [--dist-opam] option to define the opam file. *)

val readme : Fpath.t option Term.t
(** A [--readme] option to define the readme. *)

val change_log : Fpath.t option Term.t
(** A [--change-log] option to define the change log. *)

val opam : Fpath.t option Term.t
(** An [--opam] option to define an opam file. *)

val delegate : Bos.Cmd.t option Term.t
(** A [--delegate] option to define the delegate. *)

val build_dir : Fpath.t option Term.t
(** A [--build-dir] option to define the build directory. *)

val publish_msg : string option Term.t
(** A [--msg] option to define a publication message. *)

(** {1 Terms} *)

val setup : unit Term.t
(** [setup env] defines a basic setup common to all commands. The
    setup does, by side effect, set {!Logs} log verbosity, adjusts
    colored output and sets the current working directory. *)

(** {1 Verbosity propagation} *)

val propagate_verbosity_to_pkg_file : unit -> Bos.Cmd.t
(** [propagate_verbosity_to_pkg_file ()] is
    a command line fragment that has the option to propagate
    the current log verbosity to an invocation of the package
    description. *)

(** {1 Warnings and errors} *)

val warn_if_vcs_dirty : string -> (unit, R.msg) result
(** [warn_if_vcs_dirty msg] warns with [msg] if the VCS is dirty. *)

val handle_error : (int, R.msg) result -> int
(** [handle_error r] is [r]'s result or logs [r]'s error and returns [3]. *)

val exits : Cmd.Exit.info list
(** [exits] is are the exit codes common to all commands. *)
