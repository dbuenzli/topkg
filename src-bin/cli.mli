(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Cmdliner} and common definitions for commands. *)

open Cmdliner
open Rresult

(** {1 Manual sections and fragments} *)

val common_opts :string

val common_opts_man : Manpage.block list
(** [common_opts_man] is the manual section for common options. *)

val see_also : cmds:string list -> Manpage.block list
(** [see_also cmds] is a "see also" manpage fragment. *)

(** {1 Converters, arguments and lookups} *)

val path_arg : Fpath.t Arg.converter
(** [path_arg] is a path argument converter. *)

val pkg_file : Fpath.t Term.t
(** A [--pkg-file] option to specify the package description file to use. *)

val ignore_pkg : bool Term.t
(** An [--ignore-pkg] option to ignore the package description file. *)

val dist_pkg_file : Fpath.t Term.t
(** A [--dist-pkg-file] option to define the package description
    file to use in a distribution. *)

val dist_file : Fpath.t option Term.t
(** A [--dist-file] option to define the distribution archive file. *)

val change_log : Fpath.t option Term.t
(** A [--change-log] option to define the change log. *)

val opam_file : Fpath.t option Term.t
(** An [--opam-file] option to define the opam file. *)

val delegate : string option Term.t
(** A [--delegate] option to define the delegate. *)

(** {1 Common Lookups} *)

val find_dist_file :
  Topkg_care.Distrib.det -> Fpath.t option ->
  (Fpath.t, [> Rresult.R.msg ]) Result.result
(** [find_dist_file] is a distribution archive file lookup procedure
    to use with {!dist_file} *)

(** {1 Terms} *)

val setup : unit Term.t
(** [setup env] defines a basic setup common to all. The setup includes,
    by side effect, setting log verbosity for {!Logs}, ajusting
    colored output and setting the current  *)

val distrib_determine :
  (pkg_file:Fpath.t -> (Topkg_care.Distrib.det, R.msg) result) Term.t
(** [distrib_determine] defines a function that given a package description
    file will determine a distribution. *)

(** {1 Error handling} *)

val handle_error : (int, R.msg) result -> int
(** [handle_error r] is [r]'s result or logs [r]'s error and returns [3]. *)

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
