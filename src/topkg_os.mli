(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** OS interaction.

    Abridged [bos]. See {!Topkg.OS} for documentation. *)

(** {1 OS} *)

open Topkg_result

module Env : sig
  val var : string -> string option
  val opt_var : string -> absent:string -> string
end

module File : sig
    val null : Topkg_fpath.t
    val dash : Topkg_fpath.t

    val exists : Topkg_fpath.t -> bool result
    val must_exist : Topkg_fpath.t -> Topkg_fpath.t result
    val delete : ?must_exist:bool -> Topkg_fpath.t -> unit result

    val fold :
      ?skip:(Topkg_fpath.t -> bool) -> (Topkg_fpath.t -> 'a -> 'a) ->
      'a -> Topkg_fpath.t list -> 'a result

    val read : Topkg_fpath.t -> string result
    val write : Topkg_fpath.t -> string -> unit result
    val write_subst :
      Topkg_fpath.t -> (string * string) list -> string -> unit result

    val tmp : unit -> Topkg_fpath.t result
end

module Dir : sig
  val exists : Topkg_fpath.t -> bool result
  val must_exist : Topkg_fpath.t -> Topkg_fpath.t result

  val current : unit -> Topkg_fpath.t result
  val set_current : Topkg_fpath.t -> unit result
  val with_current : Topkg_fpath.t -> ('a -> 'b) -> 'a -> 'b result

  val contents :
    ?dotfiles:bool -> ?rel:bool -> Topkg_fpath.t -> Topkg_fpath.t list result
end

module Cmd : sig
  val exists : Topkg_cmd.t -> bool result
  val must_exist : Topkg_cmd.t -> Topkg_cmd.t result

  val run : ?err:Topkg_fpath.t -> Topkg_cmd.t -> unit result
  val run_status : ?err:Topkg_fpath.t -> Topkg_cmd.t -> [`Exited of int] result


  type run_status = Topkg_cmd.t * [`Exited of int ]
  val success : ('a * run_status) result -> 'a result

  type run_out

  val out_string : ?trim:bool -> run_out -> (string * run_status) result
  val out_lines : ?trim:bool -> run_out -> (string list * run_status) result
  val out_file : Topkg_fpath.t -> run_out -> (unit * run_status) result
  val out_stdout : run_out -> (unit * run_status) result

  val to_string : ?trim:bool -> run_out -> string result
  val to_lines : ?trim:bool -> run_out -> string list result
  val to_file : Topkg_fpath.t -> run_out -> unit result
  val run_out : ?err:Topkg_fpath.t -> Topkg_cmd.t -> run_out
end
