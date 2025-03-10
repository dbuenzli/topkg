(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** File extensions.

    See {!Topkg.Exts} for documentation. *)

(** {1 File extensions} *)

type ext = [`Ext of string | `Obj | `Real_clib | `Lib | `Dll | `Exe]

type t = ext list

val interface : ext list
val api : ext list
val cmx : ext list
val real_c_library : ext list
val c_library : ext list
val c_dll_library : ext list
val library : ext list
val module_library : ext list
val exe : ext list
val exts : string list -> ext list
val ext : string -> ext list
val ext_to_string : Topkg_conf.OCaml.t -> ext -> string
