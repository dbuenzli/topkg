(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Results

    Abbridged  [rresult]. See {!section:Topkg.prels} for documention. *)

val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result

type ('a, 'b) r = ('a, 'b) result = Ok of 'a | Error of 'b
type 'a result = ('a, [ `Msg of string]) r

module R : sig
  val reword_error : ('b -> 'c) -> ('a, 'b) r -> ('a, 'c) r

  type msg = [ `Msg of string ]

  val msgf : ('a, Format.formatter, unit, [> msg]) format4 -> 'a

  val error_msg : string -> ('b, [> msg]) r
  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> msg]) r) format4 -> 'a

  val reword_error_msg :
    ?replace:bool -> (string -> msg) -> ('a, msg) r -> ('a, [> msg]) r
end
