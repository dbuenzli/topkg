(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( >>= ) v f = match v with Ok v -> f v | Error _ as e -> e
let ( >>| ) v f = match v with Ok v -> Ok (f v) | Error _ as e -> e

type ('a, 'b) r = ('a, 'b) result = Ok of 'a |	Error of 'b
type 'a result = ('a, [`Msg of string]) r

module R = struct
  type msg = [`Msg of string ]

  let msgf fmt =
    let kmsg _ = `Msg (Format.flush_str_formatter ()) in
    Format.kfprintf kmsg Format.str_formatter fmt

  let reword_error reword = function
  | Ok _ as r -> r
  | Error e -> Error (reword e)

  let error_msg m = Error (`Msg m)
  let error_msgf fmt =
    let kerr _ = Error (`Msg (Format.flush_str_formatter ())) in
    Format.kfprintf kerr Format.str_formatter fmt

  let reword_error_msg ?(replace = false) reword = function
  | Ok _ as r -> r
  | Error (`Msg e) ->
      let (`Msg e' as v) = reword e in
      if replace then Error v else error_msgf "%s\n%s" e e'
end
