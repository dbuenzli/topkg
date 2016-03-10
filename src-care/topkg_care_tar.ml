(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

type ptime = int
type t = string list

let empty = []

(* Header.

   See http://pubs.opengroup.org/onlinepubs/9699919799/utilities/\
       pax.html#tag_20_92_13_06  *)

let to_unix_path_string =
  if Fpath.dir_sep = "/" then Fpath.to_string else
  fun f -> String.concat ~sep:"/" (Fpath.segs f)

let set_filename h f =
  let s = to_unix_path_string f in
  match String.length s with
  | n when n <= 100 -> Bytes.blit_string s 0 h 0 (String.length s)
  | n ->
      try match String.cut ~rev:true ~sep:"/" s with
      | None -> raise Exit
      | Some (p, n) ->
          (* This could be made more clever by trying to find
               the slash nearest to the half string position. *)
          if String.length p > 155 || String.length n > 100 then raise Exit;
          Bytes.blit_string n 0 h 0 (String.length n);
          Bytes.blit_string p 0 h 345 (String.length p);
      with
      | Exit -> failwith (strf "%a: file name too long" Fpath.pp f)

let set_string off h s = Bytes.blit_string s 0 h off (String.length s)
let set_octal field off len (* terminating NULL included *) h n =
  let octal = Printf.sprintf "%0*o" (len - 1) n in
  if String.length octal < len
  then Bytes.blit_string octal 0 h off (String.length octal) else
  failwith (strf "field %s: can't encode %d in %d-digit octal number"
              field (len - 1) n)

let header_checksum h =
  let len = Bytes.length h in
  let rec loop acc i =
    if i > len then acc else
    loop (acc + (Char.to_int @@ Bytes.unsafe_get h i)) (i + 1)
  in
  loop 0 0

let header fname mode mtime size typeflag =
  try
    let h = Bytes.make 512 '\x00' in
    set_filename h fname;
    set_octal "mode"  100 8 h mode;
    set_octal "owner" 108 8 h 0;
    set_octal "group" 116 8 h 0;
    set_octal "size"  124 12 h size;
    set_octal "mtime" 136 12 h mtime;
    set_string        148 h "        "; (* Checksum *)
    set_string        156 h typeflag;
    set_string        257 h "ustar";
    set_string        263 h "00";
    set_octal "devmajor" 329 8 h 0;
    set_octal "devminor" 329 8 h 0;
    let c = header_checksum h in
    set_octal "checksum" 148 9 (* not NULL terminated *) h c;
    Ok (Bytes.unsafe_to_string h)
  with Failure msg -> R.error_msg msg

(* Files *)

let padding content = match String.length content mod 512  with
| 0 -> ""
| n -> Bytes.unsafe_to_string (Bytes.make (512 - n) '\x00')

let add t fname ~mode ~mtime kind =
  let typeflag, size, data = match kind with
  | `Dir -> "5", 0, []
  | `File cont -> "0", String.length cont, [cont; padding cont]
  in
  header fname mode mtime size typeflag
  >>| fun header -> List.rev_append data (header :: t)

(* Encode *)

let to_string t =
  let end_of_file = Bytes.unsafe_to_string (Bytes.make 1024 '\x00') in
  String.concat (List.rev (end_of_file :: t))

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
