(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string

let dir_sep_prefix s =
  Topkg_string.is_prefix ~affix:Filename.dir_sep s ||
  (String.length s > 0 && s.[0] = '/')

let dir_sep_suffix s =
  Topkg_string.is_suffix ~affix:Filename.dir_sep s ||
  (String.length s > 0 && s.[String.length s - 1] = '/')

let append =
  fun p q -> match p with
  | "" -> q
  | p ->
      match q with
      | "" -> p
      | q ->
          if dir_sep_prefix q then q else
          if dir_sep_suffix p then (p ^ q) else
          (p ^ "/" ^ q)

let ( // ) = append

let is_dir_path p = match p with
| "." | ".." -> true
| _ ->
    let is_suffix affix = Topkg_string.is_suffix ~affix p in
    List.exists is_suffix ["/"; "/.."; "/."]

let is_file_path p = not (is_dir_path p)

let basename s = Filename.basename s
let dirname s = Filename.dirname s

let last_dot_index s = try Some (String.rindex s '.') with Not_found -> None
let get_ext s = match last_dot_index s with
| None -> ""
| Some i -> Topkg_string.with_index_range ~first:i s

let has_ext e p = Topkg_string.is_suffix ~affix:e p

let rem_ext s = match last_dot_index s with
| None -> s
| Some i -> Topkg_string.with_index_range ~last:(i - 1) s
