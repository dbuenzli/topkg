(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring
open Bos

type flavour = [ `Markdown | `Asciidoc ]

let flavour_of_fpath f = match String.Ascii.lowercase (Fpath.get_ext f) with
| ".md" -> Some `Markdown
| ".asciidoc" | ".adoc" -> Some `Asciidoc
| _ -> None

let rec drop_blanks = function "" :: ls -> drop_blanks ls | ls -> ls
let last_line = function [] -> None | l :: rev_ls -> Some l

(* Detecting headers *)

let simple_header hchar l before rest =
  match String.(length @@ take ~sat:(Char.equal hchar) l) with
  | 0 -> None
  | n -> Some (n, l, before, rest)

let underline_header n uchar l before rest =
  let is_underline_header uchar l =
    String.(length @@ take ~sat:(Char.equal uchar) l) >= 2
  in
  if not (is_underline_header uchar l) then None else
  match last_line before with
  | None -> None
  | Some t -> Some (n, strf "%s\n%s" t l, List.tl before, rest)

let rec find_markdown_header before = function
| [] -> None
| l :: ls ->
    match simple_header '#' l before ls with
    | Some _ as h -> h
    | None ->
        match underline_header 1 '=' l before ls with
        | Some _ as h -> h
        | None ->
            match underline_header 2 '-' l before ls with
            | Some _ as h -> h
            | None -> find_markdown_header (l :: before) ls

let rec find_asciidoc_header before = function
| [] -> None
| l :: ls ->
    match simple_header '=' l before ls with
    | Some _ as h -> h
    | None ->
        match underline_header 1 '-' l before ls with
        | Some _ as h -> h
        | None ->
            match underline_header 2 '~' l before ls with
            | Some _ as h -> h
            | None ->
                match underline_header 3 '^' l before ls with
                | Some _ as h -> h
                | None ->
                    match underline_header 4 '+' l before ls with
                    | Some _ as h -> h
                    | None -> find_asciidoc_header (l :: before) ls

let head find_header text =
  let lines = String.cuts ~sep:"\n" text in
  let ret h acc =
    let contents = String.concat ~sep:"\n" (List.rev @@ drop_blanks acc) in
    Some (h, contents)
  in
  match find_header [] lines with
  | None -> None
  | Some (n, first, _ (* discard *), rest) ->
      let rec loop acc rest = match find_header acc rest with
      | None -> ret first (List.rev_append rest acc)
      | Some (n', h, before, rest) ->
          if n' < n then loop (h :: before) rest else
          ret first before
      in
      loop [] rest

let head ?(flavour = `Markdown) text = match flavour with
| `Markdown -> head find_markdown_header text
| `Asciidoc -> head find_asciidoc_header text

let header_title ?(flavour = `Markdown) h = match String.cuts ~sep:"\n" h with
| [h] ->
    begin match flavour with
    | `Markdown -> String.(trim @@ drop ~sat:(Char.equal '#') h)
    | `Asciidoc -> String.(trim @@ drop ~sat:(Char.equal '=') h)
    end
| h :: _  -> h (* underline headers *)
| [] -> assert false

(* Toy URI parsing *)

let split_uri ?(rel = false) uri = match String.(cut ~sep:"//" (trim uri)) with
    | None -> None
    | Some (scheme, rest) ->
        match String.cut ~sep:"/" rest with
        | None -> Some (scheme, rest, "")
        | Some (host, path) ->
            let path = if rel then path else "/" ^ path in
            Some (scheme, host, path)

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
