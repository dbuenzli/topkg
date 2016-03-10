(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

(* Watermarks *)

type watermark_def =
[ `String of string
| `Name
| `Version
| `Vcs of [ `Commit_id ]
| `Opam of string * string * string ]

type watermark = string * watermark_def

let opam_fields file =
  (Topkg_opam.File.fields file)
  |> R.reword_error_msg ~replace:true
    (fun msg -> R.msgf "Watermarks: %s" msg)
  |> Topkg_log.on_error_msg ~level:Topkg_log.Warning
    ~use:(fun () -> [])

let opam_field =
  let find k m = try Some (List.assoc k m) with Not_found -> None in
  let opam_memo = ref [] in (* memoizes the opam files *)
  let rec get file field = match find file !opam_memo with
  | None ->
      opam_memo := (file, (opam_fields file)) :: !opam_memo;
      get file field
  | Some fields ->
      match find field fields with
      | Some vs -> vs
      | None ->
          Topkg_log.warn begin fun m ->
            m "file %s: opam field %S undefined or unsupported" file field
          end;
          ["UNDEFINED"]
  in
  get

let vcs_commit_id () =
  (Topkg_vcs.get () >>= fun r -> Topkg_vcs.head r)
  |> R.reword_error_msg ~replace:true
    (fun msg -> R.msgf "Watermarks: VCS commit id determination: %s" msg)
  |> Topkg_log.on_error_msg ~level:Topkg_log.Warning
    ~use:(fun () -> "UNDEFINED")

let define_watermarks ~name ~version watermarks =
  let define (id, v) =
    let (id, v as def) = match v with
    | `String s -> (id, s)
    | `Version -> (id, version)
    | `Name -> (id, name)
    | `Vcs `Commit_id -> (id, vcs_commit_id ())
    | `Opam (file, fld, sep) -> (id, String.concat sep (opam_field file fld))
    in
    Topkg_log.info (fun m -> m "Watermark %s = %S" id v);
    def
  in
  List.map define watermarks

let default_watermarks =
  let space = " " in
  let comma = ", " in
  [ "NAME", `Name;
    "VERSION", `Version;
    "VCS_COMMIT_ID", `Vcs `Commit_id;
    "PKG_MAINTAINER", `Opam ("opam", "maintainer", comma);
    "PKG_AUTHORS", `Opam ("opam", "authors", comma);
    "PKG_HOMEPAGE", `Opam ("opam", "homepage", comma);
    "PKG_ISSUES", `Opam ("opam", "bug-reports", space);
    "PKG_DOC", `Opam ("opam", "doc", space);
    "PKG_LICENSE", `Opam ("opam", "license", comma);
    "PKG_REPO", `Opam ("opam", "dev-repo", space); ]

let default_files_to_watermark =
  let is_binary_ext ext =
    let module Set = Set.Make (String) in
    let exts =
      Set.(empty |>
           add ".flv" |> add ".gif" |> add ".ico" |> add ".jpeg" |>
           add ".jpg" |> add ".mov" |> add ".mp3" |> add ".mp4" |>
           add ".otf" |> add ".pdf" |> add ".png" |> add ".ttf" |> add ".woff")
    in
    Set.mem ext exts
  in
  let not_binary f = not (is_binary_ext (Topkg_fpath.ext f)) in
  fun () ->
    Topkg_vcs.get ()
    >>= fun r -> Topkg_vcs.tracked_files r
    >>= fun files -> Ok (List.filter not_binary files)

let watermark_file ws file =
  Topkg_os.File.read file >>= fun content ->
  Topkg_os.File.write_subst file ws content >>= fun () ->
  Topkg_log.info (fun m -> m "Watermarked %s" file); Ok ()

let rec watermark_files ws = function
| [] -> Ok ()
| f :: fs -> watermark_file ws f >>= fun () -> watermark_files ws fs

(* Distribution *)

let default_commit_ish () =
  Topkg_vcs.get () >>= fun r -> Topkg_vcs.head ~dirty:false r

let default_version ~commit_ish =
  Topkg_vcs.get () >>= fun r -> Topkg_vcs.describe r ~dirty:true ~commit_ish

let default_exclude_paths =
  fun () -> Ok [".git"; ".gitignore"; "build"; "Makefile"; "_build"]

type t =
  { commit_ish : unit -> string result;
    version : commit_ish:string -> string result;
    watermarks : watermark list;
    files_to_watermark : unit -> Topkg_fpath.t list result;
    massage : unit -> unit result;
    exclude_paths : unit -> Topkg_fpath.t list result; }

let v
    ?(commit_ish = default_commit_ish) ?(version = default_version)
    ?(watermarks = default_watermarks)
    ?(files_to_watermark = default_files_to_watermark)
    ?(massage = fun () -> Ok ())
    ?(exclude_paths = default_exclude_paths)
    () =
  { commit_ish; version; watermarks; files_to_watermark; massage;
    exclude_paths }

let commit_ish d = d.commit_ish ()
let version d ~commit_ish = d.version commit_ish
let watermarks d = d.watermarks
let files_to_watermark d = d.files_to_watermark ()
let massage d = d.massage ()
let exclude_paths d = d.exclude_paths ()

let run_watermark ~name ~version d =
  files_to_watermark d >>= fun files ->
  let ws = define_watermarks ~name ~version (watermarks d) in
  watermark_files ws files

let run_prepare ~name ~version d =
  run_watermark ~name ~version d
  >>= fun () -> d.massage ()
  >>= fun () -> d.exclude_paths ()

let run_prepare_pin ~name d =
  version d ~commit_ish:"HEAD"
  >>= fun version -> run_watermark ~name ~version d
  >>= fun () -> d.massage ()

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
