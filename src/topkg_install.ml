(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type file = string * Topkg_fexts.ext
type field_move =
  { field : Topkg_opam.Install.field;
    built : bool;
    src : file;
    dst : file; }

type t = field_move list

type field =
  ?built:bool -> ?cond:bool -> ?exts:Topkg_fexts.t -> ?dst:string ->
  string -> t

let to_file s = match Topkg_string.cut ~rev:true s ~sep:'.' with
| None -> s, `Ext ""
| Some (name, ext) -> name, `Ext (Topkg_string.strf ".%s" ext)

let mvs
    ?(drop_exts = []) field ?(built = true) ?(cond = true) ?(exts = [])
    ?dst src
  =
  if not cond then [] else
  let mv src dst = { field; built; src; dst } in
  let expand exts s d = List.map (fun e -> mv (s, e) (d, e)) exts in
  let dst = match dst with None -> Filename.basename src | Some dst -> dst in
  let files =
    if exts = [] then [mv (to_file src) (to_file dst)] else
    expand exts src dst
  in
  let has_ext (_, ext) ext' = ext = ext' in
  let keep { src; _ } = not (List.exists (has_ext src) drop_exts) in
  List.find_all keep files

let lib =
  let drop_exts =
    (* TODO *)
    let c = Topkg_conf.OCaml.v Topkg_conf.empty `Host_os in
    let native = Topkg_conf.OCaml.native c in
    let native_dynlink = Topkg_conf.OCaml.native_dynlink c in
    if native && not native_dynlink then Topkg_fexts.ext ".cmxs" else
    if native then [] else
    Topkg_fexts.(c_library @ exts [".cmx"; ".cmxa"; ".cmxs"])
  in
  mvs ~drop_exts `Lib

let share = mvs `Share
let share_root = mvs `Share_root
let etc = mvs `Etc
let toplevel = mvs `Toplevel
let doc = mvs `Doc
let misc = mvs `Misc
let stublibs = mvs `Stublibs
let man = mvs `Man

let bin_drops =
  (* TODO *)
  if not Topkg_conf.OCaml.(native (v Topkg_conf.empty `Host_os))
  then Topkg_fexts.ext ".native" else []

let bin_mvs
    field ?(auto = false) ?built ?cond ?(exts = Topkg_fexts.exe) ?dst src
  =
  let src, dst =
    if not auto then src, dst else
    let dst = match dst with
    | None -> Some (Filename.basename src)
    | Some _ as dst -> dst
    in
    let src =
      (* TODO *)
      if Topkg_conf.OCaml.(native (v Topkg_conf.empty `Host_os))
      then src ^ ".native" else src ^ ".byte"
      in
      src, dst
  in
  mvs ~drop_exts:bin_drops field ?built ?cond ~exts ?dst  src

let bin = bin_mvs `Bin
let sbin = bin_mvs `Sbin
let libexec = bin_mvs `Libexec

let parse_mllib contents =
  let lines = Topkg_string.cuts ~sep:'\n' contents in
  let add_mod acc l =
    let m = String.trim @@ match Topkg_string.cut ~sep:'#' l with
    | None -> l
    | Some (m, _ (* comment *)) -> m
    in
    if m = "" then acc else m :: acc
  in
  List.fold_left add_mod [] lines

let mllib ?(field = lib) ?(cond = true) ?api ?dst_dir mllib =
  if not cond then [] else
  let lib_dir = Topkg_fpath.dirname mllib in
  let lib_base = Topkg_fpath.rem_ext mllib in
  let dst f = match dst_dir with
  | None -> None
  | Some dir -> Some (Topkg_fpath.append dir (Topkg_fpath.basename f))
  in
  let api mllib_mods = match api with
  | None -> mllib_mods
  | Some api ->
      let in_mllib i = List.mem (Topkg_string.capitalize i) mllib_mods in
      let api, orphans = List.partition in_mllib api in
      let warn o =
        Topkg_log.warn (fun m -> m "mllib %s: unknown interface %s" mllib o)
      in
      List.iter warn orphans;
      api
  in
  let library = field ?dst:(dst lib_base) ~exts:Topkg_fexts.library lib_base in
  let add_mods acc mllib_mods =
    let api = api mllib_mods in
    let add_mod acc m =
      let fname = Topkg_fpath.append lib_dir (Topkg_string.lowercase m) in
      if List.mem m api
      then (field ?dst:(dst fname) ~exts:Topkg_fexts.api fname :: acc)
      else (field ?dst:(dst fname) ~exts:Topkg_fexts.cmx fname :: acc)
    in
    List.fold_left add_mod acc mllib_mods
  in
  begin
    Topkg_os.File.read mllib
    >>= fun contents -> Ok (parse_mllib contents)
    >>= fun mods -> Ok (List.flatten @@ add_mods [library] mods)
  end
  |> Topkg_log.on_error_msg ~use:(fun () -> [])

let to_instructions ?header c os i =
(*
  let native = Topkg_conf_ocaml.native c in
  let native_dynlink = Topkg_conf_ocam.native_dynlink c in
*)
  let bdir = Topkg_conf.build_dir c in
  let ocaml_conf = Topkg_conf.OCaml.v c os in
  let ext_to_string = Topkg_fexts.ext_to_string ocaml_conf in
  let maybe_build = [ ".cmti"; ".cmt" ] in
  let file_to_str ?(build_target = false) (n, ext) =
    let ext = match ext with
    (* Work around https://github.com/ocaml/ocamlbuild/issues/6 *)
    | `Exe when build_target -> `Ext ""
    | _ -> ext
    in
    Topkg_string.strf "%s%s" n (ext_to_string ext)
  in
  let add_instruction (targets, moves) { field; built; src; dst; } =
    let src = file_to_str ~build_target:true src in
    let dst = file_to_str dst in
    let maybe = List.exists (Filename.check_suffix src) maybe_build in
    let targets = if built && not maybe then src :: targets else targets in
    let src = if built then Topkg_string.strf "%s/%s" bdir src else src in
    let move = (field, Topkg_opam.Install.move ~maybe src ~dst) in
    (targets, move :: moves)
  in
  let targets, moves = List.fold_left add_instruction ([], []) i in
  targets, ((`Header header), moves)


let codec : t Topkg_codec.t = (* we don't care *)
  let fields = (fun _ -> ()), (fun () -> []) in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind:"install" fields unit)

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
