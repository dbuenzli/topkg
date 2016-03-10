(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

(* Ustar archives *)

let path_set_of_dir dir ~exclude_paths =
  try
    let not_excluded =
      let add_excl acc p = match Fpath.of_string p with
      | Error (`Msg msg) -> failwith msg
      | Ok p -> Fpath.Set.add Fpath.(dir // p) acc
      in
      let excludes = List.fold_left add_excl Fpath.Set.empty exclude_paths in
      fun p -> Ok (not (Fpath.Set.mem p excludes))
    in
    let traverse = `Sat not_excluded in
    let elements = `Sat not_excluded in
    let err _ e = e in
    OS.Dir.fold_contents ~dotfiles:true ~err ~elements ~traverse
      Fpath.Set.add Fpath.Set.empty dir
  with
  | Failure msg -> R.error_msg msg

let tar dir ~exclude_paths ~root ~mtime =
  let tar_add file tar =
    let fname = match Fpath.rem_prefix dir file with
    | None -> assert false
    | Some file -> Fpath.(root // file)
    in
    Logs.info (fun m -> m "Archiving %a" Fpath.pp fname);
    tar
    >>= fun tar -> OS.Path.Mode.get file
    >>= fun mode -> OS.Dir.exists file
    >>= function
    | true -> Topkg_care_tar.add tar fname ~mode ~mtime `Dir
    | false ->
        OS.File.read file
        >>= fun cont -> Topkg_care_tar.add tar fname ~mode ~mtime (`File cont)
  in
  path_set_of_dir dir ~exclude_paths
  >>= fun fset -> Fpath.Set.fold tar_add fset (Ok Topkg_care_tar.empty)
  >>| fun tar -> Topkg_care_tar.to_string tar

(* Bzip2 *)

let bzip2 = OS.Env.(value "TOPKG_BZIP2" cmd ~absent:(Cmd.v "bzip2"))
let ensure_bzip2 () = OS.Cmd.must_exist bzip2 >>| fun _ -> ()
let bzip2 s ~dst = OS.Cmd.(in_string s |> run_io bzip2 |> to_file dst)

(* Distribution *)

type det =
  { build_dir : Fpath.t;
    name : string;
    commit_ish : Topkg.Vcs.commit_ish;
    version : string }

let build_dir det = det.build_dir
let name det = det.name
let commit_ish det = det.commit_ish
let version det = det.version

let rootname ?(opam = false) d =
  let version = match String.head d.version with
  | Some 'v' -> String.with_range ~first:1 d.version
  | _ -> d.version
  in
  let sep = if opam then '.' else '-' in
  Fpath.v (strf "%s%c%s" d.name sep version)

let determine ~pkg_file ~build_dir ~name ~commit_ish ~version =
  let det = match build_dir, name, commit_ish, version with
  | Some bdir, Some name, Some commit_ish, Some version ->
      Ok (bdir, name, commit_ish, version)
  | _ ->
      R.join (Topkg_care_ipc.ask ~pkg_file @@
              Topkg.Private.Ipc.distrib_determine
                ~build_dir ~name ~commit_ish ~version)
  in
  det
  >>= fun (bdir, name, commit_ish, version) -> Fpath.of_string bdir
  >>= fun build_dir -> Fpath.of_string (strf "%s-%s" name version)
  >>= fun _ (* just check this yielded a valid path *) ->
  Ok { build_dir; name; commit_ish; version }

let clone_repo det repo =
  let dir = Fpath.(det.build_dir // rootname det + ".build") in
  OS.Dir.create det.build_dir
  >>= fun _ -> OS.Dir.exists dir
  >>= fun ex -> (if ex then OS.Dir.delete ~recurse:true dir else Ok ())
  >>= fun () -> Topkg.Vcs.clone repo ~dir:(Fpath.to_string dir)
  >>= fun () -> Ok dir

let prepare_repo det ~dist_pkg_file ~dir =
  let prepare () =
    let name = det.name in
    let version = det.version in
    let commit_ish =  det.commit_ish in
    let branch = strf "topkg-dist-%s" version in
    let prepare = Topkg.Private.Ipc.distrib_prepare ~name ~version in
    Topkg.Vcs.get ()
    >>= fun repo -> Topkg.Vcs.checkout repo ~branch ~commit_ish
    >>= fun () -> Topkg.Vcs.commit_ptime_s repo ~commit_ish
    >>= fun mtime -> R.join (Topkg_care_ipc.ask ~pkg_file:dist_pkg_file prepare)
    >>= fun excludes -> Ok (mtime, excludes)
  in
  OS.Dir.with_current dir prepare ()

let archive_path det = Fpath.(det.build_dir // rootname det + ".tbz")
let archive det ~keep_dir ~dir ~exclude_paths ~mtime =
  let archive = archive_path det in
  tar dir ~exclude_paths ~root:(rootname det) ~mtime
  >>= fun tar -> bzip2 tar ~dst:archive
  >>= fun () -> (if keep_dir then Ok () else OS.Dir.delete ~recurse:true dir)
  >>= fun () -> Ok archive

let tar_cmd = OS.Env.(value "TOPKG_TAR" cmd ~absent:(Cmd.v "tar"))
let unarchive ?(clean = false) ar =
  let clean_dir dir =
    OS.Dir.exists dir >>= function
    | true when clean -> OS.Dir.delete ~recurse:true dir
    | _ -> Ok ()
  in
  let archive_dir, ar = Fpath.split_base ar in
  let unarchive ar =
    let dir = Fpath.rem_ext ar in
    OS.Cmd.must_exist tar_cmd
    >>= fun cmd -> clean_dir dir
    >>= fun () -> OS.Cmd.run Cmd.(tar_cmd % "-xjf" % p ar)
    >>= fun () -> Ok Fpath.(archive_dir // dir)
  in
  OS.Dir.with_current archive_dir unarchive ar

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
