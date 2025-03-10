(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bos_setup

let pp_since ppf = function
| "" -> ()
| v -> Fmt.pf ppf " since %a" Topkg_care.Pp.version v

let pp_dirty ppf = function
| false -> ()
| true -> Fmt.pf ppf "The repository is %a.@," Topkg_care.Pp.dirty ()

let pp_commit ppf (id, log) =
  Fmt.pf ppf "%a %s" Topkg_care.Pp.commit id log

let pp_status ppf (dirty, version, changes) = match changes with
| [] when not dirty -> Fmt.pf ppf "@[<v>No changes%a@]" pp_since version
| changes ->
    Fmt.pf ppf "@[<v>Changes%a:@,%a%a@]"
      pp_since version pp_dirty dirty (Fmt.list pp_commit) changes

let find_latest_version_tag repo =
  let rev_compare v v' = -1 * compare v v' in
  let parse_tag acc t = match Topkg.String.parse_version t with
  | None -> acc
  | Some v -> (v, t) :: acc
  in
  Topkg.Vcs.tags repo >>| fun tags ->
  match List.(sort rev_compare (fold_left parse_tag [] tags)) with
  | (_, latest) :: _ -> Some latest
  | [] -> None

let find_after repo = function
| Some after -> Ok after
| None ->
    find_latest_version_tag repo >>| function
    | None ->
        Logs.info (fun m -> m "No VCS version tag found."); ""
    | Some tag ->
        Logs.info (fun m -> m "Latest VCS version tag found: %s" tag); tag

let status () _ after until =
  begin
    Topkg.Vcs.get ()
    >>= fun repo -> Topkg.Vcs.is_dirty repo
    >>= fun dirty -> find_after repo after
    >>= fun after -> Topkg.Vcs.changes repo ~after ~until
    >>= fun changes ->
    Logs.app (fun m -> m "%a" pp_status (dirty, after, changes));
    Ok (if dirty || changes <> [] then 0 else 1)
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let after =
  let doc = "Commit-ish $(docv) after which commits are considered.
             Default is the latest VCS version tag of the form [v]X.Y.Z[+info]."
  in
  Arg.(value & opt (some string) None & info ["after"] ~doc ~docv:"COMMIT-ISH")

let until =
  let doc = "Commit-ish $(docv) until which commits are considered." in
  let docv = "COMMIT-ISH" in
  Arg.(value & opt string "HEAD" & info ["until"] ~doc ~docv)

let doc = "List commits to publish in the next distribution"
let sdocs = Manpage.s_common_options
let exits =
  (Cmd.Exit.info 0 ~doc:"changes have been detected.") ::
  (Cmd.Exit.info 1 ~doc:"no changes have been detected.") ::
  Cmd.Exit.defaults

let man_xrefs = [ `Main ]
let man =
  [ `S Manpage.s_description;
    `P "The $(tname) command consults the package's VCS and outputs the
        list of commits that define the changes for the next distribution." ]

let cmd =
  Cmd.v (Cmd.info "status" ~doc ~sdocs ~exits ~man ~man_xrefs) @@
  Term.(const status $ Cli.setup $ Cli.pkg_file $ after $ until)
