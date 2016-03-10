(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let find_latest_version_tag r =
  let rev_compare v v' = -1 * compare v v' in
  let parse_tag acc t = match Topkg.String.parse_version t with
  | None -> acc
  | Some v -> (v, t) :: acc
  in
  Topkg.Vcs.tags r >>| fun tags ->
  match List.(sort rev_compare (fold_left parse_tag [] tags)) with
  | (_, latest) :: _ -> Some latest
  | [] -> None

let find_after r = function
| Some after -> Ok after
| None ->
    find_latest_version_tag r >>| function
    | None ->
        Logs.info (fun m -> m "No VCS version tag found."); ""
    | Some tag ->
        Logs.info (fun m -> m "Latest VCS version tag found: %s" tag); tag

let pp_version ppf = function
| "" -> ()
| v -> Fmt.pf ppf " since %a" Fmt.(styled `Cyan string) v

let pp_dirty ppf = function
| false -> ()
| true -> Fmt.pf ppf "The repository is %a.@," Fmt.(styled `Red string) "dirty"

let pp_commit ppf (id, log) =
  Fmt.(pf ppf "%a %s" (styled `Yellow string) id log)

let pp_status ppf (dirty, version, changes) = match changes with
| [] when not dirty ->
    Fmt.pf ppf "@[<v>No changes%a@]" pp_version version
| changes ->
    Fmt.pf ppf "@[<v>Changes%a:@,%a%a@]"
      pp_version version pp_dirty dirty (Fmt.list pp_commit) changes

let find_until ~pkg_file until = match until with
| Some until -> Ok until
| None ->
    R.join (Topkg_care.Ipc.ask ~pkg_file Topkg.Private.Ipc.distrib_commit_ish)

let status () pkg_file after until =
  begin
    Topkg.Vcs.get ()
    >>= fun repo -> Topkg.Vcs.is_dirty repo
    >>= fun dirty -> find_after repo after
    >>= fun after -> find_until ~pkg_file until
    >>= fun until -> Topkg.Vcs.changes repo ~after ~until
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
  let doc = "Commit-ish $(docv) until which commits are considered. If
             absent provided by the package's distribution commit
             determination function."
  in
  Arg.(value & opt (some string) None & info ["until"] ~doc ~docv:"COMMIT-ISH")

let doc = "list commits to publish in the next distribution"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) command consults the package's VCS and outputs the
        list of commits that define the changes for the next distribution.";
  ] @ Cli.common_opts_man @ [
    `S "EXIT STATUS";
    `P "The $(b,$(tname)) command exits with one of the following values:";
    `I ("0", "changes have been detected.");
    `I ("1", "no changes have been detected.");
    `I (">1", "an error occured.");
    `S "ENVIRONMENT VARIABLES";
  ] @ Cli.see_also ~cmds:[]

let cmd =
  let info = Term.info "status" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure status $ Cli.setup $ Cli.pkg_file $ after $ until) in
  (t, info)

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
