(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

(* Tool lookup *)

type os = [ `Build_os | `Host_os ]

let os_to_string = function
| `Build_os -> "build-os"
| `Host_os -> "host-os"

let os_tool_env name os =
  let pre = match os with `Build_os -> "BUILD_OS_" | `Host_os -> "HOST_OS_" in
  pre ^ String.uppercase name

let os_bin_dir_env = function
| `Build_os -> "BUILD_OS_BIN"
| `Host_os -> "HOST_OS_XBIN"

let os_suff_env = function
| `Build_os -> "BUILD_OS_SUFF"
| `Host_os -> "HOST_OS_SUFF"

let tool name os =
  let tool = match Topkg_os.Env.var (os_tool_env name os) with
  | Some bin -> bin
  | None ->
      match Topkg_os.Env.var (os_bin_dir_env os) with
      | Some path -> Topkg_fpath.(path // name)
      | None ->
          match Topkg_os.Env.var (os_suff_env os) with
          | Some suff -> name ^ suff
          | None -> name
  in
  Topkg_cmd.v tool

(* OCaml configuration *)

module OCaml = struct

  (* Log strings *)

  let conf fmt = "OCaml %s conf: " ^^ fmt
  let conf_key fmt = conf ("key %s: " ^^ fmt)

  (* Configuration *)

  type t =
    { os : os; mutable conf : (string * string) list;
      (* Mutability is only used to add the value found by a discover
         procedure for keys that are not yet exposed in ocamlc -config.
         See http://caml.inria.fr/mantis/view.php?id=7172 *) }

  let empty os = { os; conf = [] }

  let read_config os =
    let parse_line acc l = match Topkg_string.cut ~sep:':' l with
    | Some (k, v) -> (k, String.trim v) :: acc
    | None ->
        Topkg_log.warn begin fun m ->
          m "OCaml %s conf: cannot parse line %S" (os_to_string os) l;
        end;
        acc
    in
    begin
      let ocamlc = tool "ocamlc" os in
      Topkg_os.Cmd.(run_out Topkg_cmd.(ocamlc % "-config") |> to_lines)
      >>= fun lines ->
      let conf = List.(rev (fold_left parse_line [] lines)) in
      Ok { os; conf }
    end
    |> R.reword_error_msg ~replace:true
      (fun msg -> R.msgf (conf " %s") (os_to_string os) msg)
    |> Topkg_log.on_error_msg ~level:Topkg_log.Warning ~use:(fun () -> empty os)

  let host_os = lazy (read_config `Host_os)
  let build_os = lazy (read_config `Build_os)
  let v = function
  | `Host_os -> Lazy.force host_os
  | `Build_os -> Lazy.force build_os

  let add_discovery k v c = c.conf <- (k, v) :: c.conf
  let find k c = try Some (List.assoc k c.conf) with Not_found -> None
  let get ~absent k c = match find k c with
  | Some v -> v
  | None ->
      Topkg_log.warn begin fun m ->
        m (conf_key "undefined, using %S") (os_to_string c.os) k absent
      end;
      absent

  let get_string_with_discovery k c ~discover = match find k c with
  | Some v -> v
  | None -> let v = discover k c in add_discovery k v c; v

  let get_bool_with_discovery k c ~discover =
    let maybe_v = match find k c with
    | None -> None
    | Some v ->
        try Some (bool_of_string v) with
        | (* That good old joke... *) Invalid_argument _ ->
            Topkg_log.warn begin fun m ->
              m (conf_key "could not parse boolean,@ trying to discover")
                (os_to_string c.os) k
            end;
            None
    in
    match maybe_v with
    | Some v -> v
    | None -> let v = discover k c in add_discovery k (string_of_bool v) c; v

  let get_bool_stdlib_file_exists_discovery k c ~file ~on_error =
    get_bool_with_discovery k c ~discover:begin fun k c ->
      match find "standard_library" c with
      | None ->
          Topkg_log.warn begin fun m ->
            m (conf_key
                 "undefined, stdlib dir not found for discovery@ using %B")
              (os_to_string c.os) k on_error
          end;
          on_error
      | Some stdlib_dir ->
          match Topkg_os.File.exists (Topkg_fpath.(stdlib_dir // file c)) with
          | Ok exist -> exist
          | Error (`Msg e) ->
              Topkg_log.warn begin fun m ->
                m (conf_key "undefined,@ discovery error: %s,@ using %B")
                  (os_to_string c.os) k e on_error
              end;
              on_error
    end

  let version c = (* parses the specification described in Sys.ocaml_version *)
    let dumb_version = 0, 0, 0, None in
    let k = "version" in
    match find k c with
    | None ->
        Topkg_log.warn begin fun m ->
          m (conf_key "missing, using 0.0.0") (os_to_string c.os) k
      end;
        dumb_version
    | Some version ->
        match Topkg_string.parse_version version with
        | Some version -> version
        | None ->
            Topkg_log.warn begin fun m ->
              m (conf_key "cannot parse from %S, using 0.0.0")
                (os_to_string c.os) k version
            end;
            dumb_version

  let ext_obj c = get ~absent:".o" "ext_obj" c
  let ext_asm c = get ~absent:".s" "ext_asm" c
  let ext_lib c = get ~absent:".a" "ext_lib" c
  let ext_dll c = get ~absent:".so" "ext_dll" c
  let ext_exe c =
    get_string_with_discovery "ext_exe" c ~discover:begin fun k c ->
      (* Not exposed until at least 4.03. The discover logic is based on
         the knowledge articulated in this message:
         http://lists.ocaml.org/pipermail/wg-windows/2015-July/000037.html *)
      let find_c_toolchain c = match find "ccomp_type" c, find "os_type" c with
      | None, _  | _, None -> None
      | Some ccomp_type, Some os_type ->
          match ccomp_type, os_type with
          | "msvc", _  -> Some `Win_msvc
          | "cc", "Win32" -> Some `Win_cc
          | _, _  -> Some `Other
      in
      match find_c_toolchain c with
      | Some (`Win_msvc | `Win_cc) -> ".exe"
      | Some `Other -> ""
      | None ->
          Topkg_log.warn begin fun m ->
            m (conf_key "undefined and@ no C toolchain@ detected,@ using \"\"")
              (os_to_string c.os) k;
          end;
          ""
    end

  let native c =
    let file c = "libasmrun" ^ (ext_lib c) in
    get_bool_stdlib_file_exists_discovery "native" c ~file ~on_error:false

  let native_dynlink c =
    let file _ = "dynlink.cmxa" in
    get_bool_stdlib_file_exists_discovery "natdynlink" c ~file ~on_error:false

  let dump ppf c =
    let pp_elt ppf (k, v) = Format.fprintf ppf "(%S, %S)" k v in
    let rec loop = function
    | [] -> ()
    | v :: vs ->
        if vs = [] then (Format.fprintf ppf "@[%a@]" pp_elt v) else
        (Format.fprintf ppf "@[%a@];@ " pp_elt v; loop vs)
    in
    Format.fprintf ppf "@[<1>["; loop c.conf; Format.fprintf ppf "]@]"
end

(* TODO cleanup *)

(* Parses the command line. The actual cmd execution occurs in the call
     to Pkg.describe. *)

let err ?(stop = true) fmt =
  let k _ = if stop then exit 1 else () in
  Format.kfprintf k Format.err_formatter ("%s: " ^^ fmt ^^ "@.") Sys.argv.(0)

let err_parse a = err "value of `%s' is not 'true' or 'false'" a
let err_mdef a = err "bool `%s' is defined more than once" a
let err_miss a = err ~stop:false "boolean key '%s' is missing" a
let warn_unused k = err ~stop:false "warning: environment key `%s` unused" k

let cmd, env =
  let rec parse_env acc = function
  | key :: def :: defs ->
        begin try ignore (List.assoc key acc); err_mdef key; [] with
        | Not_found -> parse_env ((key, def) :: acc) defs
        end
  | [] -> (List.rev acc)
  | key :: [] -> acc (* err "key '%s' has no definition" key *)
  in
  match List.tl (Array.to_list Sys.argv) with
  | ("help" | "-h" | "--help" | "-help") :: args -> `Help, parse_env [] args
  | "build" :: args -> `Build, parse_env [] args
  | "ipc" :: verbosity :: _ as args ->
      ignore (Topkg_log.level_of_string verbosity
      >>= fun level -> Ok (Topkg_log.set_level level)); (* FIXME *)
      `Ipc (Topkg_cmd.of_list args), []
  | args -> `Unknown args, []

let kenv = ref []
let get () = !kenv
let add_bool key b = kenv := (key, (`Bool b)) :: !kenv
let add_string key s = kenv := (key, (`String s)) :: !kenv
let bool ?(quiet = false) ?(absent = fun () -> Ok true) key =
  let b = try bool_of_string (List.assoc key env) with
  | Not_found ->
      if cmd = `Build && not quiet then err_miss key;
      begin match absent () with
      | Ok v -> v
      | Error (`Msg e) ->
          Topkg_log.err (fun m ->
                m "error while determining value for key '%s':\n%s" key e);
          true
      end
  | Invalid_argument _ -> err_parse key; true
  in
  add_bool key b; b

let string ?(quiet = false) ?(absent = fun () -> Ok "undefined") key =
  let s = try List.assoc key env with
  | Not_found ->
      if cmd = `Build && not quiet then (err_miss key; "undefined") else
      begin match absent () with
      | Ok v -> v
      | Error (`Msg e) ->
          Topkg_log.err (fun m ->
              m "error while determining value for key '%s':\n%s" key e);
           "undefined"
      end
  in
  add_string key s; s

let warn_unused () =
  let keys = List.map fst env in
  let keys_used = List.map fst (get ()) in
  let unused = List.find_all (fun k -> not (List.mem k keys_used)) keys in
  List.iter warn_unused unused

(* Build context *)

let installer = bool ~absent:(fun () -> Ok false) "installer"
let vcs =
  let absent () = Topkg_vcs.find () >>= function vcs -> Ok (vcs <> None) in
  bool ~quiet:true ~absent "vcs"

let build =
  if not vcs then `Distrib else
  if installer then `Pin else
  `Dev

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
