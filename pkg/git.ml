#!/usr/bin/env ocaml 
#directory "pkg"
#use "config.ml" 

(* This is only for git checkout builds, it can be ignored
   for distribution builds. *)

let () = 
  if Dir.exists ".git" then begin
    Vars.substitute ~skip:Config.substitute_skip ~vars:Config.vars ~dir:".";
    match Config.hook_git with
    | None -> () | Some hook -> Cmd.(exec_ocaml hook |> exit_on_error)
  end

