#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

(* Sample configuration file *)

module Config = struct
  include Config_default
  let vars =
    [ "NAME", "pkg";
      "VERSION", Git.describe ~chop_v:true "master";
      "MAINTAINER", "someone\\@example.org>" ]
end
