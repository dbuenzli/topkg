#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default
  let vars =
    [ "NAME", "topkg";
      "VERSION", Git.describe ~chop_v:true "master";
      "PKG_WWW", "http://erratique.ch/software/topkg" ]
end
