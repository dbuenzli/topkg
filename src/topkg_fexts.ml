(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type ext = [ `Ext of string | `Obj | `Real_clib | `Lib | `Dll | `Exe ]
type t = ext list

let interface = [ `Ext ".mli"; `Ext ".cmi"; `Ext ".cmti"; ]
let cmx = [ `Ext ".cmx" ]
let api = interface @ cmx
let real_c_library = [ `Real_clib ]
let c_library = [ `Lib ]
let c_dll_library = [ `Dll ]
let library = [` Ext ".cma"; `Ext ".cmxa"; `Ext ".cmxs" ] @ c_library
let module_library = (api @ library)
let exe = [ `Exe ]
let ext e = [ `Ext e ]
let exts es = List.map (fun e -> `Ext e) es

let ext_to_string c =
  let ext_obj = Topkg_conf.OCaml.ext_obj c in
  let ext_lib = Topkg_conf.OCaml.ext_lib c in
  let ext_dll = Topkg_conf.OCaml.ext_dll c in
  let ext_exe = Topkg_conf.OCaml.ext_exe c in
  function
  | `Ext s -> s
  | `Obj -> ext_obj
  | `Lib | `Real_clib -> ext_lib
  | `Dll -> ext_dll
  | `Exe -> ext_exe
