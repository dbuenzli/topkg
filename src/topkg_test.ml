(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  { exec : Topkg_fpath.t;
    args : Topkg_cmd.t;
    run : bool;
    dir : Topkg_fpath.t option; }

let v exec ~args ~run ~dir = { exec; args; run; dir }
let exec t = t.exec
let args t = t.args
let run t = t.run
let dir t = t.dir
let codec =
  let fields =
    (fun t -> (t.exec, t.args, t.run, t.dir)),
    (fun (exec, args, run, dir) -> { exec; args; run; dir})
  in
  Topkg_codec.(view ~kind:"test" fields (t4 fpath cmd bool (option fpath)))
