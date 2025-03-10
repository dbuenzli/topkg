(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Topkg_result

type artefact = [`Distrib | `Doc | `Alt of string]
type t = { artefacts : artefact list }

let v ?(artefacts = [`Doc; `Distrib]) () = { artefacts }
let artefacts p = p.artefacts
let codec_artefact =
  let tag = function `Distrib -> 0 | `Doc -> 1 | `Alt _ -> 2 in
  let codecs =
    let alt_case =
      ((function `Alt s -> s | _ -> assert false),
       (function s -> `Alt s))
    in
    Topkg_codec.([| const `Distrib; const `Doc; view alt_case string |])
  in
  Topkg_codec.alt ~kind:"artefact" tag codecs

let codec =
  let artefacts = Topkg_codec.(list codec_artefact) in
  let fields =
    (fun p -> p.artefacts),
    (fun artefacts -> { artefacts })
  in
  Topkg_codec.version 0 @@
  Topkg_codec.(view ~kind:"publish" fields artefacts)
