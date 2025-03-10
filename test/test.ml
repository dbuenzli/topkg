(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  let args = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  Printf.printf "The test is ok, the arguments are: %s\n" args
