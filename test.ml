(* Test Module *)

open Dlx
open Tiling

let m1 = Array.create_matrix 2 2 true
let domino1 = create_piece [| [| true; true |] |]
let domino2 = create_piece [| [| true |]; [| true |] |]

let p1 = create_problem m1 [domino1; domino2]
let () = assert (count_solutions (emc p1) = 2)

let p =
  let c = open_in "tests/domino_8_8.rem" in
  let r = Parser.raw_parser c in
  close_in c;
  r
let () = assert (List.length p = 3)





