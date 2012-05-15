(* Test Module *)

open Dlx
open Zdd
open Tiling
open Format

let m1 = Array.create_matrix 2 2 true
let domino1 = create_piece [| [| true; true |] |]
let domino2 = create_piece [| [| true |]; [| true |] |]

let p1 = create_problem m1 [domino1; domino2]
let () = assert (count_solutions (create (emc p1)) = 2)

let p =
  let c = open_in "tests/domino_8_8.rem" in
  let r = Parser.read_problem c in
  close_in c;
  Tiling.emc r


let to_set l =
  let s = ref Zdd.S.empty in 
  List.iter (fun e -> s := Zdd.S.add e !s) l;
  !s

let () = 
  let s1 = to_set [1; 2; 3] in
  let s2 = to_set [1; 2] in
  let z1 = Zdd.singleton s1 in 
  let z2 = Zdd.singleton s2 in 
  assert (Zdd.mem s1 z1);
  assert (Zdd.is_empty (Zdd.remove s1 z1));
  let z3 = Zdd.union z1 z2 in 
  assert (Zdd.mem s1 z3);
  assert (Zdd.mem s2 z3)

let () = 
  assert (Emc.Z.count_solutions (Emc.Z.create p) = 12988816) ;
  let emc = Queens.emc 5 in 
  let qp_zdd = (Emc.Z.create ~primary:(2 * 5) emc) in
  let qp_dlx = (Emc.D.create ~primary:(2 * 5) emc) in
  assert (Emc.Z.count_solutions qp_zdd = 10);
  assert (Emc.D.count_solutions qp_dlx = 10)





