(* Test Module *)

open Dlx
open Zdd
open Tiling
open Format


let r = Lexer.parse_file "tests/non-regression.rem"
let problems = Interp.interp r
let p = List.hd problems
  
  


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
  assert (Emc.Z.count_solutions 
    (Emc.Z.create (* ~primary:(Tiling.emc_primaries p)*) (Tiling.emc p)) =
      12988816) ;
  let emc = Queens.emc 5 in 
  let qp_zdd = (Emc.Z.create ~primary:(2 * 5) emc) in
  let qp_dlx = (Emc.D.create ~primary:(2 * 5) emc) in
  assert (Emc.Z.count_solutions qp_zdd = 10);
  assert (Emc.D.count_solutions qp_dlx = 10)


let p =
  Pattern.create
    [|
     [|true; true; true; true|];
     [|true; false; false; false|];
     [|true; true; true; false|];
     [|true; false; false; false|];
     [|true; false; false; false|];
    |]
let p =
  Pattern.create
    [|
     [|true; true;|];
     [|true; false;|];
    |]


(*

let () = printf "%a\n@." Pattern.print p

let () = printf "%a\n@." Pattern.print (Pattern.shift p 1 1)

let () = printf "%a\n@." Pattern.print (Pattern.union p (Pattern.shift p 1 1))

let () = printf "%a\n@." Pattern.print (Pattern.inter p (Pattern.shift p 1 1))

let () = printf "%a\n@." Pattern.print (Pattern.diff p (Pattern.shift p 1 1))
let () = printf "%a\n@." Pattern.print (Pattern.diff p p)

let t = Tile.create p

let () = printf "%a@." Tile.print t
*)

