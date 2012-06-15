(* Test Module *)

open Dlx
open Zdd
open Tiling
open Format


let r = Lexer.parse_file "tests/non-regression.rem"
let problems = Interp.interp r
let p = List.hd problems
  
(* sets of sets of integers *)
module SS = struct
  include Set.Make(Zdd.S)
  let of_zdd z =
    List.fold_left (fun ss s -> add s ss) empty (Zdd.elements z)
  let to_zdd ss =
    fold Zdd.add ss Zdd.bottom
  (* n sets of <=k elements each, values in 0..m-1 *)
  let random n k m =
    let random_set () =
      let s = ref Zdd.S.empty in
      for i = 1 to Random.int (k+1) do s := Zdd.S.add (Random.int m) !s done;
      !s
    in
    let ss = ref empty in
    for i = 1 to n do ss := add (random_set ()) !ss done; !ss
  let print_set fmt s =
    fprintf fmt "{";
    Zdd.S.iter (fun x -> fprintf fmt "%d,@ " x) s; fprintf fmt "}"
  let print fmt ss =
    fprintf fmt "{"; iter (fun s -> fprintf fmt "%a,@ " print_set s) ss;
    fprintf fmt "}"
end

(* union *)
let () =
  for n = 0 to 10 do for k = 0 to 10 do
      let ss1 = SS.random n k 20 in
      let z1 = SS.to_zdd ss1 in
      let ss2 = SS.random n k 20 in
      let z2 = SS.to_zdd ss2 in
      assert (SS.equal (SS.union ss1 ss2) (SS.of_zdd (Zdd.union z1 z2)));
      assert (SS.equal (SS.inter ss1 ss2) (SS.of_zdd (Zdd.inter z1 z2)));
      (*assert (SS.equal (SS.diff  ss1 ss2) (SS.of_zdd (Zdd.diff  z1 z2)));*)
    done done

let to_set l =
  let s = ref Zdd.S.empty in 
  List.iter (fun e -> s := Zdd.S.add e !s) l;
  !s

let () =
  let z1 = construct 1 bottom top in
  let z2 = construct 2 bottom top in
  assert (mem (to_set [1]) z1);
  assert (mem (to_set [2]) z2);
  assert (mem (to_set [1]) (union z1 z2));
  assert (mem (to_set [2]) (union z1 z2))

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
  let primary, m = Tiling.emc p in
  assert (Emc.Z.count_solutions (Emc.Z.create ~primary m) = 12988816) ;
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

