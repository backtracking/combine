(* ZDD *)

type unique = int

(* invariant:
   - elements increase when we descend in subtrees
   - the right subtree of a ZDD is never Bottom *)
type t = Bottom | Top | Node of unique * int * t * t
type zdd = t

let bottom = Bottom
let top = Top

let unique = function
  | Bottom -> 0
  | Top -> 1
  | Node (u , _, _, _) -> u

module ZddHashtbl = 
  Hashtbl.Make 
    (struct 
       type t = zdd
       let hash = function
         | Bottom -> 0
         | Top -> 1
         | Node (_, i, z1, z2) ->
	     (19 * (19 * i + unique z1) + unique z2) land max_int
       let equal k1 k2 = 
         match k1, k2 with
           | Top, Top 
           | Bottom, Bottom -> true
           | Node (_, i1, l1, r1), Node (_, i2, l2, r2) ->
               i1 = i2 && unique l1 = unique l2 && unique r1 = unique r2
           | _ -> false
     end 
    )

let hsize = 19997 (* 200323 *)

let global_table = ZddHashtbl.create hsize

let unique_ref = ref 2

let construct i z1 z2 =
  if z2 = Bottom then z1 else
  let zdd = Node (!unique_ref, i, z1, z2) in 
  try
    ZddHashtbl.find global_table zdd 
  with Not_found ->
    ZddHashtbl.add global_table zdd zdd;
    incr unique_ref;
    zdd

(* z1 = z2 iff z1 == z2 iff unique z1 = unique z2 *)

module H1 = 
  Hashtbl.Make
    (struct
       type t = zdd
       let hash = unique
       let equal = (==)
     end)

let cardinal zdd = 
  let table = H1.create hsize in
  let rec cardinal = function
    | Top -> 1
    | Bottom -> 0
    | Node(_, i, z1, z2) -> memo z1 + memo z2
  and memo z =
    try
      H1.find table z
    with Not_found ->
      let c = cardinal z in
        H1.add table z c;
        c
  in 
    cardinal zdd

module H2 = 
  Hashtbl.Make 
    (struct 
       type t = zdd * zdd
       let hash (z1, z2) = (19 * unique z1 + unique z2) land max_int
       let equal (z11, z12) (z21, z22) = z11 == z21 && z12 == z22
     end) 


let inter z1 z2 =
  let table = H2.create hsize in
  let rec inter = function
    | Node(_, _, z1, _), Top
    | Top, Node(_, _, z1, _) -> memo (z1, Top)
    | Top, Top -> Top
    | Bottom, _
    | _, Bottom -> Bottom
    | (Node (u1, i1, l1, r1) as z1), (Node (u2, i2, l2, r2) as z2) ->
(*      if u1 = u2 then
        z1
      else *) if i1 = i2 then
        construct i1 (memo (l1, l2)) (memo (r1, r2))
      else if i1 > i2 then
        memo (z1, l2)
      else
        memo (z2, l1)
  and memo k =
    try
      H2.find table k
    with Not_found ->
      let r = inter k in
      H2.add table k r;
      r
  in
  inter (z1, z2)

let size z =
  let s = ref 0 in
  let h = H1.create 17 in
  let rec visit z =
    if not (H1.mem h z) then begin
      H1.add h z ();
      match z with
	| Bottom | Top -> ()
	| Node (_, _, l, r) -> incr s; visit l; visit r
    end
  in
  visit z;
  !s

(* DOT output *)

open Format

let print_to_dot fmt z =
  let h = H1.create 17 in (* ZDD -> node name *)
  let name = let r = ref 0 in fun () -> incr r; "N" ^ string_of_int !r in
  let rec visit = function
    | Bottom ->
        let s = name () in
	fprintf fmt "node [shape = none, label=\"Bot\"]; %s;@\n" s; s
    | Top ->
        let s = name () in
	fprintf fmt "node [shape = none, label=\"Top\"]; %s;@\n" s; s
    | Node (_, i, l, r) ->
        let nl = memo l in
	let nr = memo r in
	let n = name () in
	fprintf fmt "node [shape = circle, label=\"%d\"]; %s;@\n" i n;
	fprintf fmt "%s -> %s [style = dotted];@\n%s -> %s;@\n" n nl n nr;
	n
  and memo z = match z with
    | Bottom | Top -> visit z
    | _ -> try H1.find h z with Not_found -> let n = visit z in H1.add h z n; n
  in
  fprintf fmt "@[<hov 2>digraph ZDD {@\n";
  ignore (visit z);
  fprintf fmt "}@]@."

let column j m =
  let n = Array.length m in
  (* we build the solution from bottom up, i.e. i = n-1,...,1,0 *)
  let rec build z zf i =
    (* z  = exactly one i such that m[i][j]=true
       zf = only i such that m[i][j]=false *)
    if i < 0 then z
    else if m.(i).(j) then build (construct i z zf) zf (i-1)
    else build (construct i z z) (construct i zf zf) (i-1)
  in
  let r = build bottom top (n-1) in
  Format.printf "column %d: %d@." j (size r);
  r

(**
let column j m = 
  let length = Array.length m in 
  let rec column i exist =
    if i >= length then 
      if exist then Top
      else Bottom
    else if exist then 
      if m.(i).(j) then 
        column (i + 1) exist
      else 
        construct i (column (i + 1) true) (column (i + 1) true)
    else 
      if m.(i).(j) then  
        construct i (column (i + 1) false) (column (i + 1) true) 
      else 
        if i <> length - 1 then 
          construct i (column (i + 1) false) (column (i + 1) false) 
        else 
          Bottom
  in 
  let result = column 0 false in
    Format.printf "Debug : %d@." (cardinal result);
    result
***)

let tiling m =
  let width = Array.length m.(0) in
  let rec tiling j zdd_acc =
    printf " => %d@." (size zdd_acc);
    if j < 0 then
      zdd_acc
    else
      tiling (j-1) (inter (column j m) zdd_acc)
  in
  tiling (width-2) (column (width-1) m)


    (*
     let rec Top_elt zdd = 
     match zdd with 
     | Node(i, _, ct) -> i::Top_elt ct
     | _ -> []
       *)

    (*
     let rec singleton s = 
     match s with 
     | [] -> Top
     | h::t -> Node(h, Bottom, singleton t)
       *)


    (*
     let rec is_in s zdd = 
     match s, ct with 
     | [], Top -> true
     | h::t, Node(i, ct1, ct2) -> 
     if i = h then is_in t ct2
     else false
     | _, _ -> false
       *)





