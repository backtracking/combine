(* ZDD *)


open Format

type unique = int

(* invariant:
 - elements increase when we descend in subtrees
 - the right subtree of a ZDD is never Bottom *)
type zdd = Bottom | Top | Node of unique * int * zdd * zdd
type t = zdd

module S = Set.Make(
  struct
    type t = int 
    let compare = compare
  end
  )
type elt = S.t

let bottom = Bottom
let empty = bottom
let is_empty zdd = zdd == Bottom

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

module type ARITH = sig
  type t
  val zero: t
  val one: t
  val add: t -> t -> t
end

module Cardinal(A: ARITH) : sig val cardinal: zdd -> A.t end = struct

  let cardinal zdd = 
    let table = H1.create hsize in
    let rec cardinal = function
      | Top -> A.one
      | Bottom -> A.zero
      | Node(_, i, z1, z2) -> A.add (memo z1) (memo z2)
    and memo z =
      try
        H1.find table z
      with Not_found ->
        let c = cardinal z in
        H1.add table z c;
        c
    in 
    cardinal zdd

end

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


let split z = failwith "not yet implemented"
let max_elt z = failwith "not yet implemented"
let min_elt z = failwith "not yet implemented"
let partition z = failwith "not yet implemented"
let fold z = failwith "not yet implemented"
let subset z = failwith "not yet implemented"
let diff z = failwith "not yet implemented"




let equal z1 z2 = 
  match z1, z2 with
    | Top, Top 
    | Bottom, Bottom -> true
    | Node(u1, _,_,_), Node(u2, _,_,_) when u1 = u2 -> true
    | _ -> false

let compare z = failwith "not yet implemented"

let union z1 z2 = 
  let table = H2.create hsize in
  let rec union = function
    | Node(_, i, z1, z2), Top
    | Top, Node(_, i, z1, z2) -> construct i (memo (z1, Top)) z2
    | Top, Top -> Top
    | Bottom, z
    | z, Bottom -> z
    | (Node (_, i1, l1, r1) as z1), (Node (_, i2, l2, r2) as z2) ->
        if i1 = i2 then
          construct i1 (memo (l1, l2)) (memo (r1, r2))
        else if i1 > i2 then
          memo (z1, l2)
        else
          memo (z2, l1)

  and memo k =
    try
      H2.find table k
    with Not_found ->
      let r = union k in
      H2.add table k r;
      r
  in
  union (z1, z2)



let singleton s = 
  let elts = S.elements s in
  let rec singleton = 
    function
      | [] -> Top
      | h::t -> construct h Bottom (singleton t)
  in 
  singleton elts

let add s z = union (singleton s) z

let remove s z = 
  let elts = S.elements s in
  let rec remove elts z = 
    match elts, z with 
      | [], Top -> Bottom
      | [], Node (_, _ ,z1, _ ) -> remove [] z1
      | h::t, Node(_, i, z1, z2) -> 
          if i = h then begin 
            construct i z1 (remove t z2) end
          else if h > i then 
            construct i (remove elts z1) z2
          else z 
      | _ -> z
  in remove elts z

let mem s z = 
  let l = S.elements s in
  let rec mem l z = 
    match l, z with 
      | [], Top -> true
      | [], Node (_, _ ,z1, _ ) -> mem [] z1
      | h :: t, Node(_, i, z1, z2) -> 
          if h = i then mem t z2
          else if h > i then mem l z1
          else false
      | _ -> false
  in mem l z

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

let choose zdd = 
  let rec any_element zdd s = match zdd with 
    | Node (_, i, _, ct) -> any_element ct (S.add i s)
    | Top -> s
    | Bottom -> raise Not_found
  in 
  any_element zdd S.empty

let choose_list zdd = S.elements (choose zdd)

let iter_list f zdd = 
  let rec iter_element f l = function
    | Top -> f l
    | Bottom -> ()
    | Node (_, i, z1, z2)-> iter_element f l z1 ;
                            iter_element f (i :: l) z2 
  in 
  iter_element f [] zdd

let iter f zdd = 
  let rec iter_element f s = function
    | Top -> f s
    | Bottom -> ()
    | Node (_, i, z1, z2)-> iter_element f s z1 ;
                            iter_element f (S.add i s) z2 
  in 
  iter_element f S.empty zdd


let for_all f zdd = 
  try 
    iter (fun e -> if not (f e) then raise Exit) zdd;
    true
  with 
    | Exit -> false


let filter p zdd = 
  let z = ref Bottom in 
  iter (fun e -> if p e then z := add e !z) zdd;
  !z

let elements zdd = 
  let l = ref [] in 
  iter (fun e -> l := e :: !l) zdd;
  !l

let exists f zdd = 
  try 
    iter (fun e -> if f e then raise Exit) zdd;
    false
  with 
    | Exit -> true






