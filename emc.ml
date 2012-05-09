(* EMC Module *)

open Format

type problem = bool array array

type solution = int list
(** a solution is a set of rows *)

module type S = sig
  val find_solution: problem -> solution
  val iter_solution: (solution -> unit) -> problem -> unit
  val simple_count_solutions: problem -> int
  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
  module Count(A: ARITH) : sig 
    val count_solutions: problem -> A.t 
  end
end


module D = struct
  let find_solution p = Dlx.get_first_solution p
  let iter_solution f p = Dlx.iter_solution (
    fun e -> f (Dlx.list_of_solution e)) p
  let simple_count_solutions = Dlx.count_solutions 
  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
  module Count = functor (A: ARITH) ->  
  struct
    let count_solutions p = 
      let r = ref A.zero in
        iter_solution (fun _ -> r:= A.add !r A.one) p;
        !r
  end
end


module Z = struct

  open Zdd

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
      r

  let inter_right_to_left cols = 
    let width = Array.length cols in
    let z = ref cols.(0) in
      for j = 1 to width - 1 do
        let c = cols.(j) in
          printf "size = %d@." (size !z);
          z := inter c !z
      done;
      !z

  let inter_left_to_right cols = 
    let width = Array.length cols in
    let z = ref cols.(width - 1) in
      for j = width - 2 downto 0 do
        let c = cols.(j) in
          printf "size = %d@." (size !z);
          z := inter c !z
      done;
      !z


  let inter_middle_balancing cols = 
    let width = Array.length cols in
    let min = 0 in 
    let max = width - 1 in 
    let rec balancing min max = 
      let mid = min + (max - min) / 2 + ((max - min) mod 2) in 
        if min = max then ()
        else begin
          for j = min to mid - 1 do
            cols.(mid + (j - min)) <- inter cols.(mid + (j - min)) cols.(j)
          done;
          balancing mid max
        end
    in 
      balancing min max;
      printf "size = %d@." (size cols.(max));
      cols.(max)

  let tiling m =
    let width = Array.length m.(0) in
    let cols = Array.init width (fun j -> (* j, min_row j 0, *) column j m) in
      inter_middle_balancing cols


  let find_solution p = any_element (tiling p)
  let iter_solution f p = iter_element f (tiling p)
  let simple_count_solutions p = cardinal (tiling p) 

  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
  module Count(A: ARITH) = struct
    module C = Cardinal(A)
    let count_solutions p = C.cardinal (tiling p)
  end
end





















