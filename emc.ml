(* EMC Module *)

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
  let find_solution p = Zdd.any_element (Zdd.tiling p)
  let iter_solution f p = Zdd.iter_element f (Zdd.tiling p)
  let simple_count_solutions p = Zdd.cardinal (Zdd.tiling p) 
  module type ARITH = sig
    type t
    val zero: t
    val one: t
    val add: t -> t -> t
  end
  module Htbl = 
    Hashtbl.Make 
      (struct
         type t = Zdd.t
         let hash = Zdd.unique 
         let equal = (==) 
       end)
  module Count = functor (A: ARITH) ->  
  struct
    let count_solutions p = 
      let table = Htbl.create 19997 in
      let zdd = Zdd.tiling p in
      let rec cardinal = function
        | Zdd.Top -> A.one
        | Zdd.Bottom -> A.zero
        | Zdd.Node(_, i, z1, z2)-> A.add (memo z1) (memo z2)
      and memo z =
        try
          Htbl.find table z
        with Not_found ->
          let c = cardinal z in
            Htbl.add table z c;
            c
      in 
        cardinal zdd
  end
end





















