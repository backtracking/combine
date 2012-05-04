(* Dlx Module *)

type node = {
    mutable c : node;
    mutable s: int;
    mutable up: node;
    mutable down: node;
    mutable left: node;
    mutable right: node;
    mutable name : string;
}

val print_solutions: bool array array -> unit

val print_first_solution : bool array array -> unit

val get_first_solution : bool array array -> int list
            
val count_solutions : bool array array -> int

val get_solution_array : bool array array -> int list array

val get_solution_list : bool array array -> int list list

val print_solution_array : int list array -> unit

val list_of_solution : node array * int -> int list 

val iter_solution : (node array * int -> unit) -> bool array array-> unit
