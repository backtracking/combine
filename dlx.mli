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

val print_solutions:
  bool array array -> unit

val print_first_solution :
  bool array array -> unit
            
val count_solutions :
  bool array array -> int

val get_solution_array :
  bool array array -> int list array

val print_solution_array :
  int list array -> unit
