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

val find_first_solution :
  bool array array -> unit
            



