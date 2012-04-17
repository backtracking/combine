type node = {
    mutable c : node;
    mutable s: int;
    mutable up: node;
    mutable down: node;
    mutable left: node;
    mutable right: node;
    mutable name : string;
}

val find_solution:
  bool array array -> int list

val one_node : 
  unit -> node

val add_right : 
  node -> node -> unit

val add_below : 
  node -> node -> unit

val add_row :
  node array -> bool array -> int -> unit

val generate_headers :
  bool array array -> node -> node array

val iter_right :
  ?self:bool -> (node -> unit) -> node -> unit
 
val iter_down :
  ?self:bool -> (node -> unit) -> node -> unit
      
val iter_left :
  ?self:bool -> (node -> unit) -> node -> unit
      
val iter_up :
  ?self:bool -> (node -> unit) -> node -> unit
                                                         
val create :
  bool array array -> node

val cover :
  node -> unit

val uncover :
  node -> unit

val print_solution :
  node array -> unit

val choose_min :
  node -> node

val search :
  int -> node -> node array -> unit



            



