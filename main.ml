let n = int_of_string Sys.argv.(1)


let board = Array.create_matrix n n true


let test_clockwise = 
  [|
    [|true; true|];
    [|true; false|];
    [|false; true|]
   |]


let pieces = [
  [|[|true; true|]|]
]

let _ = 
  Tiling.display_boolean_matrix test_clockwise;
  Format.printf "----------------------@.";
  Tiling.display_boolean_matrix 
    (Tiling.quarter_turn_anticlockwise test_clockwise)
(*  let m = Tiling.matrix_of_game board pieces in 
    Format.printf "%d@." (Dlx.get_solution_number m) 
    *)


