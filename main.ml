let l = int_of_string Sys.argv.(1)
let h = int_of_string Sys.argv.(2)


let board = Array.create_matrix l h true


let pieces = [
  [|[|true; true|];[|true; false|]|]
]

let _ = 
  Tiling.display_boolean_matrix board;
  let m = Tiling.matrix_of_game board (Tiling.get_all_rotations pieces) in 
    Format.printf "Dlx solutions : %d@." (Dlx.get_solution_number m) 


