let n = int_of_string Sys.argv.(1)


let board = Array.create_matrix n n true

let pieces = [
  [|[|true|]; [|true|]|];
  [|[|true; true|]|]
]

let _ = 
let m = Tiling.matrix_of_game board pieces in 
  Format.printf "%d@." (Dlx.get_solution_number m) 



