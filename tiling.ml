(* Tiling Module *)


let existing_position board x y = 
  x < Array.length board 
  && y < Array.length board.(0) 
  && board.(x).(y)

let is_possible_position piece board x y =
  let possible = ref true in
  let i = ref 0 in 
  let j = ref 0 in 
    while i < Array.length piece && !possible do
      j := 0;
      while j < Array.length piece.(0) && possible do
        if piece.(i).(j) then 
          possible := existing_position board (x + i) (y + j);
        j := !j + 1
      done;
      i := !i + 1
    done;
    !possible






let matrix_of_game board pieces =
  assert false

  


