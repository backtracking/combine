(* Tiling Module *)

let existing_position board x y = 
  x < Array.length board 
  && y < Array.length board.(0) 
  && board.(x).(y)

let is_possible_position piece board x y =
  try
    for i = 0 to Array.length piece - 1 do
      for j = 0 to Array.length piece.(0) - 1 do
        if piece.(i).(j) && not (existing_position board (x + i) (y + j))
        then raise Exit
      done;
    done;
    true
  with 
    | Exit -> false


let one_line l n p x y = 
  let line = Array.make n false in                                         		  
    for i = 0 to Array.length p - 1 do  
      for j = 0 to Array.length p.(0) - 1 do  
        if p.(i).(j) then
          line.((x + i) * l + y + j) <- true
      done 
    done;
    line 



let matrix_of_game board pieces =
  let h = Array.length board in 
  let l = Array.length board.(0) in 
  let n = l * h  in
  let lines = ref [] in 
  let add_piece p = 
    for i = 0 to Array.length board - 1 do
      for j = 0 to Array.length board.(0) do
        if is_possible_position p board i j then
          lines := one_line l n p i j::!lines
      done 
    done 
  in 
    List.iter add_piece pieces; 
    Array.of_list !lines

  


