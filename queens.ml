(* Queens Module *)

open Format

let range = ref 0

(* emc size : rang * (rang + 1) *)


let msg = "usage: ./queens -n value"
let spec = ["-n", Arg.Set_int range, "   Range of the queens problem" ]

let () = Arg.parse spec (fun _ ->()) msg
let () = if !range < 1 then begin Arg.usage spec msg; exit 1 end

let n = !range

let get_line i j = 
  let line = Array.make (6 * n - 2) false in
  line.(i) <- true;
  line.(n + j) <- true;
  line.(2 * n + i + j) <- true;
  line.(4 * n - 1 + n - 1 - i + j) <- true;
  line

let emc n = 
  let lr = ref [] in 
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      lr := get_line i j :: !lr 
    done
  done;
  Array.of_list !lr

let () = 
  printf "Range : %d@." !range;
  let emc_array = emc n in 
  Emc.print_problem_size emc_array;
  let p = Emc.Z.create ~primary:(2 * n) emc_array in
  printf "DLX : solutions : %d@." (Emc.Z.count_solutions p)




