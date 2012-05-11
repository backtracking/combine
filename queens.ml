(* Queens Module *)

open Format

let range = ref 0

(* emc size : rang * (rang + 1) *)


let msg = "usage: ./queens -n value"
let spec = ["-n", Arg.Set_int range, "   Range of the queens problem" ]

let () = Arg.parse spec (fun _ ->()) msg
let () = if !range < 1 then begin Arg.usage spec msg; exit 1 end


let nrange = !range

let get_line i j n = 
  let line = Array.make (7 * nrange - 2) false in
    line.(i) <- true;
    line.(nrange + j) <- true;
    line.(3 * nrange - 1+ i - j) <- true;
    line.(4 * nrange - 1 + i + j) <- true;
    line.(6 * nrange - 2 + n) <- true;
    line

let const_unused () = 
  let emc_length = (7 * nrange - 2) in 
  let lr = ref [] in 
    for i = 0 to emc_length - 1 - nrange do
      let line = Array.make emc_length false in 
        line.(i) <- true;
        lr := line :: !lr;
    done ;
    !lr

let emc nrange = 
  let lr = ref (const_unused ()) in 
    for i = 0 to nrange - 1 do
      for j = 0 to nrange - 1 do
          lr := get_line i j i:: !lr 
      done
    done;
    Array.of_list !lr




let () = 
  printf "Range : %d@." !range;
  let emc_array = emc nrange in 
(*     Emc.print_boolean_matrix emc_array; *)
    Emc.print_problem_size emc_array;
    printf "DLX : solutions : %d@." (Emc.D.count_solutions emc_array)




