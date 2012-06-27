(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2012                                                    *)
(*    Remy El Sibaie                                                      *)
(*    Jean-Christophe Filliatre                                           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Queens Module *)

open Format

let range = ref 0

(* emc size : rang * (rang + 1) *)


let msg = "usage: ./queens -n value"
let spec = ["-n", Arg.Set_int range, "   Range of the queens problem" ]

let () = Arg.parse spec (fun _ ->()) msg

let get_line i j n = 
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
      lr := get_line i j n :: !lr 
    done
  done;
  Array.of_list !lr

let () = 
  if !range > 0 then begin
    printf "Range : %d@." !range;
    let emc_array = emc !range in 
    Emc.print_problem_size emc_array;
    let p = Emc.D.create ~primary:(2 * !range) emc_array in
    printf "DLX : solutions : %d@." (Emc.D.count_solutions p);
    let p = Emc.Z.create ~primary:(2 * !range) emc_array in
    printf "ZDD : solutions : %d@." (Emc.Z.count_solutions p)
  end





