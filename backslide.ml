open Printf
open Combine

type btile = {n : bool; e : bool; s : bool; w : bool;}


let empty = {n = false; e = false; s = false; w = false;}

let print fmt t = fprintf fmt "(%B, %B, %B, %B)" t.n t.e t.s t.w
let print_tiles = List.iter (printf "%a\n" print)

let print_line line = 
  Array.iteri (fun i e -> 
    let v = if e then 1 else 0 in 
    printf "%d" v; 
    if (i + 1) mod 4 = 0 then printf " ";
    (* if (i + 1) mod 16 = 0 then printf "\n"; *)
  ) line
let print_matrix = Array.iter (fun e -> print_line e; printf "\n")

let xorr a b = a && not b || not a && b, a && b

let rec bsucc t = 
  let n, r  = xorr t.n true in
  let e, r = xorr t.e r in
  let s, r = xorr t.s r in
  let w, _ = xorr t.w r in
  {n; e; s; w} 
    

let build_tiles n =
  let rec build n = function
    | [] -> build n [empty]
    | h :: _ as l -> if n = 0 then l 
      else build (n - 1) ((bsucc h) :: l)
  in build n []
      
  

type t = N | E | S | W

    


let length = 16 
let w = 4

let line tile t x y =
  let a = Array.make ((w + 1) * length) false in
  
  a.(tile) <- true;

  if t.n && y > 0 then a.((((y - 1) * w) + x) * 4 + 2 + length) <- true 
  else if not t.n then a.((y * w + x) * 4 + length) <- true
  else raise Exit;
  
  if t.e && x < w - 1 then a.((y * w + x + 1) * 4 + 3 + length) <- true
  else if not t.e then a.((y * w + x) * 4 + 1 + length) <- true
  else raise Exit;

  if t.s && y < w - 1 then a.((((y + 1) * w) + x) * 4 + length) <- true
  else if not t.s then a.((y * w + x) * 4 + 2 + length) <- true
  else raise Exit;

  if t.w && x > 0 then a.((y * w + x - 1) * 4 + 1 + length) <- true
  else if not t.w then a.((y * w + x) * 4 + 3 + length) <- true
  else raise Exit;
  a
    
  
let emc tiles = 
  let rec set_lines tile accl = 
    let rec step h i accl = 
      if i = 16 then accl 
      else try step h (i + 1) ((line tile h (i mod 4) (i / 4)) :: accl) 
        with Exit -> step h (i + 1) accl
    in function | [] -> accl | h :: tail -> set_lines (tile + 1) 
      ((step h 0 accl)) tail 
  in 
  Array.of_list (set_lines 0 [] tiles)

let print_solution m = List.iter (fun s ->
  print_line s)
  
  

let () =
  let tiles = build_tiles 15 in
  (* print_tiles tiles; *)
  let matrix = emc tiles in
  print_matrix matrix;
  let p = Emc.D.create matrix in
  let s = Emc.D.find_solution p in
  let n = List.length s in
  printf "matrix : %dx%d.\n" (Array.length matrix) (Array.length matrix.(0));
  printf "solution size : %d.\n" n


