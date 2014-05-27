open Printf
open Combine

(* open Mlpost *)

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
      

  
    
    

let length = 16 
let w = 4

let line a t x y =
  (* a.((y * w + x) * 4) <- false; *)
  if t.n && y > 0 then a.((((y - 1) * w) + x) * 4 + 2) <- true
  else if not t.n then a.((y * w + x) * 4) <- true;
  
  if t.e && x < w - 1 then a.((y * w + x + 1) * 4 + 3) <- true
  else if not t.e then a.((y * w + x) * 4 + 1) <- true;

  if t.s && y < w - 1 then a.((((y + 1) * w) + x) * 4) <- true
  else if not t.s then a.((y * w + x) * 4 + 2) <- true;

  if t.w && x > 0 then a.((y * w + x - 1) * 4 + 1) <- true
  else if not t.w then a.((y * w + x) * 4 + 3) <- true
    
  
let rec set_lines m ntile = 
  function | [] -> () | h :: tail ->
    for i = 0 to length - 1 do
      line m.(length * ntile + i) h (i mod 4) (i / 4)
    done;
    set_lines m (ntile + 1) tail


let () =
  let matrix = Array.make_matrix (length * length) (length * 4) false in
  let tiles = build_tiles 15 in
  (* print_tiles tiles; *)
  set_lines matrix 0 tiles;
  let p = Emc.D.create matrix in
  let s = Emc.D.find_solution p in
  let n = List.length s in
  printf "solution size : %d.\n" n


