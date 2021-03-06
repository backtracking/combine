(**************************************************************************)
(*                                                                        *)
(*  Combine - an OCaml library for combinatorics                          *)
(*                                                                        *)
(*  Copyright (C) 2012-2014                                               *)
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

type node = {
  mutable c: node;
  mutable s: int;
  mutable up: node;
  mutable down: node;
  mutable left: node;
  mutable right: node;
  mutable name : string;
}

type t = {
  header: node;
  nc    : int;  (* number of columns *)
}

let one_node () =
  let rec h = { name = "head"; c = h; s = 0; up = h;
                down = h; left = h; right = h } in h

(* Adds n2 to the right of n1 in the DLM *)
let add_right n1 n2 =
  let tmp = n1.right in
  n1.right <- n2;
  n2.right <- tmp;
  n2.left <- n1;
  n2.right.left <- n2

(* Adds n2 under n1 in the DLM *)
let add_below n1 n2 =
  let tmp = n1.down in
  n1.down <- n2;
  n2.down <- tmp;
  n2.up <- n1;
  n2.down.up <- n2

(* Adds row after the headers in the DLM *)
let add_row headers row i =
  let dummy = one_node () in
  let rec addi_rec n previous =
    if n < Array.length row then
      if row.(n) then begin
        let element = one_node () in
        element.s <- i;
        element.c <- headers.(n);
        element.name <- "";
        headers.(n).s <- headers.(n).s + 1;
        if previous != dummy then add_right previous element;
        add_below headers.(n) element;
        addi_rec (n + 1) element
      end else
        addi_rec (n + 1) previous
  in
  addi_rec 0 dummy

(* Returns a DLM only with the headers *)
let generate_headers ?primary size h =
  let headers = Array.init size (fun _ -> one_node ()) in
  let primary = match primary with
    | None -> size
    | Some p -> if p < 0 || p > size then invalid_arg "create"; p
  in
  headers.(0).s <- 0;
  headers.(0).name <- "C0";
  add_right h headers.(0);
  for n = 1 to primary - 1 do
    headers.(n).s <- 0;
    headers.(n).name <- String.concat "" ["C";string_of_int n];
    add_right headers.(n - 1) headers.(n);
  done;
  headers (* on retourne le node array *)

(* Applies f to elements of the DLM, from left to right*)
let iter_right ?(self = true) f n =
  if self then f n;
  let rec rec_iter_right node =
    if node != n then begin
      f node;
      rec_iter_right node.right
    end
  in
  rec_iter_right n.right

(* Creates a DLM from a boolean matrix *)
let create ?primary m =
  let h = one_node () in
  let nc = Array.length m.(0) in
  let headers = generate_headers ?primary nc h in
  for i = Array.length m - 1 downto 0 do
    add_row headers m.(i) i
  done;
  { header = h; nc = nc; }

let create_sparse ?primary ~columns:nc a =
  let h = one_node () in
  let headers = generate_headers ?primary nc h in
  let row = Array.make nc false in
  for i = Array.length a - 1 downto 0 do
    Array.fill row 0 nc false;
    List.iter (fun c -> row.(c) <- true) a.(i);
    add_row headers row i
  done;
  { header = h; nc = nc; }

(* test create_sparse using create
let create ?primary m =
  let nc = Array.length m.(0) in
  let row a =
    let r = ref [] in
    Array.iteri (fun i b -> if b then r := i :: !r) a;
    !r
  in
  create_sparse ?primary ~columns:nc (Array.map row m)
*)

(* Applies f to elements of the DLM, from up to down*)
let iter_down ?(self = true) f n =
  if self then f n;
  let rec rec_iter_down node =
    if node != n then begin
      f node;
      rec_iter_down node.down
    end
  in
  rec_iter_down n.down

(* Applies f to elements of the DLM, from right to left *)
let iter_left ?(self = true) f n =
  if self then f n;
  let rec rec_iter_left node =
    if node != n then begin
      f node;
      rec_iter_left node.left
    end
  in
  rec_iter_left n.left

(* Applies f to elements of the DLM, from down to up *)
let iter_up ?(self = true) f n =
  if self then f n;
  let rec rec_iter_up node =
    if node != n then begin
      f node;
      rec_iter_up node.up
    end
  in
  rec_iter_up n.up

(* Removes the given column and all rows in column own list from
 the DLM*)
let cover column_header =
  column_header.right.left <- column_header.left;
  column_header.left.right <- column_header.right;
  let cover_node n =
    n.down.up <- n.up;
    n.up.down <- n.down;
    n.c.s <- n.c.s - 1
  in
  let cover_row n =
    iter_right ~self:false cover_node n
  in
  iter_down ~self:false cover_row column_header

(* Un-removes the given column and all rows in column own list from
 the DLM*)
let uncover column_header =
  let uncover_node n =
    n.c.s <- n.c.s + 1;
    n.down.up <- n;
    n.up.down <- n
  in
  let uncover_row n =
    iter_left ~self:false uncover_node n
  in
  iter_up ~self:false uncover_row column_header;
  column_header.right.left <- column_header;
  column_header.left.right <- column_header

(* Print the given solution *)
let print_solution fmt (o, k) =
  for i = 0 to k - 1  do
    Format.fprintf fmt "%d" o.(i).s;
    if i < k-1 then Format.fprintf fmt "@ "
  done

(* Returns the min column *)
let choose_min h =
  let rec rec_chose min node =
    if node == h then min
    else if node.s < min.s then
      rec_chose node node.right
    else
      rec_chose min node.right
  in
  rec_chose h.right h.right.right

(* Searches for all solutions, applying [f] on each *)
let rec search f k h o =
  if h == h.right then f (o, k)
  else
    let column = choose_min h in
    let get_down r =
      o.(k) <- r;
      iter_right ~self:false (fun j -> cover j.c) r;
      search f (k + 1) h o;
      iter_left ~self:false (fun j -> uncover j.c) r
    in
      cover column;
      iter_down ~self:false get_down column;
      uncover column

type solution = node array * int

(* Returns a solution as an int list *)
let list_of_solution (o, k) =
  let rec rec_stl l i =
    if i = k then l else rec_stl (o.(i).s :: l) (i + 1)
  in
  rec_stl [] 0


(* Applies f to all solutions returned by function search *)
let iter_solution f dlm =
  let o = Array.init dlm.nc (fun _ -> one_node ()) in
  search f 0 dlm.header o

let count_solutions m =
  let r = ref 0 in
  iter_solution (fun (_, _) -> r:= !r + 1) m;
  !r

(* unused... *)
let _get_solution_list m =
  let list_ref = ref [] in
  iter_solution (
    fun (o, k) -> list_ref := list_of_solution (o, k) :: !list_ref
  ) m;
  !list_ref

(* Print the given solution as an int list *)
let _print_list_solution fmt l =
  List.iter (fun e -> Format.fprintf fmt "%d " e) l; Format.fprintf fmt "@."

exception Solution of (node array * int)

let get_first_solution m =
  try
    iter_solution (fun s -> raise (Solution s)) m;
    raise Not_found
  with
    | Solution s -> s
