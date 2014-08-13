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

(* Implementation of DLX using arrays and integer indices instead of
   records and (thus pointers).

   Each node is an integer.
   Node 0 is the main header.
   Nodes 1..nc are the columns headers.
   Nodes nc+1 and above are regular nodes.
*)

type t = {
  nc    : int;        (* number of columns) *)
  c     : int array;
  s     : int array;
  up    : int array;
  down  : int array;
  left  : int array;
  right : int array;
  name  : string array;
  mutable next: int; (* next node available *)
}

let one_node t =
  let n = t.next in
  t.next <- n + 1;
  t.name.(n) <- "head";
  t.c.(n) <- n;
  t.s.(n) <- 0;
  t.up.(n) <- n;
  t.down.(n) <- n;
  t.left.(n) <- n;
  t.right.(n) <- n;
  n

(* Adds n2 to the right of n1 in the DLM *)
let add_right t n1 n2 =
  let tmp = t.right.(n1) in
  t.right.(n1) <- n2;
  t.right.(n2) <- tmp;
  t.left.(n2) <- n1;
  t.left.(t.right.(n2)) <- n2

(* Adds n2 under n1 in the DLM *)
let add_below t n1 n2 =
  let tmp = t.down.(n1) in
  t.down.(n1) <- n2;
  t.down.(n2) <- tmp;
  t.up.(n2) <- n1;
  t.up.(t.down.(n2)) <- n2

(* Adds row after the headers in the DLM *)
let add_row t row i =
  let rec addi_rec n previous =
    if n < Array.length row then
      if row.(n) then begin
        let element = one_node t in
        assert (element > t.nc);
        let h = n + 1 in
        t.s.(element) <- i;
        t.c.(element) <- h;
        t.name.(element) <- "";
        t.s.(h) <- t.s.(h) + 1;
        if previous <> -1 then add_right t previous element;
        add_below t h element;
        addi_rec (n + 1) element
      end else
        addi_rec (n + 1) previous
  in
  addi_rec 0 (-1)

let generate_headers ?primary size t =
  let primary = match primary with
    | None -> size
    | Some p -> if p < 0 || p > size then invalid_arg "create"; p
  in
  (* create size nodes for the headers *)
  for n = 1 to size do ignore (one_node t) done;
  (* and link the primary ones with node 0 *)
  for n = 1 to primary do
    t.s.(n) <- 0;
    t.name.(n) <- "C" ^ string_of_int (n - 1);
    add_right t (n - 1) n;
  done

let create ?primary m =
  let nc = Array.length m.(0) in
  let add n b = if b then n+1 else n in
  let n = Array.fold_left (Array.fold_left add) (1 + nc) m in
  let t = {
    nc = nc;
    c     = Array.make n 0;
    s     = Array.make n 0;
    up    = Array.make n 0;
    down  = Array.make n 0;
    left  = Array.make n 0;
    right = Array.make n 0;
    name  = Array.make n "";
    next = 0;
  } in
  let n0 = one_node t in
  assert (n0 = 0);
  let headers = generate_headers ?primary nc t in
  for i = Array.length m - 1 downto 0 do
    add_row t m.(i) i
  done;
  t

(* Applies f to elements of the DLM, from left to right*)
let iter_right t ?(self = true) f n =
  if self then f n;
  let right = t.right in
  let rec rec_iter_right node =
    if node != n then begin
      f node;
      rec_iter_right right.(node)
    end
  in
  rec_iter_right right.(n)

(* Applies f to elements of the DLM, from up to down*)
let iter_down t ?(self = true) f n =
  if self then f n;
  let down = t.down in
  let rec rec_iter_down node =
    if node != n then begin
      f node;
      rec_iter_down down.(node)
    end
  in
  rec_iter_down down.(n)

(* Applies f to elements of the DLM, from right to left *)
let iter_left t ?(self = true) f n =
  if self then f n;
  let left = t.left in
  let rec rec_iter_left node =
    if node != n then begin
      f node;
      rec_iter_left left.(node)
    end
  in
  rec_iter_left left.(n)

(* Applies f to elements of the DLM, from down to up *)
let iter_up t ?(self = true) f n =
  if self then f n;
  let up = t.up in
  let rec rec_iter_up node =
    if node != n then begin
      f node;
      rec_iter_up up.(node)
    end
  in
  rec_iter_up up.(n)

(* Removes the given column and all rows in column own list from
 the DLM*)
let cover t h =
  t.left.(t.right.(h)) <- t.left.(h);
  t.right.(t.left.(h)) <- t.right.(h);
  let cover_node n =
    t.up.(t.down.(n)) <- t.up.(n);
    t.down.(t.up.(n)) <- t.down.(n);
    let c = t.c.(n) in
    t.s.(c) <- t.s.(c) - 1
  in
  let cover_row n =
    iter_right t ~self:false cover_node n
  in
  iter_down t ~self:false cover_row h

(* Un-removes the given column and all rows in column own list from
 the DLM*)
let uncover t h =
  let uncover_node n =
    let c = t.c.(n) in
    t.s.(c) <- t.s.(c) + 1;
    t.up.(t.down.(n)) <- n;
    t.down.(t.up.(n)) <- n
  in
  let uncover_row n =
    iter_left t ~self:false uncover_node n
  in
  iter_up t ~self:false uncover_row h;
  t.left.(t.right.(h)) <- h;
  t.right.(t.left.(h)) <- h

(* Returns the min column *)
let choose_min t =
  let rec rec_chose min node =
    if node == 0 then min
    else if t.s.(node) < t.s.(min) then
      rec_chose node t.right.(node)
    else
      rec_chose min t.right.(node)
  in
  let c = t.right.(0) in
  rec_chose c t.right.(c)

(* Searches for all solutions, applying [f] on each *)
let rec search f k t o =
  if t.right.(0) == 0 then f (o, k)
  else
    let column = choose_min t in
    let get_down r =
      o.(k) <- r;
      iter_right t ~self:false (fun j -> cover t t.c.(j)) r;
      search f (k + 1) t o;
      iter_left t ~self:false (fun j -> uncover t t.c.(j)) r
    in
    cover t column;
    iter_down t ~self:false get_down column;
    uncover t column

(* Applies f to all solutions returned by function search *)
let iter_solution f t =
  let o = Array.make t.nc 0 in
  search f 0 t o

let count_solutions m =
  let r = ref 0 in
  iter_solution (fun (_, _) -> r:= !r + 1) m;
  !r

