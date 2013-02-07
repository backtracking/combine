
(* 4-coloring planar graphs using Combine

   requires OCamlgraph *)

open Format
open Graph
open Combine

(* command line *)
let n_ = ref 30
let prob_ = ref 0.5
let seed_ = ref None

let arg_spec =
  ["-v", Arg.Int (fun i -> n_ := i),
   " <int>  number of vertices";
   "-prob", Arg.Float (fun f -> prob_ := f),
   " <float>  probability to discrad an edge";
   "-seed", Arg.Int (fun n -> seed_ := Some n),
   " <int>  random seed"
  ]
let () = Arg.parse arg_spec (fun _ -> ()) "usage: color <options>"

let n = !n_
let prob = !prob_

let seed = match !seed_ with
  | None -> Random.self_init (); Random.int (1 lsl 29)
  | Some s -> s
let () = Format.printf "seed = %d@." seed; Random.init seed

(* undirected graphs with integer coordinates and integer labels on edges *)

module IntInt = struct
  type t = int * int
end
module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end
module G = Imperative.Graph.AbstractLabeled(IntInt)(Int)
open G

(* a random graph with n vertices *)
module R = Rand.Planar.I(G)
let g0 = R.graph ~xrange:(20,780) ~yrange:(20,580) ~prob n

(* drawing *)
let round f = truncate (f +. 0.5)
let pi = 4.0 *. atan 1.0

open Graphics
let () = open_graph " 800x600"

let vertex_radius = 5

let draw_edge v1 v2 =
  let (xu,yu) = G.V.label v1 in
  let (xv,yv) = G.V.label v2 in
  set_color black;
  let dx = float (xv - xu) in
  let dy = float (yv - yu) in
  let r = sqrt (dx *. dx +. dy *. dy) in
  let d = float vertex_radius +. 3. in
  let xs, ys = float xu +. d *. dx /. r, float yu +. d *. dy /. r in
  let xd, yd = float xv -. d *. dx /. r, float yv -. d *. dy /. r in
  moveto (round xs) (round ys);
  lineto (round xd) (round yd)

let draw_vertex v =
  let (x,y) = G.V.label v in
  set_color red;
  draw_circle x y vertex_radius

let color_vertex v color =
  let x,y = G.V.label v in
  set_color color;
  fill_circle x y vertex_radius

let draw_graph () =
  clear_graph ();
  set_color red;
  set_line_width 1;
  G.iter_vertex draw_vertex g0;
  G.iter_edges draw_edge g0

let cols = [| white; red; green; blue; yellow; black |]

module Hv2 = Hashtbl.Make(
  struct
    type t = G.V.t * G.V.t
    let equal (a,b) (c,d) = G.V.equal a c && G.V.equal b d
    let hash (a,b) = (G.V.hash a + G.V.hash b) land max_int
  end)

let num_edge =
  let r = ref (-1) in
  let h = Hv2.create 17 in
  fun a b ->
    let e = if V.compare a b < 0 then a, b else b, a in
    try Hv2.find h e with Not_found -> incr r; Hv2.add h e !r; !r

let vertexi = Hashtbl.create 17

let emc_coloring () =
  let v = nb_vertex g0 in
  let e = nb_edges g0 in
  Format.eprintf "V = %d@." v;
  Format.eprintf "E = %d@." e;
  let emc = Array.make (4*v) [] in
  let set j i = emc.(j) <- i :: emc.(j) in
  let i = ref 0 in
  let add_vertex a =
    Hashtbl.add vertexi !i a;
    let j = 4 * !i in
    set j     !i;
    set (j+1) !i;
    set (j+2) !i;
    set (j+3) !i;
    let add_edge b =
      let c = v + 4 * num_edge a b in
      set (j  ) (c  );
      set (j+1) (c+1);
      set (j+2) (c+2);
      set (j+3) (c+3)
    in
    G.iter_succ add_edge g0 a;
    incr i
  in
  G.iter_vertex add_vertex g0;
  (* Format.eprintf "%a@." Emc.print_boolean_matrix emc; *)
  v, v + 4*e, emc

open Unix

let utime f x =
  let u = (times()).tms_utime in
  let y = f x in
  let ut = (times()).tms_utime -. u in
  (y,ut)

let print_utime f x =
  let (y,ut) = utime f x in
  Format.printf "user time: %2.2f@." ut;
  y

let print_solution fmt s = List.iter (fun i -> fprintf fmt "%d " i) s

let () =
  draw_graph ();
  let primary, columns, emc = emc_coloring () in
  print_utime
    (fun () ->
      printf "DLX@.";
      let dlx = Emc.D.create_sparse ~primary ~columns emc in
      let s = Emc.D.find_solution dlx in
      printf "  => %a@." print_solution s;
      let show j =
        let v = Hashtbl.find vertexi (j / 4) in
        let c = 1 + j mod 4 in
        color_vertex v cols.(c)
      in
      List.iter show s;
      Format.printf "  DLX: found!@.")
    ();
  ignore (
    let st = Graphics.wait_next_event [ Key_pressed ] in
    if st.key = 'q' then exit 0
  );
  print_utime
    (fun () ->
      printf "SAT (with minisat2)@.";
      let sat = Emc.Sat.create_sparse ~primary ~columns emc in
      Emc.Sat.print_in_file "color.dimacs" sat;
      let minisat2 ~input ~output = sprintf "minisat2 %s %s" input output in
      let s = Emc.Sat.find_solution minisat2 sat in
      printf "  => %a@." print_solution s;
      let show j =
        let v = Hashtbl.find vertexi (j / 4) in
        let c = 1 + j mod 4 in
        color_vertex v cols.(c)
      in
      List.iter show s;
      Format.printf "  SAT: found!@.")
    ();
  ignore (
    let st = Graphics.wait_next_event [ Key_pressed ] in
    if st.key = 'q' then exit 0
  );
  print_utime
    (fun () ->
      printf "ZDD@.";
      let zdd = Emc.Z.create_sparse ~primary ~columns emc in
      Format.eprintf "  ZDD => %d@." (Emc.Z.count_solutions zdd))
    ();
  ignore (Graphics.wait_next_event [ Key_pressed ]);
  close_graph ()


(*
Local Variables:
compile-command: "make color.opt"
End:
*)
