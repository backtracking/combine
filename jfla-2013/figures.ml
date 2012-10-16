
open Mlpost
open Num
open Box
open Command

let caml = [|
"..**..........";
".***..........";
"****..**.**...";
"..**..*****...";
"..**.*******..";
"..***********.";
"..************";
"...****..***.*";
"....**....**..";
"....**....**..";
"....**....**..";
"....**....**..";
"....**....**..";
           |]

let caml2 = [|
"....****....................";
"..******....................";
"********....****..****......";
"....****....**********......";
"....****..**************....";
"....**********************..";
"....************************";
"......********....******..**";
"........****........****....";
"........****........****....";
"........****........****....";
"........****........****....";
"........****........****....";
            |]

let bool_array_of_string s = Array.init (String.length s) (fun i -> s.[i] = '*')
let caml = Array.map bool_array_of_string caml
let caml2 = Array.map bool_array_of_string caml2

let u = bp 5.
let p x y = Point.pt (x *. u, -. y *. u)
let cell =
  Box.shift (p 0.5 0.5) (rect ~dx:zero ~dy:zero (empty ~width:u ~height:u ()))

let of_bool_matrix m =
  let h = Array.length m in
  assert (h > 0);
  let w = Array.length m.(0) in
  iter 0 (h-1) (fun y ->
  iter 0 (w-1) (fun x ->
    if m.(y).(x) then Box.draw (Box.shift (p (float x) (float y)) cell)
    else nop))

let () = Metapost.emit "caml_problem" (of_bool_matrix caml)
let () = Metapost.emit "caml2_problem" (of_bool_matrix caml2)

let scott = Array.init 8 (fun i -> Array.init 8 (fun j ->
            not (3 <= i && i <= 4 && 3 <= j && j <= 4)))
let () = Metapost.emit "scott_problem" (of_bool_matrix scott)

let pentaminos = [
[|
"*****";
|]; [|
"***";
"*..";
"*..";
|]; [|
".**";
".*.";
"**.";
|]; [|
"***";
"**.";
|]; [|
"***.";
"..**";
|]; [|
".**";
"**.";
"*..";
|]; [|
"****";
".*..";
|]; [|
"***";
".*.";
".*.";
|]; [|
".**";
"**.";
".*.";
|]; [|
"***";
"*.*";
|]; [|
"****";
"*...";
|]; [|
".*.";
"***";
".*.";
|]
]

let () =
  let c = ref 0 in
  let emit_penta p = incr c;
    let m = Array.map bool_array_of_string p in
    Metapost.emit (Format.sprintf "penta%d" !c) (of_bool_matrix m);
  in
  List.iter emit_penta pentaminos


let caml_solution = [|
"..TT..........";
".UUS..........";
"VVRS..44.10...";
"..RQ..53210...";
"..PQ.=532966..";
"..PMM=><;9:87.";
"..OOLL><;?:87W";
"...NNDD..?A@.W";
"....EE....A@..";
"....GF....CB..";
"....GF....CB..";
"....KJ....IH..";
"....KJ....IH..";
                    |]

let char_array_of_string s = Array.init (String.length s) (fun i -> s.[i])
let caml_solution = Array.map char_array_of_string caml_solution

let hdomino =
  Box.shift (p 1. 0.5)
    (round_rect ~dx:zero ~dy:zero (empty ~width:(2.*.u) ~height:u ()))
let vdomino =
  Box.shift (p 0.5 1.)
    (round_rect ~dx:zero ~dy:zero (empty ~width:u ~height:(2.*.u) ()))

let of_char_matrix m =
  let seen = Hashtbl.create 17 in Hashtbl.add seen '.' ();
  let unseen c = not (Hashtbl.mem seen c || (Hashtbl.add seen c (); false)) in
  let h = Array.length m in
  assert (h > 0);
  let w = Array.length m.(0) in
  iter 0 (h-1) (fun y ->
  iter 0 (w-1) (fun x ->
    let c = m.(y).(x) in
    if unseen c then begin
      if x < w-1 && m.(y).(x+1) = c then
        Box.draw (Box.shift (p (float x) (float y)) hdomino)
      else
        Box.draw (Box.shift (p (float x) (float y)) vdomino)
    end else nop))

let () = Metapost.emit "caml_solution" (of_char_matrix caml_solution)

let modbox s =
  round_rect (tex (Format.sprintf "\\tt %s" s))

let archi =
  let tiling = modbox "Tiling" in
  let emc = modbox "Emc" in
  let zdd = modbox "Zdd" in
  let dlx = modbox "Dlx" in
  let b = vbox ~padding:(bp 30.)
    [tiling;
     emc;
     hbox ~padding:(bp 30.) [dlx; zdd]]
  in
  let box_label_arrow x y ~pos ~label =
    let label = Picture.tex label in
    Helpers.box_label_arrow ~sep:(bp 5.) ~pos label (sub x b) (sub y b) in
  Box.draw b ++
  box_label_arrow tiling emc ~label:"r\\'eduction" ~pos:`Right ++
  box_label_arrow emc    dlx ~label:"imm\\'ediat"  ~pos:`Left  ++
  box_label_arrow emc    zdd ~label:"r\\'eduction" ~pos:`Right ++
  nop

let () = Metapost.emit "archi" archi

let u = bp 20.
let node ~stroke () =
  round_rect ~stroke ~dx:zero ~dy:zero (empty ~width:u ~height:u ())

let dlx m0 =
  let h = Array.length m0 in
  let w = Array.length m0.(0) in
  let row ?(stroke=Color.black) r =
    let stroke = Some stroke in
    Array.init (w+2)
      (fun i ->
         if i = 0 || i = w+1 || not r.(i-1)
         then empty ~width:u ~height:u () else node ~stroke ())
  in
  let headers = row ~stroke:Color.red (Array.make w true) in
  let m = Array.init (h+3)
    (fun i ->
       if i = 0 || i = h+2 then row (Array.make w false)
       else if i = 1 then headers else row m0.(i-2)) in
  let b = tabular ~hpadding:u ~vpadding:u m in
  let box i j = sub m.(i+1).(j) b in
  let arrow ~ofs p1 p2 =
    Helpers.draw_simple_arrow (Point.shift ofs p1) (Point.shift ofs p2) in
  let right i j =
    let x = box i j in let y = box i (j+1) in
    arrow ~ofs:(Point.pt (0., 2.)) (east x) (west y) in
  let left i j =
    let x = box i j in let y = box i (j-1) in
    arrow ~ofs:(Point.pt (0., -2.)) (west x) (east y) in
  let up i j =
    let x = box i j in let y = box (i-1) j in
    arrow ~ofs:(Point.pt (2., 0.)) (north x) (south y) in
  let down i j =
    let x = box i j in let y = box (i+1) j in
    arrow ~ofs:(Point.pt (-2., 0.)) (south x) (north y) in
  Box.draw b ++
  iter 0 h (fun i ->
  iter 1 w (fun j ->
              if i = 0 || m0.(i-1).(j-1) then
                right i j ++ left i j ++ up i j ++ down i j else nop
           )) ++
  nop

let () = Metapost.emit "dlx" (dlx
                                [| [| true; false; true; true;  |];
                                   [| false; true; true; false; |];
                                   [| true; true; false; true; |];
                                   [| true; false; false; true; |];
                                   [| false; true; false; false |] |])
