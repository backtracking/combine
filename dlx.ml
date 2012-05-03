(* Dlx Module *)

(* DLM : doubly linked matrix *)


type node = {
  mutable c : node;
  mutable s: int;
  mutable up: node;
  mutable down: node;
  mutable left: node;
  mutable right: node;
  mutable name : string;
}

exception Solution of node array * int
exception NotFound

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
  let rec addi_rec n previous = 
    if n = (Array.length row) then  begin
      () (* si on arrive au bout de la ligne on arrete *)
    end
    else 
      if row.(n) = true then ( (* si la case est à true *)
        let element = one_node () in
          element.s <- i; (* creation de l'element e ajouter *)
          element.c <- headers.(n);
          element.name <- "";
          headers.(n).s <- headers.(n).s + 1;
          if n != 0 then (* ajout au previous si non premier *)
            add_right previous element; 
          add_below headers.(n) element; (* ajout sous le headers.(i) *)
          addi_rec (n + 1) element (* au suivant *)
      )
      else 
        addi_rec (n + 1) previous (* rien a traiter, au suivant *)
  in 
    addi_rec 0 (one_node ()) 



(* Returns a DLM only with the headers *)
let generate_headers m h =
  let size = Array.length m.(0) in 
  let headers = Array.init size (fun _ -> one_node ()) in
  let rec link_rec n = 
    if n = size then (* en arrivant à la derniere case on arrete*)
      ()
    else begin
      headers.(n).s<-0;
      headers.(n).name<-String.concat "" ["line";string_of_int n];
      add_right headers.(n - 1) headers.(n); 
      (* on lie le header a droite du precedent *)
      link_rec (n + 1) (* suivant *)
    end
  in
    headers.(0).s<-0;
    add_right h headers.(0); (* on lie le premier à droite de la tete *)
    link_rec 1; 
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



(* Creates a DLM from a boolean matrix *)
let create m = 
  let h = one_node () in (* creation de la tete *)
  let headers = generate_headers m h in (* on recupere les headers *)
    (* On ajoute les lignes en partant de la derniere *)
    for i = Array.length m - 1 downto 0 do
      add_row headers m.(i) i (* ajout sous les headers *)
    done;
    h (* retourne la tete *)



(* Removes the given column and all rows in column own list from 
 the DLM*)
let cover column_header = 
  column_header.right.left <- column_header.left;
  column_header.left.right <- column_header.right;
  let cover_node n = (* retire l'elt *)
    n.down.up <- n.up;  
    n.up.down <- n.down;
    n.c.s <- n.c.s - 1
  in 
  let cover_row n = (* retire la ligne *)
    iter_right ~self:false cover_node n
  in 
    (* retire la colonne, head non compris *)
    iter_down ~self:false cover_row column_header 


(* Un-removes the given column and all rows in column own list from 
 the DLM*)
let uncover column_header =
  let uncover_node n = (* remet l'elt *)
    n.c.s <- n.c.s + 1;
    n.down.up <- n;
    n.up.down <- n
  in 
  let uncover_row n = (* remet la ligne *)
    iter_left ~self:false uncover_node n
  in 
    iter_up ~self:false uncover_row column_header; (* remet la colonne *)
    column_header.right.left <- column_header;
    column_header.left.right <- column_header


(* Print the given solution *)
let print_solution (o, k) =
  for i = 0 to k - 1  do
    Format.printf "%d@." o.(i).s
  done

(* Print the given solution as an int list *)
let print_list_solution l =
  List.iter (fun e -> Format.printf "%d " e) l; Format.printf "@."


(* Returns a solution as an int list *)
let list_of_solution (o, k) = 
  let rec rec_stl l i = 
    if i = k then l
    else rec_stl (o.(i).s::l) (i + 1)
  in 
    rec_stl [] 0


(* Returns the min column *)
let choose_min h = 
  let rec rec_chose min node =
    if node == h then min (* si on retombe sur la tete on arrete *)
    else if node.s < min.s then 
      (* si le nombre de 1 dans la colonne n > ... de la colonne min *)
      rec_chose node node.right 
    else 
        rec_chose min node.right
  in 
    rec_chose h.right h.right.right 

(* Search the solution set of matrix covering problem and apply f on it 
* *)
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

(* Applies f to all solutions returned by search 
   function from a boolean matrix*)
let iter_solution f m =
  let dlm = create m in
  let o = Array.init (Array.length m.(0)) (fun _ -> one_node ()) in
    search f 0 dlm o



(* Visible functions *)

(* retourne le nombre de solutions *)
let count_solutions m =
  let r = ref 0 in
    iter_solution (fun (_, _) -> r:= !r + 1) m;
    !r

(* Return a solution (int list) array *)
let get_solution_array m =
  let n = count_solutions m in
  let s_array = Array.make n [] in 
  let i = ref 0 in 
    iter_solution (
      fun (o, k) -> s_array.(!i) <- list_of_solution (o, k); i := !i + 1
    ) m;
    s_array

(* Print all solutions from a solution array on stdout *)
let print_solution_array s_array = 
  Array.iteri (fun i s -> Format.printf "Solution %d :" i;
                          print_list_solution s) s_array

(* Print all solution on stdout *)
let print_solutions m = 
  iter_solution print_solution m

(* Print the first solution founded on stdout *)
let print_first_solution m =
  try 
    iter_solution (fun (o, k) -> ignore(raise (Solution (o, k)))) m; 
    raise NotFound
  with 
    | Solution (o, k) -> print_solution (o, k)
    | NotFound -> Format.printf "No solutions.@."





