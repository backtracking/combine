
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

 (* Ajoute n2 à droite de n1 dans la liste circulaire de n1 *)
let add_right n1 n2 =
let tmp = n1.right in
  n1.right <- n2;
  n2.right <- tmp;
  n2.left <- n1;
  n2.right.left <- n2


 (* Ajoute n2 en dessous de n1 dans la liste circulaire de n1 *)
let add_below n1 n2 =
let tmp = n1.down in 
  n1.down <- n2;
  n2.down <- tmp;
  n2.up <- n1;
  n2.down.up <- n2


(* Ajoute la ligne row apres les headers *)
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



 (* Lie h et tous les headers.(i) entre eux *)
let generate_headers m h =
let size = Array.length m.(0) in 
let headers = Array.init size (fun _ -> one_node ()) in
let rec link_rec n = 
  if n = size then (* en arrivant à la derniere case on arrete*)
    ()
  else begin
    headers.(n).s<-0;
    add_right headers.(n - 1) headers.(n); 
    (* on lie le header a droite du precedent *)
    link_rec (n + 1) (* suivant *)
  end
         in
  headers.(0).s<-0;
  add_right h headers.(0); (* on lie le premier à droite de la tete *)
  link_rec 1; 
  headers (* on retourne le node array *)



 (* Applique la fonction f à tout les elements de la list à droite de n*)
let iter_right ?(self = true) f n = 
    if self then f n;
    let rec rec_iter_right node = 
      if node != n then begin
        f node;
        rec_iter_right node.right
      end 
    in 
      rec_iter_right n.right



(* Applique la fonction f à tout les elements de la list sous n *)
let iter_down ?(self = true) f n = 
    if self then f n;
    let rec rec_iter_down node = 
      if node != n then begin
        f node;
        rec_iter_down node.down
      end 
    in 
      rec_iter_down n.down


(* Applique la fonction f à tout les elements de la list a gauche de n *)
let iter_left ?(self = true) f n = 
    if self then f n;
    let rec rec_iter_left node = 
      if node != n then begin
        f node;
        rec_iter_left node.left
      end 
    in 
      rec_iter_left n.left



(* Applique la fonction f à tout les elements de la list au dessus de n *)
let iter_up ?(self = true) f n = 
    if self then f n;
    let rec rec_iter_up node = 
      if node != n then begin
        f node;
        rec_iter_up node.up
      end 
    in 
      rec_iter_up n.up



 (* Cree une matrix doublement chainee a partir d'une matrix *)
let create m = 
let h = one_node () in (* creation de la tete *)
let headers = generate_headers m h in (* on recupere les headers *)
  (* On ajoute les lignes en partant de la derniere *)
  for i = Array.length m - 1 downto 0 do
    add_row headers m.(i) i (* ajout sous les headers *)
  done;
  h (* retourne la tete *)



(* Recouvre : retire une colonne et les lignes correspondant aux 
 elts de la colonne *)
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



(* Découvre : rajoute une colonne et les lignes correspondant aux elts 
 de la colonne *)
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



(* affiche la solution o *)
let print_solution (o, k) =
  for i = 0 to k - 1  do
    Format.printf "%d@." o.(i).s
  done

(* retourne une solution sous forme de int list *)
let solution_to_list (o, k) = 
  let rec rec_stl l i = 
    if i = k - 1 then l
    else rec_stl (o.(i).s::l) (i + 1)
  in 
    rec_stl [] 0


(* TODO : Inutilise - a ajouter *)
(* Choisit la colonne min *)
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


(* recherche l'ensemble des solutions au probleme de recouvrement *)
let rec search f k h o = 
  if h == h.right then f (o, k)
  else 
    let column = h.right in
    let get_down r = 
      o.(k) <- r;
      iter_right ~self:false (fun j -> cover j.c) r;
      search f (k + 1) h o;
      iter_left ~self:false (fun j -> uncover j.c) r
    in 
      cover column;
      iter_down ~self:false get_down column;
      uncover column


(* Initialise la matrice doublement chainee et affiche les solutions *)
let iter_solution f m =
  let dlm = create m in
  let o = Array.init (Array.length m.(0)) (fun _ -> one_node ()) in
    search f 0 dlm o




(* Fonctions visibles *)




(* retourne le nombre de solutions *)
let get_solution_number m =
  let r = ref 0 in
    iter_solution (fun (_, _) -> r:= !r + 1) m

(* Affiche toute les solutions *)
let print_solutions m = 
  iter_solution print_solution m

(* affiche la premiere solution *)
let find_first_solution m =
  try 
    iter_solution (fun (o, k) -> ignore(raise (Solution (o, k)))) m; 
    raise NotFound
  with 
    | Solution (o, k) -> print_solution (o, k)
    | NotFound -> Format.printf "No solutions.@."





