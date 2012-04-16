
type node = {

  mutable s: int;
  mutable up: node;
  mutable down: node;
  mutable left: node;
  mutable right: node;
}


let one_node () =
  let rec h = { s = 0; up = h; down = h; left = h; right = h } in
    h

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
let add_row headers row =
  let rec addi_rec n previous = 
    if n = (Array.length row) then 
      () (* si on arrive au bout de la ligne on arrete *)
    else 
      if row.(n) = true then ( (* si la case est à true *)
        let element = one_node () in
          element.s <- -1; (* creation de l'element e ajouter *)
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
let create_headers m h =
  let size = Array.length m in 
  let headers = Array.init size (fun _ -> one_node ()) in
  let rec link_rec n = 
    if n = size then (* en arrivant à la derniere case on arrete*)
      ()
    else begin
      headers.(n).s<-n;
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
let iter_right f n = 
  let first = n in
  f n;
  let rec rec_iter_right node = 
    if node == first then 
      ()
    else begin
      (f node);
      rec_iter_right (node.right)
    end 
  in 
    rec_iter_right (n.right)



(* Applique la fonction f à tout les elements de la list sous n*)
let iter_down f n = 
  let first = n in
    f n;
    let rec rec_iter_down node = 
      if node == first then
        ()
      else begin
        (f node);
        rec_iter_down (node.down)
      end 
    in 
      rec_iter_down (n.down)



(* Cree une matrix doublement chainee a partir d'une matrix *)
let create_linked_matrix m = 
  let h = one_node () in (* creation de la tete *)
  let headers = create_headers m h in (* on recupere les headers *)
    (* On ajoute les lignes en partant de la derniere *)
    for i = Array.length m - 1 downto 0 do
      add_row headers m.(i) (* ajout sous les headers *)
    done;
    h (* retourne la tete *)



let find_solution m =
  let linked_matrix = create_linked_matrix m in
  (* on imprime toutes les colonnes *)
  let print_node node = Format.printf "%d " node.s in
  let print_column n =
    if n != linked_matrix then begin
      iter_down print_node n;
      Format.printf "@."
    end
  in
    iter_right print_column linked_matrix;
    (* l'algo DLX commence ici *)
    assert false


