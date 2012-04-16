
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
          element.s <- n; (* creation de l'element e ajouter *)
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
  let headers = Array.init nc (fun _ -> one_node ()) in
  let rec link_rec n = 
    if n = size then (* en arrivant à la derniere case on arrete*)
      ()
    else
      add_right headers.(n - 1) headers(n); (* on lie le header a droite du precedent *)
    link_rec (n + 1) (* suivant *)
  in
    add_right h headers.(0); (* on lie le premier à droite de la tete *)
    link_rec 1; 
    headers (* on retourne le node array *)


(* Cree une matrix doublement chainee a partir d'une matrix *)
let create_linked_matrix m = 
  let h = one_node () in (* creation de la tete *)
  let headers = create_headers m h in (* on recupere les headers *)
  let rec add_rows_rec i = (* On ajoute les lignes en partant de la derniere *)
    if i = 0 then 
      ()
    else (
      add_row headers m.(i); (* ajout sous les headers *)
      add_rows_rec (i - 1) (* ligne suivante *)
    )
  in
    add_rows_rec (Array.length m);
    h (* retourne la tete *)

let find_solution m =
  let linked_matrix = create_linked_matrix m in


    (* l'algo DLX commence ici *)
    assert false


