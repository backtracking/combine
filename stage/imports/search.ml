(*
solutions : un tableau contenant des lignes correspondant a la solution
k : numero de la ligne dans le tableau de solutions

recouvre : retire une colonne entiere avec la solution montree
precedemment, ainsi que toute les lignes ou la valeur est '1'

decouvre : replace une colonne dans la matrice

*)
let rec recherche f k entete solutions = 
  if h == h.droite then f (solutions, k)
  (* condition d'arret : matrice vide *)
  else 
    let colonne = choisir_colonne h in
    let descendre r = 
      solutions.(k) <- r;
      iter_droite  (fun j -> recouvre j.entete) r;
      recherche f (k + 1) h solutions;
      iter_gauche  (fun j -> decouvre j.entete) r
    in 
      recouvre colonne;
      iter_bas descendre colonne;
      decouvre colonne
