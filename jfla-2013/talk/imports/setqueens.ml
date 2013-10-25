let line = Array.make 46 false in

let i, j = ... (* Position choisie *)
  
(* Ligne *)
ligne.(i) <- true; 

(* Colonne *)
ligne.(n + j) <- true;

(* Diagonale gauche-droite *)
ligne.(2 * n + i + j) <- true;

(* Diagonale droite-gauche *)
ligne.(4 * n - 1 + n - 1 - i + j) <- true;
