let ligne = Array.make 324 false in

let v = ... (* valeur de l'element *)
let i, j = ... (* position : i en abcisse, j en ordonnee*)

(* Case *)
ligne.(9 * (9 * 3 + i) + j) <- true

(* Ligne *)
ligne.(9 * j + v - 1) <- true;

(* Colonne *)
ligne.(9 * (9 + i) + v - 1) <- true;

(* Cellule *)
ligne.(9 * (9 * 2 + (j / 3 + (i / 3) * 3)) +  v - 1) <- true;
