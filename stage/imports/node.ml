type noeud = {
  mutable entete : noeud; (* l'en-tete de la colonne *)
  mutable haut   : noeud; (* noeud au dessus *)
  mutable bas    : noeud; (* noeud en dessous *)
  mutable gauche : noeud; (* noeud a gauche *)
  mutable droite : noeud; (* noeud a droite *)
  nom : string;           (* nom eventuel de la case *)
}
