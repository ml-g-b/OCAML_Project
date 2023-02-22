let rec echange_adjacents i l=
  match l with
    []->failwith "Erreur echange adjacents : Liste vide"
  |x::r->match r with
      []->failwith "Erreur echange adjacents : Indice trop petit"
    |y::r1->if i<=1
        then y::(x::r1)
        else x::echange_adjacents (i-1) r;;

let rec nieme i l=
  match l with
    []->failwith "Erreur nieme : Liste vide ou indice trop grand"
  |x::r->if i=0
      then x
      else nieme (i-1) r;;

let rec echanger_a_gauche_bis comp i l j=
  match l with
    []->failwith "vide"
  |x::r->if j=0
      then l
      else if comp (nieme j l) (nieme (j-1) l)
      then echanger_a_gauche_bis comp i (echange_adjacents j l) (j)
      else echanger_a_gauche_bis comp i l (j-1);;

let echanger_a_gauche comp i l=
  echanger_a_gauche_bis comp i l ((List.length l) -1);; 

let rec tri_du_nain_bis comp l i=
  match l with
    []->[]
  |x::r->if i =0
      then l
      else tri_du_nain_bis comp (echanger_a_gauche comp i l) (i-1);;

let tri_du_nain comp l=
  tri_du_nain_bis comp l (List.length l);;