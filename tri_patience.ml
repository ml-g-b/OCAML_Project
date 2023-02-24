let rec inserer_liste comp e ll=
  match ll with
    []->[[e]]
  |x::r->if comp e (List.hd x)
      then (e::x)::r
      else x::(inserer_liste comp e r);;

let rec former_liste_bis comp l ll=
  match l with
    []->ll
  |x::r->former_liste_bis comp r (inserer_liste comp x ll);;

let former_listes comp l= 
  former_liste_bis comp l [];;

let rec min_tetes comp ll min=
  match ll with
    []->min
  |x::r->if comp (List.hd x) min
      then min_tetes comp r (List.hd x)
      else min_tetes comp r min;;

let rec supprimer_tete e ll=
  match ll with
    []->failwith "Non trouvé"
  |x::r->if e = (List.hd x)
      then if (List.tl x)=[]
        then r
        else (List.tl x)::r
      else x::(supprimer_tete e r);;

let rec fusion_k_voies comp ll=
  match ll with
    []->[]
  |x::r->let minimum = min_tetes comp ll (List.hd x) in
      minimum::(fusion_k_voies comp (supprimer_tete minimum ll));;

let tri_patience1 comp l=
  fusion_k_voies comp (former_listes comp l);;

let rec fusion comp l1 l2=
  match (l1,l2) with
    ([],[])->[]
  |([],y::r2)->l2
  |(x::r1,[])->l1
  |(x::r1,y::r2)->if comp x y
      then x::(fusion comp r1 l2)
      else y::(fusion comp l1 r2);;

let rec fusion_2_a_2_aux comp ll=
  match ll with
    []->[]
  |[x]->[x]
  |x::(y::r)->(fusion comp x y)::(fusion_2_a_2_aux comp r);;
  
let rec fusion_2_a_2 comp ll=
  if List.length ll = 1
  then ll
  else fusion_2_a_2 comp (fusion_2_a_2_aux comp ll);;

let tri_patience2 comp l=
  fusion_2_a_2 comp (former_listes comp l);;
  
