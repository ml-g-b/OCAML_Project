let rec renverse_bis l lf=
  match l with
    []->lf
  |x::r->renverse_bis r (x::lf);;

let renverse l=
  renverse_bis l [];;

let rec divise_en_deux_bis l i l1 l2=
  match l with
    []->(renverse l1,l2)
  |x::r->if i>=0
      then divise_en_deux_bis r (i-1) (x::l1) l2
      else divise_en_deux_bis [] (i-1) l1 l;;

let divise_en_deux l=
  divise_en_deux_bis l (((List.length l)-1)/2) [] [];; 

let rec fusion comp l1 l2=
  match (l1, l2) with
    ([],[])->[]  
  |([],y::r2)->l2
  |(x::r1,[])->l1
  |(x::r1,y::r2)->if comp x y
      then x::(fusion comp r1 l2)
      else y::(fusion comp l1 r2);;


let rec tri_fusion comp l=
  match l with
    []->[]
  |[x]->l
  |x::r->let (l1,l2)=divise_en_deux l in 
      fusion comp (tri_fusion comp l1) (tri_fusion comp l2);; 
