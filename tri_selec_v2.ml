
let rec minimums_bis comp l e i j=
  match l with
    []->(e, i)
  |x::r->if comp x e
      then minimums_bis comp r x (i+1+j) 0
      else minimums_bis comp r e (i) (j+1);;

let minimums comp l=
  match l with
    []->failwith "Liste vide"
  |x::r->minimums_bis comp r x 0 0;;

let rec placer e i l=
  match l with
    []->[]
  |x::r->if i=0
      then e::r
      else x::(placer e (i-1) r);;


let rec tri_selection_v2 comp l=
  match l with 
    []->failwith "Liste vide"
  |[x]->[x]
  |x::r->let (min, i)=minimums comp l in
      min::(tri_selection_v2 comp (placer x (i-1) r));;