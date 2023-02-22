let rec partition_bis comp l pivot l1 l2 l3=
  match l with
    []->(l1,l2,pivot::l3)
   |x::r->if x=pivot
          then partition_bis comp r pivot l1 l2 (x::l3)
          else if comp x pivot
          then partition_bis comp r pivot (x::l1) l2 l3
          else partition_bis comp r pivot l1 (x::l2) l3;;
          
let partition comp l pivot=
  partition_bis comp l pivot [] [] [];;

let rec tri_rapide comp l=
  match l with
    []->[]
  |x::r->let (l1,l2,l3)=partition comp r x in
      (tri_rapide comp l1)@l3@(tri_rapide comp l2);; 