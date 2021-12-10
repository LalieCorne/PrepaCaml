
let g = [|
  [| 0 ; 1 ; 0 ; 0 |];
  [| 0 ; 0 ; 0 ; 0 |];
  [| 1 ; 1 ; 0 ; 0 |];
  [| 0 ; 0 ; 1 ; 0 |]
	|]
;;
Array.length g;;

type poids = Valeur of int 
	     | Inf;;

let p = [|
  [| Inf ; Valeur(5) ; Inf ; Inf |];
  [| Inf ; Inf ; Inf ; Inf |];
  [| Valeur(1) ; Valeur(4) ; Inf ; Inf |];
  [| Inf ; Inf ; Valeur(3) ; Inf |]
	|]
;;


(*exo 1*)

let degout graph s =
  let somme = ref 0 in
  for k = 0 to (Array.length graph - 1) do
    somme := graph.(s).(k) + !somme
  done;
  !somme
;;

degout g 2;;

let degin graph s =
  let somme = ref 0 in
  for k = 0 to (Array.length graph - 1) do
    somme := graph.(k).(s) + !somme
  done;
  !somme
;;

degin g 2;;


(*exo 2*)

let rec cheminValide graph c = match c with
  |[] -> failwith "liste vide"
  |[_] -> true
  |[x;y] -> graph.(x).(y) = 1
  |t1::t2::q -> (graph.(t1).(t2) = 1) && (cheminValide graph (t2::q))
;;

cheminValide g [0;1;2;3];;
cheminValide g [3;2;1];;


(*exo 3*)
let rec coutChemin g p c = match c with
  |[] -> failwith "liste vide"
  |[_] -> 0
  |t1::t2::q -> (
    match p.(t1).(t2) with
      |Valeur (a) -> a + coutChemin g p (t2::q)
      |Inf -> failwith "infini"
  )
;;

coutChemin g p [0;1;2;3];;
coutChemin g p [3;2;1];;

(* exo 4*)

let g = Array.make_matrix 10 10 0;;
let p = Array.make_matrix 10 10 Inf;;
let add_arc i j a =
  g.(i).(j) <- 1;
  p.(i).(j) <- Valeur (a)
;;

add_arc 0 1 6;
add_arc 0 2 5;
add_arc 1 3 4;
add_arc 1 4 7;
add_arc 2 3 2;
add_arc 2 5 6;
add_arc 3 6 6;
add_arc 4 6 1;
add_arc 4 8 8;
add_arc 5 6 4;
add_arc 5 7 7;
add_arc 5 9 15;
add_arc 6 8 9;
add_arc 6 7 6;
add_arc 8 9 3;;

g;;
p;;


(*exo 5*)

let accessible g x y =  
  let n = Array.length g in
  let visite = Array.make n false in
  let rec parcours i =
    if not (visite.(i)) then 
      (
	visite.(i) <- true;
	for k = 0  to (n-1) do
	  if g.(i).(k) = 1 then
	    parcours k
	done
      )
  in parcours x;
  visite.(y)
;;


(*exo 6*)

