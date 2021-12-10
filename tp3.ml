type 'a abr =
  |Feuille of int
  |Noeud of 'a abr * 'a * 'a abr

(*Question n°1 c*)
let rec mem v a = match a with
  |Feuille x -> (x=v)
  |Noeud (g, x ,d) -> 
    if v <= x then 
      mem v g
    else
      mem v d
;;

let arbre = Noeud (
  Noeud (
    Noeud (
    ),
  50,
  Noeud ())
;;

(*Question n°2 c*)
let rec insere v a = match a with
  |Feuille x -> 
    if v = x then 
      Feuille x
    else
      if v > x then
	Noeud (Feuille x , x , Feuille v)
      else
	Noeud (Feuille v , v , Feuille x)
  |Noeud (g, x, d) -> 
    if v <= x then 
      Noeud(insere v g, x, d)
    else
      Noeud (g, x, insere v d)
;;

let a1 = Feuille 5;;
let a2 = insere 3 a1;;
let a3 = insere 25 a2;;
let a4 = insere 34 a3;;
let a5 = insere 79 a4;;

(*Question n°3*)
let rec supprime  v a = 
  if mem v a then
    match a with
      |Feuille _ -> failwith "arbre vide"
      |Noeud (Feuille a, x, Feuille b) ->
	if v = a then
	  Feuille b
	else
	  Feuille a
      |Noeud (Feuille a, x, d) ->
	if v = a then
	  d
	else
	  Noeud (Feuille a, x, supprime v d)
      |Noeud (g, x, Feuille b) ->
	if v = b then
	  g
	else
	  Noeud (supprime v g, x, Feuille b)
      |Noeud (g, x, d) -> 
	if v <= x then 
	  Noeud (supprime v g, x, d)
	else
	  Noeud (g, x, supprime v d)
  else
    failwith "pas dans l'arbre"
;;
(*correction Question n°3*)
let rec supprimeC v a = match a with
  |Feuille _ -> failwith "feuille"
  |Noeud(g,x,d) -> 
    if g = Feuille v then
      d
    else if d = Feuille v then
      g
    else if v <= x then 
      Noeud (supprime v g, x, d)
    else
      Noeud (g, x, supprime v d)
;;

let a6 = supprime 2 a3;;
let a7 = supprime 3 a5;;
let a8 = supprime 35 a7;;
let a9 = supprime 5 a1;; 

(*Question n°4*)
let rec liste_Feuille a =
  let list = [] in  
  match a with
    |Feuille x -> list @ [x]
    |Noeud (g , x, d) -> list@(liste_Feuille g)@(liste_Feuille d)
;;

let rec insere_list l a = match l with
  |[] -> a
  |t::q -> insere_list q (insere t a)
;;

let triabr l = match l with
  |[] -> []
  |t :: q -> liste_Feuille (insere_list q (Feuille t))
;;
let list = [2;6;8;0;4;3];;
let list2 = triabr list;;
