type 'a abr2 =
  |Vide
  |Noeud of ('a abr2 * 'a * 'a abr2)
;;

(*Question n°5*)
let rec mem2 v a = match a with
  |Vide -> failwith "arbre vide"
  |Noeud (g , x , d) ->
    if v = x then
      (v=x)
    else 
      if v < x then
	mem2 v g
      else
	mem2 v d
;;

(*Question n°6*)
let rec  insere v a = match a with
  |Vide ->  Noeud(Vide, v, Vide)
  |Noeud (Vide, x, Vide) -> 
    if v = x then 
      Noeud (Vide, x, Vide)
    else
      if v > x then
	Noeud (Vide , x , Noeud(Vide, v, Vide))
      else
	Noeud (Noeud(Vide, v, Vide) , v , Vide)
  |Noeud (g, x, d) -> 
    if v = x then 
      Noeud (g, x, d)
    else
      if v<x then
	Noeud(insere v g, x, d)
      else
	Noeud (g, x, insere v d)
;;

let abr1 = Vide;;
let abr2 = insere 5 abr1;;
let abr3 = insere 63 abr2;;
let abr4 =  insere 1 abr3;;

(*Question n°7*)
let rec min a =
  let min = 
match a with 
  |Vide -> 
  |Noeud (Vide, x, Vide) -> x 
let rec supprime v a = match a with
  |Noeud (Vide, x, Vide) -> 
    if v = x then 
      Vide
    else 
      Noeud (Vide, x, Vide)
  |Noeud (Vide , x, d) -> 
    if v=x then
      d
    else 
      Noeud(Vide, x, supprime v d)
  |Noeud (g , x, Vide) ->
    if v=x then
      g
    else 
      Noeud ( supprime v g , x , Vide)
  |Noeud(g , x, d) ->
    if v=x then
      
