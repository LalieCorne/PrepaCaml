type 'a arbre_parfait = 'a array
type noeud = int

(*Question n°1*)

let i_racine = 0 ;;
let i_gauche i = 2 * (i + 1) - 1 ;;
let i_droit i = (i_gauche i) + 1 ;;
let i_pere i = (i + 1) / 2 - 1 ;;

(*Question n°2*)

let permute a n1 n2 =
  let v = a.(n1) in
      if n1 = n2 then
	()
      else
	(a.(n1) <- a.(n2) ;
	 a.(n2) <- v)
;;

(*Question n°3*)

let rec percole_bas a n t =
  if (i_gauche n) > t then 
    ()
  else if (i_gauche n) = t then
    if a.(i_gauche n) > a.(n) then
      (permute a n (i_gauche n);
       percole_bas a (i_gauche n) t)
    else
      ()
  else if a.(n) > a.(i_gauche n) && a.(n) > a.(i_droit n) then
      ()
  else if a.(i_gauche n) > a.(n) then
    (permute a n (i_gauche n);
     percole_bas a (i_gauche n) t)
  else if a.(i_droit n) > a.(n) then
    (permute a n (i_droit n) ;
     percole_bas a (i_droit n) t)
  else 
    ()
;;

(*corrigé?*)
let rec percole_basC a n t =
  if (i_droit n) < t then
    if (a.(i_gauche n) > a.(n) || a.(i_droit n)) > a.(n) then
	if (a.(i_gauche n) > a.(i_droit n)) then
	  (permute a n (i_gauche n);
	   percole_bas a (i_gauche n) t)
	else
	  (permute a n (i_droit n) ;
	   percole_bas a (i_droit n) t)
     else 
      ()
  else if (i_gauche n) < t then
    if a.(i_gauche n) > a.(n) then
      permute a (i_gauche n) n
    else
      ()
  else
    ()
;;

(*Question n°4*)

let rec percole_haut a n =
  if n = i_racine then
    ()
  else if a.(n) > a.(i_pere n) then
    (permute a n (i_pere n);
     percole_haut a (i_pere n))
;;

(*Question n°5*)

let organise a =
  let x = Array.length a in
  for k = x downto 0 do
    percole_bas a k (x-1)
  done
;;

let t = [|9,6,5,8,7,2,1,4,3|];;
let a = organise t ;;
t;;

(*Question n°6*)

let tris_par_tas a = 
