let x = 1 + 5 ;;
12 * 125 ;;
2 + 7 * 5 ;;
let a = 2 + 7 * 5 ;;
a * a ;;

(4 + 5) * (7 - 4) ;;
3.5 +. 7. ;;
1.5 ** 20. ;;
3 / 2 + 1 ;;
3. /. 2. +. 1. ;;
(1. +. sqrt 5.) /. 2. ;;
(log 2.) *. (cos 1.) ;;
(exp (-1.)) +. (exp 1.) ;;
(abs (int_of_float (sqrt 5.))) + 2 ;;

let x = 1 in x + 1 ;;
let y = 2 in let x = y in x + y ;;
let y = 3 in let x = y + 2 ;;

(*partie 2.3 functions*)
(*function somme*)
let somme x y  =
  x + y
;;
somme 5 7 ;;
somme 2.5 3 ;;

(*question 1*)
let successeur x =
  x + 1
;;
successeur 2;;

(*question 2*)
let carre x =
  x * x
;;
carre 5;;

(*question 3*)
let puissance4 x =
carre (carre x)
;;
puissance4 2;;

(*question 4*)
let sinush x =
  ((exp x) -. (exp (-.x))) /. 2.
;;

(*question 5*)
let diveucli x y =
  let q = a/b in
  let r = a mod b in
  let qtxt = string_of_int q
  "La division euclidienne de " ^ (string_of_int x) ^ " par " ^ (string_of_int y) ^ " a pour quotient " ^ qtxt ^ " et reste " ^ (string_of_int r)
;;
diveucli 7 3 ;;

(*partie 3.1*)
(*question 1*)
let est_bisextile année =
  if année mod 400 = 0
  then
    true
  else 
    (if année mod 4 = 0
     then
	(if année mod 100 = 0
	 then
	    false
	 else
	    true)
     else
	false)
;;

(*question 2*)
let est_bisextile année =
  if année mod 400 = 0 ||(année mod 4 = 0 && (not (année mod 100 = 0)))
  then
    true
  else
    false
;;

(*partie 3.2*)
let est_poisson jour mois =
  if mois = 2
  then 
    (if jour >= 20 
    then 
      true
    else
      false)
  else 
    (if mois = 3
     then
	(if jour <= 20
	 then
	    true
	 else 
	    false)
     else 
	false)
;;
let poisson jour mois =
  if ((jour <= 20) && (mois = 3)) || ((jour >= 20) && (mois = 2))
  then
    true
  else
    false
;;

(*partie 4.0*)
(*question 1*)
let rec  somme_entiers n =
  if n =  0 
  then
    n
  else
  n + (somme_entiers (n-1))
;;
somme_entiers 3 ;;

(*question 2*)
let rec calcul_PGCD a b =
  if a = 0
  then b
  else
    (let a = b in
    let b = a mod b in
    calcul_PGCD a b)
;;
calcul_PGCD 12 6 ;;
calcul_PGCD 6 8 ;;
