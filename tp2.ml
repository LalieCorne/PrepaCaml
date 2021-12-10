(*partie 1*)
let somme = 2 + 7 ;;

let carre x = x * x ;;

let carre2 x = x *. x ;;

let derive f x =
    let e = 1E-6 in
    	((f (x +. e)) -. (f x)) /. e ;;

let euclide a b = (a / b, a mod b) ;;

let somme_zero f g = (f 0) + (g 0) ;;

let somme f g = fun x -> (f x) + (g x) ;;

let norme vecteur =
    let x = fst vecteur in
    let y = snd vecteur in
    	sqrt (x *. x +. y *. y) ;;

let f a b =
    let c = fst a in
    let d = snd a in
    let e = fst c in
    let f = snd c in
    (e (f +. 3.)) -. (d ( b * 2)) ;;

type personne = {
  prenom : string ; 
  age : int
} ;;
let message qqun =
  "Bonjour "
  ^ qqun.prenom
  ^ " ! Vous avez "
  ^(string_of_int qqun.age)
  ^ "ans." ;;

(*question 1.*)
3. -. 2. ;;

(*question 2.*)
'a' ;;

(*question 3.*)
"Maurice" ;;

(*question 4.*)
(1 , 5.3 , "Licorne") ;;

(*question 5.*)
let somme x = x + 3 ;;

(*question 6.*)
let ajout x y = x + y ;;

(*question 7.*)
let fonction f = f (1 + 2) * 2 ;;

(*question 8.*)
let chaine f = 
  let fct x =
    x -. 3.
  in string_of_int ((f fct) +. 2.)
  

(*question 9.*)
let mod a = (fst a / 2 , snd a + 1) ;;

(*question 10.*)
(1 , (fun x -> x - 12) , 2);;


(*partie 2*)
(*question 1.*)
type trinome = {
  a : float ;
  b : float ;
  c : float
} ;;

(*question 2.*)
let discriminant trin =
  (trin.b ** 2.) -. (4. *. trin.a *. trin.c) ;;

(*question 3.*)
let somme trin1 trin2 =
  {a = trin1.a +. trin2.a ; b = trin1.b +. trin2.b ; c = trin1.c +. trin2.c} ;;

(*question 4.*)
let applique trin x =
  trin.a *. (x ** 2.) +. trin.b *. x +. trin.c ;;

(*question 5.*)
let q = {a = 1. ; b = -1. ; c = -1.};;

(*question 6.*)
applique q ;;

(*question 7.*)
let image_zero trin = applique trin 0.;;

(*question 8.*)
type delta = Positif | Negatif | Nul ;;
let racines trin = 
  let delta = discriminant trin in
  match delta with
    |Positif -> 
      ((-. trin.b -. sqrt delta)/. (2. *. trin.a) , (-. trin.b +. sqrt delta) /. (2. *. trin.a))
    |Negatif -> failwith "delta négatif"
    |Nul -> -. trin.b /. (2. *. trin.a)
;;
