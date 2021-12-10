type etat = int ;;

type alphabet = char list ;;

type auto = {
  taille : int ;
  init : etat ;
  final : etat list ;
  trans : (char * etat) list array };;

let a1 = {
  taille = 3;
  init = 0;
  final = [2];
  trans = [| [('a',1)];
	     [('b',2)];
	     [('a',2);('b',2)]
	  |]
};;

let a2 = {
  taille = 2;
  init = 0;
  final = [0];
  trans = [| [('a',1);('b',0)];
	     [('a',0);('b',1)]
	  |]
};;

let a3 = {
  taille = 3;
  init = 0;
  final = [1];
  trans = [| [('a',0);('b',1)];
	     [('a',1);('b',2)];
	     [('a',2);('b',0)]
	  |]
};;

let rec mem a l = match l with
  |[] -> false
  |t::q -> if t=a then true
    else mem a q
;;

let rec assoc a l = match l with
  |[] -> raise Not_found
  |(g,d)::q when g = a -> d
  |(g,d)::q -> assoc a q
;;

let lecture_afd a q u =
  let etat = ref q in
  let n = String.length u in
  for k = 1 to n-1 do
    let nouvel_etat = assoc u.[k] (a.trans.(!etat)) in
    etat := nouvel_etat
  done;
  !etat
;;

let recon_afd a u =
  try
    let etatf = lecture_afd a (a.init) u in
    mem etatf (a.final)
  with
    |Not_found -> false
;;

let rec map f l = match l with
  |[] -> []
  |t::q -> (f t)::(map f q)
;;

let rec present fleches = match fleches with
  |[] -> []
  |(l,e)::q -> l::(present q)
;;

let ajoute_fleches sigma puits fleche =
  let pres = present fleche in
  let rec aux l res = match l with
    |[] -> res
    |t::q -> if mem t pres then
	aux q res
      else
	aux q ((t,puits)::res)
  in aux sigma fleche
;;

let completion sigma afd = 
  let fin = afd.final in
  let debut = afd.init in
  let t = afd.taille+1 in
  let transfo = Array.make t [] in
  for k = 0 to (t-2) do
    transfo.(k) <- ajoute_fleches sigma (t-1) afd.trans.(k)
  done;
    transfo.(t-1) <- ajoute_fleches sigma (t-1) [];
  {taille = t;
   init = debut;
   final = fin;
   trans = transfo}
;;

let b1 = completion ['a';'b'] a1;;

let comp_int n l =
  let rec aux k l = 
    if k <= n then
      if mem k l then
        (aux (k+1) l)
      else
	k::(aux (k+1) l)
    else
      []
  in 
  aux 0 l
;;

comp_int 7 [1;4] ;;

let comp sigma auto =
  let b = completion sigma auto in
  {taille = b.taille;
   init = b.init;
   final = comp_int (b.taille-1) b.final;
   trans = b.trans}
;;

comp ['a';'b'] a1;;

let bijection n1 n2 = 
  let rec aux k res = 
    if k > ((n1 * n2) - 1) then
      aux (k + 1) (k::res)
    else
      []
  in 
  aux 0 []
;;

type regexp =
  |Mot of string
  |Concat of regexp * regexp
  |Union of regexp * regexp
  |Etoile of regexp
;;

let e1 = Concat (
  Etoile(
    Union (
      Mot("a"),
      Mot("b")
    )
  ),
  Mot("ababa")
);;

let e2 = Concat (
  Concat (
    Mot("a"),
    Etoile(
      Union ( 
	Union (
	  Mot ("a"),
	  Mot ("b")
	),
	Mot ("c")
      )
    )
  ),
  Mot("c")
);;

let e3 = Concat (
  Etoile (
    Union (
      Union (
	Mot("a"),
	Mot("b")
      ),
      Mot("c")
    )
  ),
  Concat (
    Mot("d"),
    Union (
      Mot("e"),
      Mot("f")
    )
  )
);;

let decompose u =
  let n = String.length u in
  let rec aux k =
  (*aux k retourne la decomposition du suffixe de u commençant en position k*)
    if k = n then 
      []
    else
      (u.[k])::(aux(k+1))
  in
  aux 0
;;

decompose "informatique";;

let rec lettres exp = match exp with
  |Mot(a) -> decompose a
  |Union(g,d)-> (lettres g) @ (lettres d)
  |Etoile (e) -> lettres e
  |Concat (g,d) -> (lettres g) @ (lettres d)
;;

lettres e3;;
  
let rec nb_lettres exp = match exp with
  |Mot(a) -> String.length a
  |Union(g,d)-> (nb_lettres g) + (nb_lettres d)
  |Etoile (e) -> nb_lettres e
  |Concat (g,d) -> (nb_lettres g) + (nb_lettres d)
;;

nb_lettres e3;;
  
let rec denote_epsilon exp = match exp with
  |Mot (a) -> ((decompose a) = [])
  |Union (g,d) -> ((denote_epsilon g) || (denote_epsilon d))
  |Concat (g,d) -> ((denote_epsilon g) && (denote_epsilon d))
  |Etoile _  -> true
;;

denote_epsilon (Etoile (
    Union (
      Union (
	Mot("a"),
	Mot("b")
      ),
      Mot("c")
    )
  ));;
denote_epsilon e3;;

let rec first exp = match exp with
  |Mot (a) -> [a.[0]]
  |Concat (g,d) -> if denote_epsilon g then
      (first g) @ (first d)
    else
      first g
  |Union (g,d) -> (first g) @ (first d)
  |Etoile (e) -> first e
;;

first e3;;

let rec last exp = match exp with
  |Mot (a) -> [a.[(String.length a) -1]]
  |Concat (g,d) -> if denote_epsilon d then
      (last d)  @ (last g)
    else
      (last d)
  |Union (g,d) -> (last g) @ (last d)
  |Etoile (e) -> last e
;;

last e3;;

let rec couple a l = match l with
  |[] -> []
  |t::q -> (a,t)::(couple a q)
;;
 
couple 'a' [1;2;3];;

let rec produit_cartesien l1 l2 = match l1 with
  |[] -> []
  |t::q -> (couple t l2)@(produit_cartesien q l2)
;;

produit_cartesien ['a';'b';'c'] [1;2;3];;

let rec voisin l = match l with
  |[] -> []
  |[a] -> []
  |t1::t2::q -> (t1,t2)::(voisin (t2::q))
;;

voisin [1;2;3;4;5];;

let rec facteurs exp = match exp with
  |Mot a -> voisin (decompose a)
  |Union (g,d) -> (facteurs g) @ (facteurs d)
  |Concat (g,d) -> (facteurs g) @ (facteurs d) @ (produit_cartesien (last g) (first d))
  |Etoile e -> (facteurs e) @ (produit_cartesien (last e) (first e))
;;

facteurs e3;;

let rec numerotation alpha = 
