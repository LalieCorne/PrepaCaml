type 'a stack = {
  sempty : unit -> bool ; (* teste si la pile est vide*)
  spush : 'a -> unit ; (* ajout un élément au début de la pile *)
  spop : unit -> 'a ; (* retire un élément au début de la pile *)
  ssize : unit -> int (* donne la longueur de la pile *)
}
;;

(* question 1 *)
let new_stack () =
  let data = ref [] in
  let empty () = 
    (!data) = []
  in
  let push x =
    let newdata = x :: (!data) in
    data := newdata
  in   
  let pop () = match (!data) with
    |[] -> failwith "liste vide"
    |t :: q -> ( data := q ; t )
  in 
  let rec size () = 
    List.length (!data)
  in
  { sempty = empty;
      spush = push;
      spop = pop;
      ssize = size
  }
;;
(* testes question 1 *)
let p = new_stack ();;
p.spush 7 ;;
p.spush 6 ;;
p.spush 5 ;;
p.spush 4 ;;
p.sempty () ;;
p.ssize () ;;
p.spop () ;;
p.spop () ;;
p.spop () ;;
p.spop () ;;
p.sempty () ;;

(* fin question 1 *)

type 'a queue = {
  qempty : unit -> bool ; (* teste si la file est vide*)
  qpush : 'a -> unit ; (* ajout un élément à la fin de la file *)
  qpop : unit -> 'a ; (* retire un élément au début de la file *)
  qsize : unit -> int (* donne la longueur de la file *)
}

(* question 2 *)
let new_queue () =
  let data = ref [] in
  let empty () =
    (!data) = []
  in
  let push x =
    data := (!data) @ [x]
  in
  let rec pop () = match (!data) with
    |[] -> failwith "liste vide"
    |t :: q -> ( data := q ; t )
  in
  let rec size () = 
    List.length (!data)
  in
  { qempty = empty;
      qpush = push;
      qpop = pop;
      qsize = size
  }
;;

(* testes question 2 *)
let f = new_queue ();;
f.qpush 7 ;;
f.qpush 6 ;;
f.qpush 5 ;;
f.qpush 4 ;;
f.qempty () ;;
f.qsize () ;;
f.qpop () ;;
f.qpop () ;;
f.qpop () ;;
f.qpop () ;;
f.qempty () ;;

(* fin question 2 *)

type ( 'f , 'n ) arbre =
  |Feuille of 'f
  |Noeud of 'n * ( 'f , 'n ) arbre * ( 'f , 'n ) arbre
;;

type ( 'f , 'n ) token =
  |F of 'f
  |N of ' n
;;

type ( 'f , 'n ) parcous = ( 'f , 'n ) token list
;;

(* question 3 *)
let arbreA = Noeud ( 'a' , 
		Noeud ( 'b' , 
			Feuille 1 , 
			Noeud ( 'd' , 
				Feuille 4 , 
				Noeud ( 'e' , 
					Feuille 5 , 
					Feuille 6 
				)
			)
		),
		Noeud ( 'c' , 
			Feuille 2 , 
			Feuille 3
		)
)
;;

(* fin question 3 *)

(* question 4 *)
let rec taille arbre = match arbre with
  |Feuille _ -> 0
  |Noeud (n, g , d) -> 1 + (taille g) + (taille d)
;;

let rec profondeur arbre = match arbre with
  |Feuille _ -> 0
  |Noeud (n, g , d) -> 1 + max (profondeur g) (profondeur d)
;;

(* testes question 4 *)
let t = taille arbreA
;;
let p = profondeur arbreA
;;

(* fin question 4 *)

(* question 5 *)
let rec largeur arbre = 
  let parcours = ref [] in
  let file = new_queue () in
  file.qpush arbre,
  while not (f.qempty ()) do
    let x = file.qpop () in match x with
      |Feuille (y) -> parcours := (F y)::(!parcours)
      |Noeud (y, g , d) -> (file.qpush (g) ; file.qpush (d) ; (parcours := (N y)::(!parcours)))
  done
    ; List.rev !parcours
;;
