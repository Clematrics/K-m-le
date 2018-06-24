type type_simple =
  |Variable of variable_de_type
  |Integer
  |Float
  |Boolean
  |Charac
  |String
  |Unit
  |Fleche of type_simple * type_simple
  |Somme of (string * type_simple) list
  |Produit of type_simple list
  |List of type_simple
  |Array of type_simple
		       
and variable_de_type = 
  {mutable age : int;
   mutable valeur : valeur_var}

and valeur_var =
  |Inconnue
  |Connue of type_simple;;

type schema =
    {parametres : variable_de_type list;
     corps : type_simple};;

exception Conflit;;

exception Circularite;;
  
(* J'aurai besoin de do_list, qui n'est pas dans la bibliothèque standard ni dans la bibliothèque List, donc je la recode. *)

let rec do_list f = function
  |[] -> ()
  |t::q -> f t; do_list f q;;
  
let rec trouver_valeur = function
  |Variable ({age = _  ; valeur = Connue t} as v)
   -> let valeur_t = trouver_valeur t in
      v.valeur <- Connue valeur_t;
      valeur_t
  |ty -> ty;;

let occurrence var ty = (* Cette fonction lève une exception en cas de création de cycles dans les types. Sinon, elle ne fait rien. *)
  let rec verif t = match trouver_valeur t with
    |Variable var2 -> if var == var2 then raise Circularite
    |Fleche(t1,t2) -> verif t1; verif t2
    |Somme l -> do_list (function (x,y) -> verif y) l
    |Produit l -> do_list verif l
    |List t -> verif t
    |Array t -> verif t
    |_ -> ()
  in verif ty;;

let rec lisser_age agemaxi t = match trouver_valeur t with
  |Variable v -> if v.age > agemaxi then v.age <- agemaxi
  |Fleche(t1,t2) -> lisser_age agemaxi t1; lisser_age agemaxi t2
  |Somme l -> do_list (function (x,y) -> lisser_age agemaxi y) l
  |Produit l -> do_list (lisser_age agemaxi) l
  |List t -> lisser_age agemaxi t
  |Array t -> lisser_age agemaxi t
  |_ -> ();;

let rec unification t1 t2 = (* Cette fonction permet de résoudre l'équation de types t1 = t2  *)
  let v1 = trouver_valeur t1
  and v2 = trouver_valeur t2 in
  if v1 != v2 then begin (* Si les deux types sont déjà égaux, il n'y a rien à faire.  *)
    match (v1,v2) with
    |Variable v, t -> occurrence v t; (* On vérifie d'abord qu'il n'y a pas de "cycle" dans l'arbre des types  *)
		      lisser_age v.age t;
		      v.valeur <- Connue t
    |t, Variable v -> occurrence v t;
		      lisser_age v.age t;
		      v.valeur <- Connue t
    |Fleche(t1,t2), Fleche(s1,s2) ->
      unification t1 s1;
      unification t2 s2
    |Produit l1, Produit l2 ->
      (match l1, l2 with
      |[],[] -> ()
      |t1::q1, t2::q2 -> unification t1 t2;
			 unification (Produit q1) (Produit q2)
      |_ -> raise Conflit) (* Si les deux types produits n'ont pas autant de composantes, il y a une erreur  *)
    |Somme l1, Somme l2 ->
      (match l1, l2 with
      |[],[] -> ()
      |(t1,_)::q1, (t2,_)::q2 -> unification t1 t2;
			 unification (Somme q1) (Somme q2)
      |_ -> raise Conflit)
    |List t, List s -> unification t s
    |Array t, Array s -> unification t s
    |_,_ -> raise Conflit (* Si on a des constructeurs différents, c'est qu'il y a conflit de type. *)
		end;;

let rec mem_physique elt = function
  |[] -> false
  |t::q when elt == t -> true
  |t::q -> mem_physique elt q;;

let rec assoc_physique elt = function
  |[] -> raise Not_found
  |(a,b)::q when a == elt -> b
  |_::q -> assoc_physique elt q;;

let age_courant = ref 0;;

let debut_de_definition = 
  incr age_courant;;

let fin_de_definition = 
  decr age_courant;;

let nouvelle_inconnue = 
  Variable {age = !age_courant; valeur = Inconnue};;

let generalisation t = 
  let parametres_t = ref [] in
  let rec aux s = match trouver_valeur t with
    |Variable v -> if (v.age > !age_courant) && (not mem_physique v !parametres_t)
             then parametres := v::!parametres_t 
    |Fleche s1 s2 -> aux s1; aux s2
    |Somme l -> do_list (function (x,y) -> aux y) l
    |Produit l -> do_list aux l
    |List s -> aux s
    |Array s -> aux s
    |_ -> ()
  in aux t;
  {parametres = parametres_t; corps = t};;

let schema0 t = {parametres = []; corps = t};;

let specialisation schema_de_type = match schema_de_type.parametres with
  |[] -> schema_de_type.corps
  |p -> let inconnues = List.map (function v -> (v, nouvelle_inconnue)) p in
    let rec copie t = match trouver_valeur t with 
      |Variable v -> try assoc_physique v inconnues with Not_found -> t 
      |Fleche s1 s2 -> Fleche (copie s1) (copie s2)
      |Somme l -> Somme (do_list (function (x,y) -> copie y) l)
      |Produit l -> Produit (do_list copie l)
      |List s -> List (copie s)
      |Array s -> Array (copie s)
    in copie schema_de_type.corps;;