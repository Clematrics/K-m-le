type lexeme = 
	|Entier of int
	|Flottant of float
	|MotClef of string
	|Identificateur of string 
	|Charactere of char 
	|Chaine of string;; 

(* Les chaînes de caractères sont reconnues comme un type natif, et non comme des tableaux de caractères, 
mais cela n'influe pas sur la production de code *)

exception ParseError;;

(* Les erreurs de parsing, contrairement aux erreurs pointées lors de l'analyse syntaxique, ne seront pas détaillées. *)

let rec lire_entier accu flux = match flux with parser 
	|[< 'c when 0 <= (int_of_char c - 48) && (int_of_char c - 48) <= 9 >] -> lire_entier (10*accu + (int_of_char c) - 48) flux  
	|[< >] -> accu;;

let rec lire_decimales accu div flux = match flux with parser
	|[< 'c when 0 <= (int_of_char c - 48) && (int_of_char c - 48) <= 9 >] -> 
		lire_decimales (accu +. (float_of_int(int_of_char c - 48))/.div) (div*.10.0) flux
	|[< >] -> accu;;

let lire_nombre accu flux = match flux with parser
	|[< partie_entiere = lire_entier accu ; reste >] -> match reste with parser
			|[< ''.' ; decimales = lire_decimales 0.0 10.0 >] -> Flottant (float_of_int partie_entiere +. decimales) 
			|[< >] -> Entier partie_entiere;;
			
let lire_charactere flux = match flux with parser 
	|[< 'c when fonctionnelles.iscarac c ; ''\'' >] -> c;;

let rec lire_chaine accu flux = match flux with parser 
	|[< 'c when fonctionnelles.iscarac c >] -> lire_chaine (accu ^ (String.make 1 c)) flux  
	|[< ''\"' >] -> accu;; (* gère aussi le cas de la chaîne vide *)

let rec lire_mot id accu flux = match flux with parser
	|[< 'c when fonctionnelles.valid_id c >] -> lire_mot id (accu ^ (String.make 1 c)) flux 
	|[< 'c when (not (fonctionnelles.iswhite c)) >] -> lire_mot false (accu ^ (String.make 1 c)) flux 
	|[< >] -> (id, accu);;

let mot_clef_ou_identificateur mot table = 
	try Hashtbl.find table mot with Not_found -> Identificateur mot;;

let mot_clef mot table = 
	Hashtbl.find table mot;;

let rec lire_commentaire flux = match flux with parser
	|[< ''*'; '')' >] -> ()
	|[< 'c >] -> lire_commentaire flux ;;

(* J'ai adopté la convention suivante : un identificateur doit commencer par une lettre, contenir des lettres ou des chiffres ou des _ *)

let rec lire_lexeme table_mots_clefs flux = match flux with parser 
	|[< 'c when fonctionnelles.iswhite c >] -> lire_lexeme table_mots_clefs flux
	|[< ''(' ; ''*' >] -> lire_commentaire flux; lire_lexeme table_mots_clefs flux
	|[< ''\'' ; c = lire_charactere >] -> Charactere c (* Ces deux cas-là sont différents des autres en raison du caractère de fin *)
	|[< ''\"' ; c = lire_chaine "" >] -> Chaine c
	|[< 'c when fonctionnelles.isnumber c >] -> lire_nombre (int_of_char c - 48) flux
	|[< 'c when fonctionnelles.isletter c >] -> let b, mot = lire_mot true (String.make 1 c) flux in 
	if b then mot_clef_ou_identificateur mot table_mots_clefs
	else try mot_clef mot table_mots_clefs with Not_found -> raise ParseError
	|[< 'c when issym c >] -> let sym = lire_symboles (String.make 1 c) flux in try
	mot_clef sym table_mots_clefs with Not_found -> raise ParseError;;

let rec analyseur_lexical table_mots_clefs flux = try (match flux with parser
	|[< l = lire_lexeme table_mots_clefs ; r = analyseur_lexical table_mots_clefs >] 
		-> [< [< 'l >] ;r >]
	|[< ''@' >] -> [< >])
	with
	|_ -> raise ParseError;; 

let construire_analyseur liste_mots_clefs = 
	let table_mots_clefs = Hashtbl.create (List.length liste_mots_clefs) in
	do_list (function mc -> Hashtbl.add table_mots_clefs mc (MotClef mc)) liste_mots_clefs;
	analyseur_lexical table_mots_clefs;;