type lexeme = 
	|Entier of int
	|Flottant of float
	|MotClef of string
	|Identificateur of string 
	|Charactere of char 
	|Chaine of string;; (* Les chaînes de caractères sont reconnues comme un type natif, et non comme des tableaux de caractères, 
						mais cela n'influe pas sur la production de code *)

let print_lexeme l = match l with
	|Entier n -> print_string "Entier "; print_int n
  	|Flottant f -> print_string "Flottant "; print_float f
  	|MotClef mc -> print_string "MotClef "; print_string mc
  	|Identificateur id -> print_string "Identificateur "; print_string id
  	|Charactere c -> print_string "Caractere "; print_char c
  	|Chaine s -> print_string "Chaine "; print_string s;;

let print_stream flux = 
  	let rec print_reste flux = 
  	try (let l = Stream.next flux in print_lexeme l; print_string "; "; print_reste flux)
  	with _ -> print_string " >]"
  	in print_string "[< "; print_reste flux;;

(* La fonction print_stream comporte un bug : elle affiche un ; en plus à la fin du flux. 
Cependant, cette fonction ne sert qu'à afficher les exemples et n'a aucune incidence sur le compilateur. Je n'ai donc pas cherché à corriger cette erreur.  *)
  
exception ParseError;;

(* Les erreurs de parsing, contrairement aux erreurs pointées lors de l'analyse syntaxique, ne seront pas détaillées. *)

let rec lire_entier accu flux = match flux with parser 
	|[< 'c when 0 <= (int_of_char c - 48) && (int_of_char c - 48) <= 9 >] -> 
		lire_entier (10*accu + (int_of_char c) - 48) flux  
	|[< >] -> accu;;

let rec lire_decimales accu div flux = match flux with parser
	|[< 'c when 0 <= (int_of_char c - 48) && (int_of_char c - 48) <= 9 >] -> 
		lire_decimales (accu +. (float_of_int(int_of_char c - 48))/.div) (div*.10.0) flux
	|[< >] -> accu;;

let lire_nombre accu flux = match flux with parser
	|[< partie_entiere = lire_entier accu ; reste >] -> match reste with parser
			|[< ''.' ; decimales = lire_decimales 0.0 10.0 >] -> 
				[< 'Flottant (float_of_int partie_entiere +. decimales) >]
			|[< >] -> [< 'Entier partie_entiere >];;
			
(* Les trois suivants (caractere, chaine, commentaire) ont ceci en commun qu'ils sont délimités dans le code par deux mots-clefs ou
suites de mots-clefs "bornes", que ce soit les apostrophes, les guillemets ou les (* *) *)

let lire_charactere flux = match flux with parser 
	|[< 'c when isletter c ; ''\'' >] -> c;;

let rec lire_chaine accu flux = match flux with parser 
	|[< 'c when (isalphanum c || issym c) >] -> lire_chaine (accu ^ (String.make 1 c)) flux  
	|[< ''\"' >] -> accu;; (* gère aussi le cas de la chaîne vide *)

let rec lire_commentaire flux = match flux with parser
	|[< ''*'; '')' >] -> ()
	|[< 'c >] -> lire_commentaire flux ;;

let rec lire_mot id accu flux = match flux with parser
	|[< 'c when valid_id c >] -> lire_mot id (accu ^ (String.make 1 c)) flux 
	|[< 'c when (not (iswhite c)) >] -> lire_mot false (accu ^ (String.make 1 c)) flux 
	|[< >] -> (id, accu);;

let mot_clef_ou_identificateur mot table = 
	try Hashtbl.find table mot with Not_found -> [< 'Identificateur mot >];;

let mot_clef mot table = 
	Hashtbl.find table mot;;

let est_mot_clef mot table =
	try (let a = Hashtbl.find table mot in true) with Not_found -> false;;

let table_symboles = ['.' ; '&' ; '+' ; '=' ; '-' ; '*' ; '/' ; '\"' ; '%' ; '|' ; '<' ; '>' ; '_' ; '(' ; ')' ; ';'];;

let rec recuperer_symboles accu flux = match flux with parser 
	|[< 'c when List.mem c table_symboles >] -> recuperer_symboles (accu ^ (String.make 1 c)) flux
	|[< >] -> accu;;

let decouper_mot_clef s table = 
	let rec aux dernier_ind = function 
		|i when i = String.length s -> dernier_ind
		|i -> if est_mot_clef (String.sub s 0 i) table then aux i (i+1) else aux dernier_ind (i+1)
	in if est_mot_clef s table then (s,"") (* on vérifie d'abord si s tout entier est un mot-clef pour éviter une erreur dans le deuxième
	appel à String.sub *)
	else let ind = aux (-1) 1 in (String.sub s 0 ind, String.sub s (ind) ((String.length s) -1));;

let decouper_symboles s table =
	let rec aux accu = function 
		|s when String.length s = 0 -> accu
		|s -> try (let mc,reste = decouper_mot_clef s table in aux [< accu ; [< 'MotClef mc >] >] reste) with _ -> raise ParseError
	in aux [< >] s;;

(* J'ai adopté la convention suivante : un identificateur doit commencer par une lettre, contenir des lettres ou des chiffres ou des _ *)

let rec lire_lexeme table_mots_clefs flux = match flux with parser 
	|[< 'c when iswhite c >] -> lire_lexeme table_mots_clefs flux
	|[< ''(' ; ''*' >] -> lire_commentaire flux; lire_lexeme table_mots_clefs flux
	(* Les deux prochains cas sont différents à cause des mots-clefs "bornes". *)
	|[< ''\'' ; c = lire_charactere >] -> [< 'Charactere c >] 
	|[< ''\"' ; c = lire_chaine "" >] -> [< 'Chaine c >]
	|[< 'c when isnumber c >] -> lire_nombre (int_of_char c - 48) flux
	|[< 'c when isletter c >] -> let b, mot = lire_mot true (String.make 1 c) flux in 
	if b then mot_clef_ou_identificateur mot table_mots_clefs
	else (try mot_clef mot table_mots_clefs with Not_found -> raise ParseError)
	|[< 'c when issym c >] -> let sym = recuperer_symboles (String.make 1 c) flux in let flux_mc = decouper_symboles sym table_mots_clefs in flux_mc;;

let rec analyseur_lexical table_mots_clefs flux = try (match flux with parser
	|[< l = lire_lexeme table_mots_clefs ; r = analyseur_lexical table_mots_clefs >] 
		-> [< l ; r >]
	|[< ''@' >] -> [< >])
	with
	|_ -> raise ParseError;; 	

let construire_analyseur liste_mots_clefs = 
	let table_mots_clefs = Hashtbl.create (List.length liste_mots_clefs) in
	do_list (function mc -> Hashtbl.add table_mots_clefs mc [< 'MotClef mc >]) liste_mots_clefs;
	analyseur_lexical table_mots_clefs;;
