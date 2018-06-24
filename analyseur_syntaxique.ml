(* Les types suivants définissent la grammaire utilisée *)

type constante = 
	|Entiere of int
	|Flottante of float
	|Const_char of char
	|Const_string of string
	|Booleenne of bool;;

type motif = 
	|MotifVariable of string
	|MotifBooleen of bool
	|MotifEntier of int
	|MotifFlottant of float
	|MotifPaire of motif * motif
	|MotifConcat of motif * motif  (* Permet de filtrer les motifs |t::q *)
	|MotifListeVide
	|Motif_nil;; (* Correspond au _ *)

type expression = 
	|Constante of constante
	|Variable of string 
	|Op_unaire of string * expression
	|Op_binaire of string * expression * expression
	|Application of expression * expression
	|Let_in of definition * bloc
	|Acces_tableau of expression * expression
	|While of expression * bloc
	|Paire of expression * expression
	|Liste of expression list
	|Tableau of expression array

and filtrage = {pattern : motif; garde : expression; corps : bloc}

and body = 
	|SansFiltre of bloc
	|AvecFiltre of filtrage list 

and definition = {recursivite : bool; nom : string; parametres : string list; corps : body}

and phrase = 
	|Expression of expression
	|Definition of definition

and bloc = Bloc of expression list;;

(* Les types ci-dessus sont mutuellement récursifs. Cela va se retrouver dans la structure de l'analyseur syntaxique. 
On aura en effet une longue suite de fonctions mutuellement récursives. *)


(* On commence maintenant l'analyse syntaxique à proprement parler. *)

(* Pour les trois fonctions suivantes, on reprend la même technique que pour l'analyse lexicale des char, string, commentaires... qui sont 
délimités par un mot-clef ouvrant et un mot-clef fermant. *)

let lire_liste lire_element flux = 
	let rec lire_reste flux = match flux with parser 
		|[< 'MotClef";" ; element = lire_element ; reste = lire_reste >] -> element::reste
		|[< >] -> [] 
	in match flux with parser 
		|[< element = lire_element ; reste = lire_reste >] -> element::reste
		|[< 'MotClef "]" >] -> [];;

let lire_tableau lire_element flux = 
	(* Cette fonction renvoie une liste parce qu'on ne peut pas connaître a priori le nombre d'éléments que contiendra le tableau. *)
	let rec lire_reste flux = match flux with parser 
		|[< 'MotClef";" ; element = lire_element ; reste = lire_reste >] -> element::reste
		|[< >] -> [] 
	in match flux with parser 
		|[< element = lire_element ; reste = lire_reste >] -> element::reste
		|[< 'MotClef "|"; 'MotClef "]" >] -> [];;

let tab_of_list l =  (* Cette fonction permet de convertir une liste en un array, pour convertir la liste renvoyée par lire_tableau. *)
	let n = List.length l in
	let t = Array.make n (List.hd l) in 
	let rec aux i = function
		|[] -> ()
		|h::q -> t.(i) <- h; aux (i+1) q
	in aux 0 l;
	t;;

let lire_par expr lire_element flux = match flux with parser
						       |[< 'MotClef ","; element2 = lire_element >] -> Paire(expr,element2)
						       |[< 'MotClef ")" >] -> expr ;;

let est_un_operateur operateurs = function 
	|MotClef op -> List.mem op operateurs (* Parce qu'il y a aussi un mem pour les tables de hachage *)
	|_ -> false;;

let lire_operation lire_base operateurs flux =
	let rec lire_reste expr1 flux = match flux with parser
		|[< '(MotClef op) when est_un_operateur operateurs (MotClef op) ; expr2 = lire_base ;
		expr = (lire_reste (Op_binaire(op,expr1,expr2))) >] -> expr
		|[< >] -> expr1
	in match flux with parser 
		|[< expr1 = lire_base ; expr = lire_reste expr1 >] -> expr;;


let lire_acces_tab id lire_interieur flux = match flux with parser
							   |[< 'MotClef "."; 'MotClef "["; expr = lire_interieur; 'MotClef "]" >] -> Acces_tableau(Variable id, expr)
							   |[< >] -> Variable id;; 
  
(* La fonction est_recursive nous servira par la suite, pour lire des definitions, mais elle ne fait pas partie de la cascade de définitions 
récursives *)

let est_recursive flux = match flux with parser 
	|[< 'MotClef "rec" >] -> true
	|[< >] -> false;;

let debut_tableau flux = match flux with parser
					|[< 'MotClef "|" >] -> true
					|[< >] -> false;;

let rec lire_pattern lire_cond flux = match flux with parser
				      |[< 'Entier n >] -> lire_garde (Motif_entier n) flux
				      |[< 'Flottant f >] -> lire_garde (Motif_flottant f) flux
				      |[< 'MotClef "true" >] -> lire_garde (Motif_booleen true) flux
				      |[< 'MotClef "false" >] -> lire_garde (Motif_booleen false) flux
				      |[< 'MotClef "_" >] -> lire_garde (Motif_nil) flux
				      |[< 'MotClef "("; motif = lire_pattern >] -> let m = lire_motif_par motif flux in lire_garde m flux
					  |[< 'MotClef "["; 'MotClef "]" >] -> (MotifListeVide, Constante(Booleenne true))
					  |[< 'Identificateur id >] -> match flux with parser
										  |[< 'MotClef "::"; q = lire_pattern >] -> lire_garde MotifConcat(id,q) flux
										  |[< >] -> lire_garde (Motif_variable id) flux

and lire_motif_par motif1 flux = match flux with
	|[< 'MotClef ","; motif2 = lire_pattern; 'MotClef ")" >] -> MotifPaire(motif1,motif2)
	|[< 'MotClef ")" >] -> motif1

and lire_garde motif lire_cond flux = match flux with parser
	|[< 'MotClef "when"; condition = lire_cond >] -> (motif,cond) 
	|[< >] -> (motif,Constante(Booleenne true);;

let rec lire_motif lire_cond lire_instr accu flux = match flux with parser
						  |[< 'MotClef "|"; (motif,garde_motif) = lire_pattern; 'MotClef "->"; instruction = lire_instr >] ->
						  let f = {pattern = motif; garde = garde_motif; corps = instr} in
						  lire_motif lire_cond lire_instr (f::accu) flux
						  |[< >] -> accu;;

let lire_while lire_cond lire_instr flux = match flux with parser
  
  |[< condition = lire_cond; 'MotClef "do"; instruction = lire_instr; 'MotClef "done" >] 
	-> While(condition,instruction)

(* Plutôt que de définir une nouvelle instruction for en machine, nous avons choisi de transformer le for en while *)

let lire_for lire_instr flux = match flux with parser
						 
	|[< 'MotClef "for"; 'Identificateur id; 'MotClef "="; 'Entier debut >] -> match flux with parser
												 |[< 'MotClef "to"; 'Entier fin; 'MotClef "do";	instruction = lire_instr; 'MotClef "done" >] -> 
												   Let_in({recursivite = false; nom = "while"; parametres = []; corps = SansFiltre(Bloc[Constante(Entiere debut)])}, 
												   	Bloc[Expression(While(Op_binaire("<=",Variable "while", Constante(Entiere(fin))),
		Bloc[instruction; Let_in({recursivite = false; nom = "while"; parametres = []; corps = SansFiltre(Bloc[Op_binaire("+",Variable "id", Constante(Entiere 1))])}, 
			Application("while",Constante(Rien))]))])

(* On a nommé la variable qui servait de compteur "while". En effet, comme while est un mot clef du langange, l'utilisateur ne pourra 
pas l'utiliser comme nom de variable et on évite donc qu'il y ait un conflit avec une variable définie par l'utilisateur. *)

(* On a la même chose pour un for avec downto *)

												 
												 |[< 'MotClef "downto"; 'Entier fin; 'MotClef "do";	instruction = lire_instr; 'MotClef "done" >] -> 
												   Let_in({recursivite = false; nom = "while"; parametres = []; corps = SansFiltre(Bloc[Constante(Entiere debut)])}, 
												   	Bloc[While(Op_binaire(">=",Variable "while", Constante(Entiere(fin))),
		Bloc[instruction; Definition{recursivite = false; nom = "while"; parametres = []; corps = SansFiltre(Bloc[Expression(Op_binaire("-",Variable "id", Constante(Entiere 1)))])}])])

let lire_if lire_cond lire_instr flux = match flux with
			
  |[< condition = lire_cond ; 'MotClef "then"; instruction1 = lire_instr >] -> match flux with parser
												 |[< 'MotClef "else"; instruction2 = lire_instr >] -> 
	Let_in({recursivite = false; nom = "if"; parametres = []; corps = AvecFiltre(
	[{pattern = MotifBooleen true; garde = Constante(Booleenne true); corps = instruction1}
	   {pattern = MotifBooleen false; garde = Constante(Booleenne false); corps = instruction2}])},
	       Application(Variable "if", Constante(Rien)))

											     |[< condition = lire_cond ; 'MotClef "then"; instruction = lire_instr >] -> 
	Definition {recursivite = false; nom = "if"; parametres = []; corps = AvecFiltre(
								 [{pattern = MotifBooleen true; garde = Constante(Booleenne true); corps = instruction}
	{pattern = MotifBooleen false; garde = Constante(Booleenne false); corps = Constante(Rien)}])}


(* Les fonctions lirel_lvl permettent d'implémenter la priorité des opérateurs *)

(* Note : cette construction ne permet pas de construire de nouveaux opérateurs, il faudrait pour cela considérer l'opération comme
l'application d'une variable à une variable ou à une paire.
Mais OCaml ne permet pas non plus de définir ses propres infixes. 
Il y a toujours l'option de définir un opérateur comme une fonction ici. *)

(* On arrive maintenant à "la cascade de définitions récursives" qui permet de construire tout l'analyseur syntaxique. 
Cette suite d'analyseurs converge d'abord vers une fonction lire_expression, qui nécessite de savoir lire des blocs pour implémenter 
le While. 
On a ensuite la lecture d'une définition, qui ne pose aucun problème particulier et est donc incluse dans la lecture d'une phrase. 
Enfin vient la fonction lire_bloc qui lit plusieurs phrases. *)
  
let rec lire_lvl0 flux = match flux with parser 
	|[< 'Entier n >] -> Constante(Entiere n)
	|[< 'Flottant x >] -> Constante(Flottante x)
	|[< 'MotClef "false" >] -> Constante(Booleenne false)
	|[< 'MotClef "true" >] -> Constante(Booleenne true)
	|[< 'Identificateur id >] -> lire_acces_tab id lire_expression flux
	|[< 'MotClef "("; expr = lire_expression >] -> lire_par expr lire_expression flux
    |[< 'MotClef "[" >] -> let b = debut_tableau flux in if b then let l = lire_tableau lire_expression flux in Tableau (tab_of_list l)
							else let l = lire_liste lire_expression flux in Liste l
	|[< 'MotClef "while" >] -> lire_while lire_expression lire_bloc flux
	|[< 'MotClef "for" >] -> lire_for lire_bloc flux
	|[< 'MotClef "if" >] -> lire_if lire_expression lire_bloc flux
													
and suite_applications fonction flux = match flux with parser
	|[< argument = lire_lvl0 ; expr = suite_applications (Application(fonction,argument)) >] -> expr
	|[< >] -> fonction

(* On traite tout de manière décuryfiée ici : je n'ai pas implémenté de fun qui permette la curyfication. *)

and lire_lvl1 flux = match flux with parser 
				    |[< expr0 = lire_lvl0 ; expr = suite_applications expr0 >] -> expr
												    
and lire_lvl2 flux =
  lire_operation lire_lvl1 ["::"] flux

and lire_lvl3 flux = match flux with parser 
	|[< 'MotClef "-"; expr = lire_lvl2 >] -> Op_unaire("-",expr)
	|[< 'MotClef "not" ; expr = lire_lvl2 >] -> Op_unaire("not",expr)
	|[< expr = lire_lvl2 >] -> expr

and lire_lvl4 flux =  
	lire_operation lire_lvl3 ["*";"/"] flux

and lire_lvl5 flux = 
	lire_operation lire_lvl4 ["+";"-"] flux

(* Il n'y a pas d'opérateurs flottants comme en OCaml : ici, quand par exemple on additionne un flottant et un entier, 
il y a conversion implicite de l'entier en flottant et le résultat renvoyé est un flottant *)

and lire_lvl6 flux = 
	lire_operation lire_lvl5 ["mod"] flux

and lire_lvl7 flux = 
	lire_operation lire_lvl6 ["=";"!=";"<";">";"<=";">="] flux

and lire_lvl8 flux = 
	lire_operation lire_lvl7 ["and"] flux

and lire_expression flux = 
	lire_operation lire_lvl8 ["or"] flux
					     
and lire_definition flux = match flux with parser
					  |[< 'MotClef "let" ; recu = est_recursive ; 'Identificateur nom_fonction ; 'MotClef "=" >] ->
					    match flux with parser
							  |[< 'MotClef "function"; liste_filtrage = lire_motif lire_expression lire_bloc [] >] -> 
							  {recursivite = recu ; nom = nom_fonction; parametres = []; corps = AvecFiltre(liste_filtrage)}
							  |[< instr = lire_bloc >] -> {recursivite = recu ; nom = nom_fonction; parametres = []; corps = SansFiltre(instr)}
	 
and reste_def def flux = match flux with parser
	|[< 'MotClef "in" ; bloc_expr = lire_bloc >] -> Expression(Let_in(def,bloc_expr))
	|[< >] -> Definition def

and lire_bloc flux = match flux with parser 
	let rec lire_reste accu flux = match flux with parser
		|[< 'MotClef ";"; expr = lire_expression; bloc_expr = (lire_reste (accu@expr)) >] -> bloc_expr
		|[< >] -> accu
	in match flux with 
		|[< expr1 = lire_expression; liste = lire_reste [expr1] >] -> liste;;

and lire_phrase flux = match flux with parser 
	|[< def = lire_definition ; phrase = reste_def def; 'MotClef ';;' >] -> phrase
	|[< expr = lire_expression ; 'MotClef ';;' >] -> Expression expr;;

let rec lire_suite_phrases accu flux = 
	|[< p = lire_phrase >] -> lire_suite_phrase (accu@p) flux
	|[< >] -> accu;; 