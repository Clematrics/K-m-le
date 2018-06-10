(* Les types suivants définissent la grammaire utilisée *)

type constante = 
	|Entiere of int
	|Flottante of float
	|Const_char of char
	|Const_string of string
	|Booleenne of bool
	|Rien;; (* Représente les parenthèses vides () *)

type motif = 
	|MotifVariable of string
	|MotifBooleen of bool
	|MotifNombre of int
	|MotifPaire of motif * motif
	|MotifConcat of string * string  (* Permet de filtrer les motifs |t::q *)
	|Motif_nil;;

type expression = 
	|Constante of constante
	|Variable of string 
	|Op_unaire of string * expression
	|Op_binaire of string * expression * expression
	|Application of expression * expression
	|Let_in of definition * expression
	|Acces_tableau of expression * expression
	|While of expression * bloc
	|Paire of expression * expression
	|Liste of expression list
	|Tableau of expression array

and filtrage = {pattern : motif; garde : expression; corps : bloc}

and body = 
	|SansFiltre of bloc
	|AvecFiltre of filtrage list 

and definition = {recursivite : bool; nom : string; corps : body}

and phrase = 
	|Expression of expression
	|Definition of definition

and bloc = Bloc of phrase list;;

(* Les types ci-dessus sont mutuellement récursifs. Cela va se retrouver dans la structure de l'analyseur syntaxique. 
On aura en effet une longue suite de fonctions mutuellement récursives. *)


(* On commence maintenant l'analyse syntaxique à proprement parler. *)


let lire_liste lire_element flux =
	let rec lire_reste flux = match flux with parser 
		|[< 'MotClef";" ; element = lire_liste ; reste = lire_reste >] -> element::reste
		|[< >] -> [] 
	in match flux with parser 
		|[< element = lire_liste ; reste = lire_reste >] -> element::reste
		|[< 'MotClef >] -> [];;

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

(* La fonction est_recursive nous servira par la suite, pour lire des definitions, mais elle ne fait pas partie de la cascade de définitions 
récursives *)

let est_recursive flux = match flux with parser 
	|[< 'MotClef "rec" >] -> true
	|[< >] -> false;;

(* Les fonctions lirel_lvl permettent d'implémenter la priorité des opérateurs *)

(* Note : cette construction ne permet pas de construire de nouveaux
opérateurs, il faudrait pour cela considérer l'opération comme
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
	|[< 'Identificateur id >] -> Variable id 
	|[< 'MotClef "("; 'MotClef ")" >] -> Constante(Rien)

and suite_applications fonction flux = match flux with parser
	|[< argument = lire_lvl0 ; expr = suite_applications (Application(fonction,argument)) >] -> expr
	|[< >] -> fonction

(* On traite tout de manière décuryfiée ici : je n'ai pas implémenté de fun qui permette la curyfication. *)

and lire_lvl1 flux = match flux with parser 
	|[< expr0 = lire_lvl0 ; expr = suite_applications expr0 >] -> expr

and lire_lvl2 flux = match flux with parser 
	|[< 'Identificateur t ; 'MotClef "."; 'MotClef "["; expr = lire_expression_arithmetique ; 'MotClef "]" >] -> Acces_tableau(Variable t,expr)
	|[< 'Identificateur t ; 'MotClef "::"; 'Identificateur q >] -> Operation_binaire("::",Variable t, Variable q)

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
	lire_operation lire_lvl6 ["=";"!=";"<";">";"<=";">="]

and lire_lvl8 flux = 
	lire_operation lire_lvl7 ["and"]

and lire_lvl9 flux = 
	lire_operation lire_lvl8 ["or"]

(* Dans un langage impératif, on s'arrêterait ici et on passerait à la lecture d'instructions *)



and lire_expression flux = match flux with parser (* Correspond au niveau 10, permet de gérer les structures while, for, if *)

	|[< 'MotClef "while"; condition = lire_expression_arithmetique; 'MotClef "do"; instruction = lire_bloc ; 'MotClef "done" >] 
	-> While(condition,instruction)

(* Plutôt que de définir une nouvelle instruction for en machine, nous avons choisi de transformer le for en while *)

	|[< 'MotClef "for"; 'Identificateur id; 'MotClef "="; 'Entier debut; 'MotClef "to"; 'Entier fin; 'MotClef "do"; 
	instruction = lire_bloc; 'MotClef "done" >] -> 
	Bloc[Let(Definition{recursivite : false; nom = "while"; corps = SansFiltre(Constante(Entiere debut)))}; 
	While(Expression(Op_binaire("<=","while",fin))),
		Bloc[instruction; Definition{recursivite = false; nom = "while"; corps = SansFiltre(Expression(Op_binaire("+",id,1)))}]]

(* On a nommé la variable qui servait de compteur "while". En effet, comme while est un mot clef du langange, l'utilisateur ne pourra 
pas l'utiliser comme nom de variable et on évite donc qu'il y ait un conflit avec une variable définie par l'utilisateur. *)

(* On a la même chose pour un for avec downto *)

	|[< 'MotClef "for"; 'Identificateur id; 'MotClef "="; 'Entier debut; 'MotClef "downto"; 'Entier fin; 'MotClef "do"; 
	instruction = lire_bloc; 'MotClef "done" >] -> 
	Bloc[Let(Definition{recursivite : false; nom = "while"; corps = SansFiltre(Constante(Entiere debut)))}; 
	While(Expression(Op_binaire("<=",fin,"while"),
		Bloc[instruction; Definition{recursivite = false; nom = __id; corps = SansFiltre(Expression(Op_binaire("-",id,1)))}]]

	|[< 'MotClef "if"; condition = lire_expression ; 'MotClef "then"; instruction1 = lire_bloc; 
	'MotClef "else"; instruction2 = lire_bloc >] ->  (* Ici, on a un exemple de gestion de la règle du longest match. 
	J'utilise le fait que OCaml vérifie les cas du filtrage dans l'ordre pour m'assurer que, si l'utilisateur entre un else, il soit
	effectivement détecté. *)
	Definition {recursivite = false; nom = "if"; corps = AvecFiltre(
	[{pattern = MotifBooleen true; garde = Constante(Booleenne true); corps = instruction1}
	{pattern = MotifBooleen false; garde = Constante(Booleenne false); corps = instruction2}])}

	|[< 'MotClef "if"; condition = lire_expression ; 'MotClef "then"; instruction = lire_bloc >] -> 
	Definition {recursivite = false; nom = "if"; corps = AvecFiltre(
	[{pattern = MotifBooleen true; garde = Constante(Booleenne true); corps = instruction1}
	{pattern = MotifBooleen false; garde = Constante(Booleenne false); corps = Constante(Rien)}])}

and lire_definition flux = match flux with parser
	|[< 'MotClef "let" ; recu = est_recursive ; 'Identificateur nom_fonction ; 'MotClef "=" ;  >]
	 ->  {recursivite = recu ; nom = nom_fonction; corps = expr}
	 
and reste_def def flux = match flux with parser
	|[< 'MotClef "in" ; expr = lire_expression >] -> Expression(Let(def,expr))
	|[< >] -> Definition def

and lire_phrase flux = match flux with parser 
	|[< def = lire_definition ; phrase = reste_def def; 'MotClef ';;' >] -> phrase
	|[< expr = lire_expression ; 'MotClef ';;' >] -> Expression expr

and lire_instruction 