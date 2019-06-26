
#load "dynlink.cma";;
#load "camlp4o.cma";;

#use "topfind";;
#camlp4o;;
(* ne fonctionne pas dans le top-level avec la commande "ocaml test.ml" :(

   camlp4o.opt /mnt/c/Users/Admin/Google\ Drive/_PREPA/TIPE/Ocaml/test.ml -o /mnt/c/Users/Admin/Google\ Drive/_PREPA/TIPE/Ocaml/final.ml
   ocaml /mnt/c/Users/Admin/Google\ Drive/_PREPA/TIPE/Ocaml/final.ml
*)

open Array;;
open Int64;;
open Map;;
open Stream;;
open String;;

module Dict = Map.Make(String);;

let to_str c = String.make 1 c;;
let str_tl str = let len = String.length str - 1 in String.sub str 1 len;;

let rec is_in elem = function
    	[] -> false
  | h::t -> (h=elem) || (is_in elem t)
;;

let hd = function
    	[] -> raise (Invalid_argument "Empty list")
  | h::t -> h
;;

let tl = function
    	[] -> raise (Invalid_argument "Empty list")
  | h::t -> t
;;

let rec any_except c stream =
  if Stream.next stream <> c then any_except c stream else ()
;;

let rec spaces = parser
                   [< '' ' | '\t' | '\n' | '\r' | '\012' ; _ = spaces >] -> ()
               |   [< >] -> ()
;;


let is_symbol = function
    	':' | ',' | '(' | ')' | '+' | '-' | '*' | '/' | '%' | '^' | '&' | '|' | '~' | '@' | '>' | '<' | '[' | ';' | ']' -> true
  |	_ -> false
;;

let is_str_symbol str = is_symbol str.[0];;

let kick_comments stream =
  let rec kick stream = match stream with parser
                                            		[< ''/' ; ''/' ; _ = any_except '\n' ; next = kick >] -> next
                                        |   [< 'c ; next = kick >] -> [< 'c ; next >]
                                        |	[< >] -> [< >]
  in spaces stream;
    kick stream
;;

let str_catcher stream =
  let rec aux str stream = match stream with parser
                                               		[< ''\\' ; 'c >] -> aux ( str ^ "\\" ^ to_str c ) stream
                                           |	[< 'c when c <> '"' >] -> aux ( str ^ to_str c ) stream
                                                                  |	[< >] -> str
in aux "" stream
;;

let split stream =
  let rec aux str = parser
                      	[< '' ' | '\t' | '\n' | '\r' | '\012' ; next = aux "" >] ->
    	    if str <> "" then
      			[< 'String.lowercase_ascii str ; next >]
    	    else
      			next
                  |	[< ''"' ; str_content = str_catcher ; ''"' ; next = aux "" >] -> [< '"\"" ; 'str_content ; '"\"" ; next >]
                  |	[< 'c when is_symbol c ; next = aux "" >] -> [< 'String.lowercase_ascii str ; 'to_str c ; next >]
                                                            |	[< 'c ; next = aux (str ^ to_str c) >] -> next
                                                            |	[< >] -> [< >]
in aux "" stream
;;

let get_directive stream =
  let rec get_args last = parser
                            		[< '")" >] -> []
                        |	[< '"," ; args = get_args "" >] ->
                        	    if last = "" then
                          			failwith "UN ARGUMENT EST DECLARE SANS NOM" (* TODO : indiquer où / créer exception spécifique au problème *)
                        	    else if is_in last args then
                          			failwith "UN ARGUMENT EST DECLARE DEUX FOIS AVEC LE MEME NOM" (* TODO : idem *)
                        	    else
                          			last::args
                        |	[< 'arg ; args = get_args arg >] ->
                        	    if last <> "" then
                          			failwith "DEUX ARGUMENTS SONT DECLARES COTE A COTE SANS SEPARATEUR" (* TODO : exception spécifique + emplacement *)
                        	    else
                          			args
                        |	[< >] -> failwith "FIN DE FICHIER ATTEINT, DIRECTIVE NON RESOLUE" (* TODO : idem *)
  in let check_args = parser
                        		[< '"(" ; args = get_args "" >] -> args
                    |	[< >] -> []
  in let rec get_expr = parser
                          		[< '"#end" >] -> [< >]
                      |	[< 'elem ; expr = get_expr >] -> [< 'elem ; expr >]
                      |	[< >] -> failwith "FIN DE FICHIER ATTEINT, DIRECTIVE NON RESOLUE" (* TODO : idem *)
  in let name = match stream with parser
                                    		[< '"#end" | "(" | ")" >] -> failwith "DIRECTIVE INCOMPLETE OU NOM INVALIDE" (* TODO classique des exc *)
                                |	[< 'str >] -> str
                                |	[< >] -> failwith "FIN DE FICHIER ATTEINT, DIRECTIVE NON RESOLUE" (* TODO : idem *)
  in let args = check_args stream in
    (name, args, get_expr stream)
;;

let label_counter = ref 0;;
let next_label () =
  incr label_counter;
  !label_counter
;;

let apply_directive dir stream =
  let name, args, expr = dir in
  let dict = ref Dict.empty in
  let rec presolve_labels = parser
                              		[< 'str when str.[0] = '#' && String.length str > 0 && str_tl str <> "use" ; next = presolve_labels >] -> begin
  	    	let label = str_tl str in
  			let value = try Dict.find label !dict with
      	    		Not_found -> begin
        					dict := Dict.add label (next_label ()) !dict;
        					!label_counter
      				end
  			in [< '"__reserved" ^ (string_of_int value) ; next >]
		end
                                                                                                                                   |	[< 'str ; next = presolve_labels >] -> [< 'str ; next >]
                                                                                                                                   |	[< >] -> [< >]
in let rec solve_args dict_ = parser
                                [< 'str ; next = solve_args dict_ >] -> begin
       			match Dict.find_opt str dict_ with
           				Some res -> [< 'res ; next >]
         			|	None -> [< 'str ; next >]
     		end
                            |	[< >] -> [< >]
in let rec apply stream_ = match stream_ with parser
                                                		[< 'str when str = name >] -> begin
  	    	if args = [] then
    				match stream_ with parser
                         		    		[< '"(" >] -> begin
        						match stream_ with parser
                             			    			[< '")" >] -> begin
            								dict := Dict.empty;
            								[< presolve_labels expr ; apply stream_ >]
          							end
                         						|   [< >] -> [< '"(" ; apply stream_ >]
      					end
                     	        	|   [< >] -> begin
                         						dict := Dict.empty;
                         						[< presolve_labels expr ; apply stream_ >]
                       		    		end
  	    	else
    				match stream_ with parser
                         		    		[< '"(" >] -> begin
        						let args_dict = ref Dict.empty in
        						let rec presolve_args args_ prestream = parser
                                                  			    			[< '")" when args_ = [] >] -> [< solve_args !args_dict expr ; apply stream_ >]
                                                                           						|   [< '")" when args_ <> [] >] -> [< prestream ; '")" ; apply stream_ >]
                                                                                                         						|   [< '"," when args_ = [] >] -> [< prestream ; '")" ; apply stream_ >]
                                                                                                                                      						|   [< '"," ; next = presolve_args (tl args_) [< prestream ; '"," >] >] -> next
                                                                                                                                      						|   [< 'str ; next = presolve_args (tl args_) [< prestream ; 'str >] >] -> begin
                                                                                                                                          								args_dict := Dict.add (hd args_) str !args_dict;
                                                                                                                                          								next
                                                                                                                                        			    			end
                                                                                                                                      						|   [< >] -> failwith "ERREUR FIN DE FICHIER ATTEINT, APPEL D'UNE DIRECTIVE NON COMPLETE" (* TODO : idem *)
						in presolve_args args [< >] stream_
		    		end
				|   [< >] -> apply stream_
		end
|	[< 'str ; next = apply >] -> [< 'str ; next >]
in apply stream
;;

(* TODO : gérer la valeur absolue, les shift et rajouter les symboles à reconnaitre *)
(*
GRAMMAR :

rule ::=
expr rule_tail

rule_tail ::=
& rule
|   | rule
|   @ rule
|	<< rule
|	>> rule
|   ε

expr ::=
term expr_tail

expr_tail ::=
+ expr
|   - expr
|   ε

term ::=
factor term_tail

term_tail ::=
* term
|   / term
|   ε

factor ::=
atom factor_tail

factor_tail ::=
^ factor
|   ε

atom ::=
( rule )
|   ~ atom
|	+ atom
|	- atom
|   int
|   float
*)
let f2i = Int64.bits_of_float;;
let i2f = Int64.float_of_bits;;

let (<^) a b = Int64.compare a b < 0;;
let (=^) a b = Int64.compare a b = 0;;

let add_i_f i f = Int64.to_float i +. f;;
let add_f_i f i = add_i_f i f;;
let sub_i_f i f = Int64.to_float i -. f;;
let sub_f_i f i = -. (sub_i_f i f);;
let mul_i_f i f = Int64.to_float i *. f;;
let mul_f_i f i = mul_i_f i f;;
let div_i_f i f = Int64.to_float i /. f;;
let div_f_i f i = f /. (Int64.to_float i);;
let pow_i_f i f = Int64.to_float i ** f;;
let pow_f_i f i = f ** (Int64.to_float i);;
let rec int64_pow x = function
    	k when k <^ Int64.zero -> 0L (* div_f_i 1. (Int64_pow x (Int64.neg k)) *)
  |	0L -> 1L
  |	1L -> x
  |	k when Int64.rem k 2L =^ 0L -> int64_pow (Int64.mul x x) (Int64.shift_right_logical k 1)
  |	k -> Int64.mul x (int64_pow x (Int64.pred k))
;;

type expr_type = Int_ | Float_;;
type ast_Op = And | Or | Xor | Not | Shl | Shr | Add | Sub | Mul | Div | Pow | Abs | Neg ;;
type ast =
      	BiOp of expr_type * ast * ast_Op * ast
    |	UnOp of expr_type * ast_Op * ast
    |	Int of int64
    |	Float of float
    |	Epsilon
;;

let get_ast_type = function
    BiOp (t, _, _, _) -> t
  |   UnOp (t, _, _) -> t
  |   Int _ -> Int_
  |   Float _ -> Float_
;;

let rec evaluate = function
    Int i -> i
  |   Float f -> f2i f
  |   UnOp (t, op, tree) -> begin match (op, t) with
        		Not, _    	-> Int64.lognot (evaluate tree)
      	|	Abs, Int_ 	-> Int64.abs (evaluate tree)
      	|	Abs, Float_ -> f2i (abs_float (i2f (evaluate tree)))
      	|	Neg, Int_ 	-> Int64.neg (evaluate tree)
      	|	Neg, Float_ -> f2i (-. (i2f (evaluate tree)))
      	|	_, _ -> failwith "opérateur non défini pour le type suivant" (* TODO : exc *)
    	end
  |   BiOp (_, l, op, r) -> begin match op, get_ast_type l, get_ast_type r with
        		And, Int_,		Int_	->	Int64.logand (evaluate l) (evaluate r)
      |	Or , Int_,		Int_	->	Int64.logor  (evaluate l) (evaluate r)
      |	Xor, Int_,		Int_	->	Int64.logxor (evaluate l) (evaluate r)
      	|	Shl, Int_,		Int_	->	Int64.shift_left (evaluate l) (Int64.to_int (evaluate r))
      	|	Shr, Int_, 		Int_	->	Int64.shift_right (evaluate l) (Int64.to_int (evaluate r))
      |	Add, Int_,		Int_	->	Int64.add (evaluate l) (evaluate r)
      |	Add, Int_,		Float_	->	f2i (add_i_f (evaluate l) (i2f (evaluate r)))
      |	Add, Float_,	Int_	->	f2i (add_f_i (i2f (evaluate l)) (evaluate r))
      |	Add, Float_,	Float_	->	f2i ( (i2f (evaluate l)) +. (i2f (evaluate r)) )
      |	Sub, Int_,		Int_	->	Int64.sub (evaluate l) (evaluate r)
      |	Sub, Int_,		Float_	->	f2i (sub_i_f (evaluate l) (i2f (evaluate r)))
      |	Sub, Float_,	Int_	->	f2i (sub_f_i (i2f (evaluate l)) (evaluate r))
      |	Sub, Float_,	Float_	->	f2i ( (i2f (evaluate l)) -. (i2f (evaluate r)) )
      |	Mul, Int_,		Int_	->	Int64.mul (evaluate l) (evaluate r)
      |	Mul, Int_,		Float_	->	f2i (mul_i_f (evaluate l) (i2f (evaluate r)))
      |	Mul, Float_,	Int_	->	f2i (mul_f_i (i2f (evaluate l)) (evaluate r))
      |	Mul, Float_,	Float_	->	f2i ( (i2f (evaluate l)) *. (i2f (evaluate r)) )
      |	Div, Int_,		Int_	->	Int64.div (evaluate l) (evaluate r)
      |	Div, Int_,		Float_	->	f2i (div_i_f (evaluate l) (i2f (evaluate r)))
      |	Div, Float_,	Int_	->	f2i (div_f_i (i2f (evaluate l)) (evaluate r))
      |	Div, Float_,	Float_	->	f2i ( (i2f (evaluate l)) /. (i2f (evaluate r)) )
      |	Pow, Int_,		Int_	->	int64_pow (evaluate l) (evaluate r)
      |	Pow, Int_,		Float_	->	f2i (pow_i_f (evaluate l) (i2f (evaluate r)))
      |	Pow, Float_,	Int_	->	f2i (pow_f_i (i2f (evaluate l)) (evaluate r))
      |	Pow, Float_,	Float_	->	f2i ( (i2f (evaluate l)) ** (i2f (evaluate r)) )
      	|	_, _, _ -> failwith "opérateur non disponible avec les types suivants" (* TODO : exc  *)
    end
;;

let is_number_type = ref Int_;;
let is_number_int = ref Int64.zero;;
let is_number_float = ref 0.;;
let is_number str =
  try is_number_int := Int64.of_string str; is_number_type := Int_; true with
      	Failure _ -> try is_number_float := float_of_string str; is_number_type := Float_; true with
        	    Failure _ -> false
;;

let to_hex (int_ : int64) = Printf.sprintf "%Lx" int_;;

let assemble_trees l_tree op r_tree =
  if r_tree = Epsilon then
    l_tree
  else
    	let t, u = get_ast_type l_tree, get_ast_type r_tree in
    	let v = if t = Float_ || u = Float_ then Float_ else Int_ in
      	BiOp (v, l_tree, op, r_tree)
;;

let try_parse_expr stream =
  let rec rule stream = begin
    		let l_tree = expr stream
    		and Some op, r_tree = rule_tail stream in
      			assemble_trees l_tree op r_tree
  end
  and rule_tail = parser
                    		[< '"&" ; tree = rule >] -> Some And, tree
                |	[< '"|" ; tree = rule >] -> Some Or , tree
                |	[< '"@" ; tree = rule >] -> Some Xor, tree
                	|	[< '"<" ; '"<" ; tree = rule >] -> Some Shl, tree
                	|	[< '">" ; '">" ; tree = rule >] -> Some Shr, tree
                |	[< >] -> None, Epsilon
  and expr stream = begin
    		let l_tree = term stream
    		and Some op, r_tree = expr_tail stream in
      			assemble_trees l_tree op r_tree
  end
  and expr_tail = parser
                    		[< '"+" ; tree = expr >] -> Some Add, tree
                |	[< '"-" ; tree = expr >] -> Some Sub, tree
                |	[< >] -> None, Epsilon
  and term stream = begin
    		let l_tree = factor stream
    		and Some op, r_tree = term_tail stream in
      			assemble_trees l_tree op r_tree
  end
  and term_tail = parser
                    		[< '"*" ; tree = term >] -> Some Mul, tree
                |	[< '"/" ; tree = term >] -> Some Div, tree
                |	[< >] -> None, Epsilon
  and factor stream = begin
    		let l_tree = atom stream
    		and Some op, r_tree = factor_tail stream in
      			assemble_trees l_tree op r_tree
  end
  and factor_tail = parser
                      		[< '"^" ; tree = factor >] -> Some Pow, tree
                  |	[< >] -> None, Epsilon
  and atom = parser
               		[< '"(" ; tree = rule ; '")" >] -> tree
           |	[< '"~" ; tree = atom >] -> UnOp( get_ast_type tree, Not, tree )
           	|	[< '"+" ; tree = atom >] -> UnOp( get_ast_type tree, Abs, tree )
           	|	[< '"-" ; tree = atom >] -> UnOp( get_ast_type tree, Neg, tree )
           |	[< 'str when is_number str >] -> begin
  	    	match !is_number_type with
      				Int_ -> Int !is_number_int
    			|	Float_ -> Float !is_number_float
		end
                                         |	[< >] -> failwith "A NUMBER OR A LITERAL WAS EXPECTED HERE" (* TODO : idem *)
in let tree = rule stream in
  [< 'to_hex (evaluate tree) ; stream >]
;;

let rec solve_expr stream =
  let rec aux = parser
                  		[< 'str when is_number str >] -> begin
  			match try_parse_expr [< 'str ; stream >] with parser
                                                  				[< 'nmb ; next = solve_expr >] -> [< 'nmb ; next >]
		end
                                              	|	[< '"(" | ")" | "~" | "+" | "-" as str >] -> begin
                                                  			match try_parse_expr [< 'str ; stream >] with parser
                                                                                                  				[< 'nmb ; next = solve_expr >] -> [< 'nmb ; next >]
                                                		end
                                              |	[< '"<" | ">" | "*" | "/" | "%" | "^" | "&" | "|" | "@" >] -> failwith "EXPRESSION DETECTEE MAIS COMMENCEE PAR UN OPERATEUR INATTENDU" (* TODO : idem *)
                                              |   [< 'str ; next = solve_expr >] -> [< 'str ; next >]
                                              |   [< >] -> [< >]
in aux stream
;;

(* return a stream of words containing a valid assembler code, without any directive left. It also return the list of the 'use' directives declared in this file, including all 'use' directives imorted in this file *)
let rec solve_file file_path files_path =
  	if Filename.is_relative file_path then
    		failwith "les chemins relatifs ne sont pas encore supportés" (* TODO : exc *);
  let chn = open_in file_path in
  let clean_stream = kick_comments (Stream.of_channel chn) in
  let split_stream = split clean_stream in
  let def_directives = ref [] in
  let rec solve stream = match stream with parser
                                             		[< '"#private" ; directive = get_directive >] -> solve (apply_directive directive stream)
                                         |	[< '"#def" ; directive = get_directive >] -> begin
                                             			def_directives := directive::!def_directives;
                                             			solve (apply_directive directive stream)
                                           		end
                                         |	[< '"#use" ; '"\"" ; 'str ; '"\"" >] ->
                                             			if is_in str files_path then
                                               				failwith ("CYCLE CREE LORS DE L'UTILISATION DE #USE : " ^ (hd files_path) ^ " CALL " ^ str ^ ", QUI EST DEJA EN COURS DE RESOLUTION"); (* TODO : idem *)
                                             			let (file_stream, imported_dir) = solve_file str (file_path::files_path) in begin
                                               def_directives := imported_dir @ !def_directives;
                                               	   			let rec solve_imported stream_ = function
                                                   					[] -> stream_
                                                 	   			|	dir::l -> solve_imported (apply_directive dir stream_) l
                                               	   			in solve (solve_imported stream imported_dir)
                                             			end
                                         |	[< 'str ; next = solve >] -> [< 'str ; next >]
                                         |	[< >] -> [< >]
  in solve_expr (solve split_stream), !def_directives
;;

let data = ref Dict.empty;;
(* TODO ; possibilité de mettre des références de données dans des déclarations de données *)
let data_catcher stream =
  	let rec get_data last stream = match stream with parser
                                                     		[< 'str when is_number str >] -> begin
  			if last <> None then
    				failwith "données mal formatées" (* TODO : idem *)
  			else
    				get_data (Some !is_number_int) stream
		end
                                                                                 	|	[< '";" ; data_ = get_data None >] -> begin match last with
                                                                                       			Some int_ -> (int_)::data_
                                                                                     		|	None -> failwith "données mal formatées" (* TODO : idem *)
                                                                                   		end
                                                                                 	|	[< '"]" >] -> []
                                                                                 	|	[< >] -> failwith "erreur, donnée non reconnue" (* TODO : idem *)
	in let name = match stream with parser
                                  		[< 'str when is_str_symbol str >] -> failwith "SYMBOLE RESERVE"
                                                                  	|	[< 'str >] -> str
                                                                  	|	[< >] -> failwith "FIN DE FICHIER ATTEINT" (* TODO *)
	in try ignore (Dict.find name !data) with
    		Not_found -> failwith "données déjà déclaréés" (* TODO *);
      	data := Dict.add name (get_data None stream) !data
;;

let imm_res = ref 0L;;
let is_imm str = match Int64.of_string_opt str with
    		Some x -> begin
      			imm_res := x;
      			true
    		end
  	|	None -> false;
;;

let reg_res = ref 0L;;
let is_reg = function
    	"ext"	-> begin reg_res := 25L; true end
  |	"sr" 	-> begin reg_res := 27L; true end
  |	"ir" 	-> begin reg_res := 26L; true end
  |	"er" 	-> begin reg_res := 28L; true end
  |	"edr" 	-> begin reg_res := 29L; true end
  |	str when str.[0] = 'r' -> begin
      		match int_of_string_opt (str_tl str) with
          			None -> false
        		|	Some x -> begin
            				if x <= 29 then begin
              					reg_res := Int64.of_int x;
              					true
            				end
            				else
              					false
          			end
    	end
  |	_ -> false
;;

type token = Stop | Sys | Call | Ret | Jmp | Jz | Jnz | Lt | Let | Eq | Load | Store | Push | Pop | And | Or | Xor | Shl | Shr | Add | Sub | Mul | Div | Bcnt | Rev | Msb | Cvrt | Ftrc | Flt | Flet | Fadd | Fsub | Fmul | Fdiv | Imm of int64 | Ptr of string | Reg of int64 | Label of string;;

let rec lexer stream = match stream with parser
                                           	[< '"data"	>] -> begin data_catcher stream; lexer stream end
                                       |	[< '"stop" 	; next = lexer >] -> [< 'Stop 	; next >]
                                       |	[< '"sys" 	; next = lexer >] -> [< 'Sys 	; next >]
                                       |	[< '"call" 	; next = lexer >] -> [< 'Call 	; next >]
                                       |	[< '"ret" 	; next = lexer >] -> [< 'Ret 	; next >]
                                       |	[< '"jmp" 	; next = lexer >] -> [< 'Jmp 	; next >]
                                       |	[< '"jz" 	; next = lexer >] -> [< 'Jz 	; next >]
                                       |	[< '"jnz" 	; next = lexer >] -> [< 'Jnz 	; next >]
                                       |	[< '"lt" 	; next = lexer >] -> [< 'Lt 	; next >]
                                       |	[< '"let" 	; next = lexer >] -> [< 'Let 	; next >]
                                       |	[< '"eq" 	; next = lexer >] -> [< 'Eq 	; next >]
                                       |	[< '"load" 	; next = lexer >] -> [< 'Load 	; next >]
                                       |	[< '"store" ; next = lexer >] -> [< 'Store 	; next >]
                                       |	[< '"push" 	; next = lexer >] -> [< 'Push 	; next >]
                                       |	[< '"pop" 	; next = lexer >] -> [< 'Pop 	; next >]
                                       |	[< '"and" 	; next = lexer >] -> [< 'And 	; next >]
                                       |	[< '"or" 	; next = lexer >] -> [< 'Or 	; next >]
                                       |	[< '"xor" 	; next = lexer >] -> [< 'Xor 	; next >]
                                       |	[< '"shl" 	; next = lexer >] -> [< 'Shl 	; next >]
                                       |	[< '"shr"	; next = lexer >] -> [< 'Shr 	; next >]
                                       |	[< '"add" 	; next = lexer >] -> [< 'Add 	; next >]
                                       |	[< '"sub" 	; next = lexer >] -> [< 'Sub 	; next >]
                                       |	[< '"mul" 	; next = lexer >] -> [< 'Mul 	; next >]
                                       |	[< '"div" 	; next = lexer >] -> [< 'Div 	; next >]
                                       |	[< '"bcnt" 	; next = lexer >] -> [< 'Bcnt 	; next >]
                                       |	[< '"rev" 	; next = lexer >] -> [< 'Rev 	; next >]
                                       |	[< '"msb" 	; next = lexer >] -> [< 'Msb 	; next >]
                                       |	[< '"cvrt" 	; next = lexer >] -> [< 'Cvrt 	; next >]
                                       |	[< '"ftrc" 	; next = lexer >] -> [< 'Ftrc 	; next >]
                                       |	[< '"flt" 	; next = lexer >] -> [< 'Flt 	; next >]
                                       |	[< '"flet" 	; next = lexer >] -> [< 'Flet 	; next >]
                                       |	[< '"fadd" 	; next = lexer >] -> [< 'Fadd 	; next >]
                                       |	[< '"fsub" 	; next = lexer >] -> [< 'Fsub 	; next >]
                                       |	[< '"fmul" 	; next = lexer >] -> [< 'Fmul 	; next >]
                                       |	[< '"fdiv" 	; next = lexer >] -> [< 'Fdiv 	; next >]
                                       |	[< 'str when is_imm str ; next = lexer >] -> [< 'Imm (!imm_res) ; next >]
                                                                                 |	[< 'str when is_reg str ; next = lexer >] -> [< 'Reg (!reg_res) ; next >]
                                                                                                                           |	[< 'str >] -> begin match stream with parser
                                                                                                                                                                     		[< '":" >] -> [< 'Label str ; lexer stream >]
                                                                                                                                                                 	|	[<      >] -> [< 'Ptr str   ; lexer stream >]
                                                                                                                             	end
                                                                                                                           |	[< >] -> [< >]
;;


(* TODO : vérifier syntaxe fonctions Map *)
let wait_for_lbl = ref Dict.empty;;
let lbl = ref Dict.empty;;
let register_ptr ptr pos =
  	try
    		let lst, p = Dict.find ptr !lbl
    		in lbl := Dict.add ptr (pos::lst, p) !lbl
  	with
      		Not_found ->
        			let lst_ = try Dict.find ptr !wait_for_lbl with Not_found -> []
        			in wait_for_lbl := Dict.add ptr (pos::lst_) !wait_for_lbl
;;

let register_label lbl_ pos = match Dict.find_opt lbl_ !lbl with
    		None -> begin
      			let lst = try Dict.find lbl_ !wait_for_lbl with Not_found -> []
      			in wait_for_lbl := Dict.remove lbl_ !wait_for_lbl;
        			lbl := Dict.add lbl_ (lst,pos) !lbl
    		end
  	|	Some _ -> failwith "label déjà déclaré" (* TODO : exc *)
;;

let make_instr opcode regs imms =
  	let rec head_instr i = function
      		[] -> (*Int64.logor (Int64.shift_left debug 32)*) (Int64.shift_left (Int64.rem opcode 127L) 25) (* TODO : debug *)
    	| h::t -> Int64.logor (Int64.shift_left h (i*5)) (head_instr (i-1) t)
  	in (head_instr 4 regs)::imms, List.length imms + 1
;;

let rec code_gen pos stream =
  	let rule_0 offset stream = match stream with parser
                                                 		[< 'Reg reg >] -> make_instr (Int64.add offset 0L) [reg] []
                                             	|	[< 'Imm imm >] -> make_instr (Int64.add offset 1L) [] [imm]
                                             	|	[< 'Ptr ptr >] -> begin
                                                 			register_ptr ptr (pos+1);
                                                 			make_instr (Int64.add offset 1L) [] [0L]
                                               		end
  	and rule_1 offset stream = match stream with parser
                                                 		[< 'Reg reg1 >] -> begin match stream with parser
                                                                                              				[< 'Reg reg2 ; 'Reg reg3 >] -> make_instr (Int64.add offset 0L) [reg1;reg2;reg3] []
                                                                                          			|	[< 'Imm imm1 ; 'Reg reg2 >] -> make_instr (Int64.add offset 1L) [reg1;reg2]  [imm1]
                                                                                          			|	[< 'Ptr ptr  ; 'Reg reg2 >] -> begin
                                                                                              				register_ptr ptr (pos+1);
                                                                                              				make_instr (Int64.add offset 1L) [reg1;reg2] [0L]
                                                                                            			end
    		end
                                             	|	[< 'Imm imm ; 'Reg reg1 ; 'Reg reg2 >] -> make_instr (Int64.add offset 2L) [reg1;reg2] [imm]
                                             	|	[< 'Ptr ptr ; 'Reg reg1 ; 'Reg reg2 >] -> begin
                                                 			register_ptr ptr (pos+1);
                                                 			make_instr (Int64.add offset 2L) [reg1;reg2] [0L]
                                               		end
  	and rule_2 offset stream = match stream with parser
                                                 		[< 'Reg reg1 >] -> begin match stream with parser
                                                                                              			[< 'Reg reg2 >] -> begin match stream with parser
                                                                                                                                           				[< 'Reg reg3 >] -> make_instr (Int64.add offset 0L) [reg1;reg2;reg3] []
                                                                                                                                       			|	[< 'Imm imm  >] -> make_instr (Int64.add offset 1L) [reg1;reg2] [imm]
                                                                                                                                       			|	[< 'Ptr ptr  >] -> begin
                                                                                                                                           					register_ptr ptr (pos+1);
                                                                                                                                           					make_instr (Int64.add offset 1L) [reg1;reg2] [0L]
                                                                                                                                         				end
      			end
                                                                                          		|	[< 'Imm imm1 ; 'Reg reg2 >] -> make_instr (Int64.add offset 2L) [reg1;reg2] [imm1]
                                                                                          		|	[< 'Ptr ptr  ; 'Reg reg2 >] -> begin
                                                                                              				register_ptr ptr (pos+1);
                                                                                              				make_instr (Int64.add offset 2L) [reg1;reg2] [0L]
                                                                                            			end
    		end
                                             	|	[< 'Imm imm1 >] -> begin match stream with parser
                                                                                            			[< 'Reg reg1 >] -> begin match stream with parser
                                                                                                                                         				[< 'Reg reg2  >] -> make_instr (Int64.add offset 3L) [reg1;reg2] [imm1]
                                                                                                                                     			|	[< 'Imm imm2  >] -> make_instr (Int64.add offset 4L) [reg1] [imm1;imm2]
                                                                                                                                     			|	[< 'Ptr ptr   >] -> begin
                                                                                                                                         					register_ptr ptr (pos+2);
                                                                                                                                         					make_instr (Int64.add offset 4L) [reg1] [imm1;0L]
                                                                                                                                       				end
                                                 			end
                                                                                        		|	[< 'Imm imm2 ; 'Reg reg >] -> make_instr (Int64.add offset 5L) [reg] [imm1;imm2]
                                                                                        		|	[< 'Ptr ptr  ; 'Reg reg >] -> begin
                                                                                            				register_ptr ptr (pos+1);
                                                                                            				make_instr (Int64.add offset 5L) [reg] [imm1;0L]
                                                                                          			end
                                               		end
                                             	|	[< 'Ptr ptr >] -> begin
                                                 			register_ptr ptr (pos+1);
                                                 			match stream with parser
                                                                     				[< 'Reg reg1 >] -> begin match stream with parser
                                                                                                                  					[< 'Reg reg2  >] -> make_instr (Int64.add offset 3L) [reg1;reg2] [0L]
                                                                                                              				|	[< 'Imm imm1  >] -> make_instr (Int64.add offset 4L) [reg1] [0L;imm1]
                                                                                                              				|	[< 'Ptr ptr2  >] -> begin
                                                                                                                  						register_ptr ptr2 (pos+2);
                                                                                                                  						make_instr (Int64.add offset 4L) [reg1] [0L;0L]
                                                                                                                					end
                                                   				end
                                                                 			|	[< 'Imm imm1 ; 'Reg reg >] -> make_instr (Int64.add offset 5L) [reg] [0L;imm1]
                                                                 			|	[< 'Ptr ptr2 ; 'Reg reg >] -> begin
                                                                     					register_ptr ptr2 (pos+2);
                                                                     					make_instr (Int64.add offset 5L) [reg] [0L;0L]
                                                                   				end
                                               		end
  	and rule_3 offset stream = match stream with parser
                                                 		[< 'Reg reg1 >] -> begin match stream with parser
                                                                                              			[< 'Reg reg2 ; 'Reg reg3 >] -> make_instr (Int64.add offset 0L) [reg1;reg2;reg3] []
                                                                                          		|	[< 'Imm imm1 ; 'Reg reg2 >] -> make_instr (Int64.add offset 1L) [reg1;reg2]  [imm1]
                                                                                          		|	[< 'Ptr ptr  ; 'Reg reg2 >] -> begin
                                                                                              				register_ptr ptr (pos+1);
                                                                                              				make_instr (Int64.add offset 1L) [reg1;reg2] [0L]
                                                                                            			end
    		end
  	in let code, size = begin match stream with parser
                                                	[< 'Stop >] -> make_instr 0L [] []
                                            |	[< 'Sys   ; 'Imm imm	>] -> make_instr 1L [] [imm]
                                            |	[< 'Call  ; res = rule_0 2L >] -> res
                                            |	[< 'Ret   ; 'Imm imm	>] -> make_instr 4L [] [imm]
                                            |	[< 'Jmp   ; res = rule_0 5L >] -> res
                                            |	[< 'Jz    ; 'Reg reg ; res = rule_0 7L >] -> res
                                            |	[< 'Jnz   ; 'Reg reg ; res = rule_0 9L >] -> res
                                            |	[< 'Lt 	  ; res = rule_1 11L >] -> res
                                            |	[< 'Let   ; res = rule_1 14L >] -> res
                                            |	[< 'Eq 	  ; res = rule_1 17L >] -> res
                                            |	[< 'Load  ; res = rule_1 20L >] -> res
                                            |	[< 'Store ; res = rule_2 23L >] -> res
                                            |	[< 'Push  ; res = rule_0 29L >] -> res
                                            |	[< 'Pop   ; 'Reg reg	>] -> make_instr 31L [reg] []
                                            |	[< 'And   ; res = rule_3 32L >] -> res
                                            |	[< 'Or 	  ; res = rule_3 34L >] -> res
                                            |	[< 'Xor   ; res = rule_3 36L >] -> res
                                            |	[< 'Shl   ; res = rule_1 38L >] -> res
                                            |	[< 'Shr   ; res = rule_1 41L >] -> res
                                            |	[< 'Add   ; res = rule_3 44L >] -> res
                                            |	[< 'Sub   ; res = rule_1 46L >] -> res
                                            |	[< 'Mul   ; res = rule_3 49L >] -> res
                                            |	[< 'Div   ; res = rule_1 51L >] -> res
                                            |	[< 'Bcnt  ; 'Reg reg1 ; 'Reg reg2 >] -> make_instr 54L [reg1;reg2] []
                                            |	[< 'Rev   ; 'Reg reg1 ; 'Reg reg2 >] -> make_instr 55L [reg1;reg2] []
                                            |	[< 'Msb   ; 'Reg reg1 ; 'Reg reg2 >] -> make_instr 56L [reg1;reg2] []
                                            |	[< 'Cvrt  ; 'Reg reg1 ; 'Reg reg2 >] -> make_instr 57L [reg1;reg2] []
                                            |	[< 'Ftrc  ; 'Reg reg1 ; 'Reg reg2 >] -> make_instr 58L [reg1;reg2] []
                                            |	[< 'Flt   ; res = rule_1 59L >] -> res
                                            |	[< 'Flet  ; res = rule_1 62L >] -> res
                                            |	[< 'Fadd  ; res = rule_3 65L >] -> res
                                            |	[< 'Fsub  ; res = rule_1 67L >] -> res
                                            |	[< 'Fmul  ; res = rule_3 70L >] -> res
                                            |	[< 'Fdiv  ; res = rule_1 72L >] -> res
                                            |	[< 'Label lbl >] -> begin register_label lbl pos; [], pos end
                                            |	[< >] -> failwith "Problème dans l'assembleur"
     end
  	in let next, last_pos = code_gen (pos+size) stream
  	in code@next, last_pos
;;

let solve_data last_pos =
  	let get = function _ -> true in
  	let rec aux pos =
    		try
      			let key, lst = Dict.find_first get !data in
        			data := Dict.remove key !data;
        			register_label key pos;
        			lst@(aux (List.length lst + pos))
    		with
        			Not_found -> []
  	in aux last_pos
;;


let solve_labels code =
  	if not (Dict.is_empty !wait_for_lbl) then
    		failwith "Un ou plusieurs addresses sont utilisées mais non déclarées" (* TODO : exc *)
  	else
    		let rec aux () =
      			let get = function _ -> true in
        			try
          				let key, (lst, pos) = Dict.find_first get !lbl in
          				let rec solve_label = function
              					[] -> ()
            				| h::t -> begin code.(h) <- Int64.of_int pos; solve_label t end
          				in solve_label lst
        			with
            				Not_found -> ()
    		in aux ();
      		code
;;

(* TODO Si une parse failure arrive car on arrive en fin de fichier et une règle est incomplète, on peut comparer l'emplacement obtenu avec celui de la fin du fichier pour indiquer que celui-ci n'est pas terminé *)

let assembler file_path =
  try
    		let solved_stream, _ = solve_file file_path [file_path] in (* obtenir un placement absolu *)
    		let lex_stream = lexer solved_stream in
    		let code, last_pos = code_gen 0 lex_stream in
    		let solved_data = solve_data last_pos in
    		let final_code = solve_labels (Array.of_list (code@solved_data)) in
      		Array.iter (function (x:int64) -> Printf.printf "%Lx" x) final_code
  with
      		_ -> failwith "Error"
;;
