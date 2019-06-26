type memory_type = CODE | CONST of int64 | FPU | REG | HEAP of int;; (* HEAP:int means offset *) (* type non nécessaire ?*)
type memory = int * int;; (* id * offset*)
type asm_code =
|	MALLOC of memory * int (* emplacement obtenu * taille en quadwords demandée *)
|	MEM_LOAD_INT of memory * int64
|	MEM_LOAD_FLOAT of memory * int64
|	MEM_LOAD_PTR of memory * int64 (* utile ? *)
|	MEM_FUNC_PTR of string
|	MEM_CALL_DATA of memory list
|	MEM_RET_DATA of memory
|	ASM_MOV of memory * memory (* destination * offset_destination * source * offset_source *)
|	ASM_NEG of memory * memory
|   ASM_FCHS of memory * memory
|   ASM_NOT of memory * memory
|   ASM_FILD of memory * memory
|	ASM_FISTP of memory * memory
|	ASM_ADD of memory * memory * memory
|	ASM_FADD of memory * memory * memory
|	ASM_SUB of memory * memory * memory
|	ASM_FSUB of memory * memory * memory
|	ASM_IMUL of memory * memory * memory
|	ASM_FMUL of memory * memory * memory
|	ASM_IDIV of memory * memory * memory
|	ASM_FDIV of memory * memory * memory
|	ASM_MOD of memory * memory * memory
|	ASM_FPREM of memory * memory * memory
|	ASM_AND of memory * memory * memory
|	ASM_OR of memory * memory * memory
|	ASM_XOR of memory * memory * memory
|	ASM_LT of memory * memory * memory
|	ASM_LET of memory * memory * memory
|	ASM_EQ of memory * memory * memory
|	ASM_NEQ of memory * memory * memory
|	ASM_LABEL of memory (* permet aussi de déclarer un nouveau bloc dans le control flow graph *)
|	ASM_JUMP of memory
|	ASM_JPIF of memory * memory * memory (* label position * alternate label position * memory location to test if is 1 *)
|	ASM_JPNIF of memory * memory * memory (* label position * alternate label position * memory location to test if is 0 *)
|	ASM_CALL of memory * string (* result location * function name *)
|	ASM_RETURN
;;

type record = asm_code list;; (* enregistrer position code pour appel fonction *)
type selection_status = {
	mutable generated_output : (string * int * record) list ; (* nom * nb paramètres * code *)
	mutable variables : ( (*string * *) ((string * memory) list)) list ;
	mutable last_memloc : int ;
	mutable labels : memory list
};;
let status = { generated_output = [] ; variables = [] ; last_memloc = 0 (* le premier emplacement mémoire commencera alors à 1, 0 étant réservé à la mémoire inexistante *) };;

let new_memloc =
	status.last_memloc <- status.last_memloc + 1;
	status.last_memloc, 0
;;

let add_variable name =
	let loc = new_memloc in
	let current_block = (name, loc)::(hd status.variables) in
	let new_variables = current_block::(tl status.variables) in
	status.variables <- new_variables;
	loc
;;

let variable_exists name =
	try
		List.find (function l ->
			try
				List.find (function n, m ->
					if name = n then
						true
					else
						false
				);
				true
			with
				Not_found -> false;
		) status.variables;
		true
	with
		Not_found -> false
;;

let is_function name =
	List.exists (function n, _, _ -> n = name) status.generated_output
;;

let get_memory_location name =
	let res = ref 0 in
	List.find (function l ->
		try
			List.find (function n, m ->
				if name = n then
					res := m; true
				else
					false
			) l
		with
			Not_found -> false;
		true;
	) status.variables;
	!res
;;

let offset loc i =
	fst loc, snd loc + i
;;

let rec selection = function
	Const (Integer c, t) ->
		let res_loc = new_memloc () in
		[MEM_LOAD_INT (res_loc, c)], res_loc
|	Const (Float f, t) ->
		let res_loc = new_memloc () in
		[MEM_LOAD_FLOAT (res_loc, c_float_to_int64 f)], res_loc
|	Const (Bool b, t) ->
		let res_loc = new_memloc () in
		[MEM_LOAD_INT (res_loc, c_bool_to_int64 b)], res_loc
|	Const (Char c, t) ->
		let res_loc = new_memloc () in
		[MEM_LOAD_INT (res_loc, c_char_to_int64 c)], res_loc
|	TupleElem (asts, t) ->
		let instrs = List.map selection asts in
		let instrs_code = List.map fst instrs in
		let code = List.fold_left (prefix @) [] instrs_code in
		let res_loc = new_memloc () in
		let copy_code = List.mapi (function i, (_, mem) -> ASM_MOV (offset res_loc i, mem)) instrs in
		copy_code@[MALLOC (res_loc, Array.length asts)]@code, res_loc
|	ArrayElement (asts, t) ->
		let instrs = List.map selection asts in
		let instrs_code = List.map fst instrs in
		let code = List.fold_left (prefix @) [] instrs_code in
		let res_loc = new_memloc () in
		let copy_code = List.mapi (function i, (_, mem) -> ASM_MOV (offset res_loc i, mem)) instrs in
		copy_code@[MALLOC (res_loc, Array.length asts)]@code, res_loc
|	SumElement (name, asts, t) -> (* TODO : get name special number *)
		let instrs = List.map selection asts in
		let instrs_code = List.map fst instrs in
		let code = List.fold_left (prefix @) [] instrs_code in
		let res_loc = new_memloc () in
		let copy_code = List.mapi (function i, (_, mem) -> ASM_MOV (offset res_loc (i + 1), mem)) instrs in
		let temp_mem = new_memloc () in
		copy_code@[ASM_MOV (res_loc, temp_mem); MEM_LOAD_INT (temp_mem, (*COMPLETE CONSTANT HERE*)); MALLOC (res_loc, Array.length asts + 1)]@code, res_loc
|	FuncElement (name, asts, t) ->
		let params = map selection asts in
		if variable_exists name then (* si name est est une variable on copie l'objet et on le rempli *)
			let mem = get_memory_location name in
			
|	Match (repr, t, asts, do_match, dont_match) -> (* TODO *)
		let rec check_all = function
			[] -> [], (0, 0)
		|	ast::t ->
				match ast with
					Nothing -> check_all t
				|	Const (c, t) -> x
				|	Field (name, t) -> x
		in check_all asts

|   OpUn (NEG, ast, t) ->
		let code, mem = selection ast
		and res_loc = new_memloc () in
		match t with
			INT -> (ASM_NEG res_loc, mem)::code, res_loc
		|   FLOAT -> (ASM_FCHS res_loc, mem)::code, res_loc
|   OpUn (NOT, ast, t) ->
		let code, mem = selection ast
		and res_loc = new_memloc ()
		(ASM_NOT res_loc, mem)::code, res_loc
|   OpBi (ADD, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FADD res_loc, mem1, mem2)::code2@code1, res_loc
		|   INT, INT -> (ASM_ADD res_loc, mem1, mem2)::code2@code1, res_loc
		|   FLOAT, INT ->
				let mem_temp = new_memloc () in
				(ASM_FADD res_loc, mem1, mem_temp)::(ASM_FILD mem_temp, mem2)::code2@code1, res_loc
		|   INT, FLOAT ->
				let mem_temp = new_memloc () in
				(ASM_FADD res_loc, mem_temp, mem2)::(ASM_FILD mem_temp, mem1)::code2@code1, res_loc
|	OpBi (SUB, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FSUB res_loc, mem1, mem2)::code2@code1, res_loc
		|   INT, INT -> (ASM_SUB res_loc, mem1, mem2)::code2@code1, res_loc
		|   FLOAT, INT ->
				let mem_temp = new_memloc () in
				(ASM_FSUB res_loc, mem1, mem_temp)::(ASM_FILD mem_temp, mem2)::code2@code1, res_loc
		|   INT, FLOAT ->
				let mem_temp = new_memloc () in
				(ASM_FSUB res_loc, mem_temp, mem2)::(ASM_FILD mem_temp, mem1)::code2@code1, res_loc
|	OpBi (MUL, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FMUL res_loc, mem1, mem2)::code2@code1, res_loc
		|   INT, INT -> (ASM_IMUL res_loc, mem1, mem2)::code2@code1, res_loc
		|   FLOAT, INT ->
				let mem_temp = new_memloc () in
				(ASM_FMUL res_loc, mem1, mem_temp)::(ASM_FILD mem_temp, mem2)::code2@code1, res_loc
		|   INT, FLOAT ->
				let mem_temp = new_memloc () in
				(ASM_FMUL res_loc, mem_temp, mem2)::(ASM_FILD mem_temp, mem1)::code2@code1, res_loc
|	OpBi (DIV, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FDIV res_loc, mem1, mem2)::code2@code1, res_loc
		|   INT, INT -> (ASM_IDIV res_loc, mem1, mem2)::code2@code1, res_loc
		|   FLOAT, INT ->
				let mem_temp = new_memloc () in
				(ASM_FDIV res_loc, mem1, mem_temp)::(ASM_FILD mem_temp, mem2)::code2@code1, res_loc
		|   INT, FLOAT ->
				let mem_temp = new_memloc () in
				(ASM_FDIV res_loc, mem_temp, mem2)::(ASM_FILD mem_temp, mem1)::code2@code1, res_loc
|	OpBi (MOD, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FPREM res_loc, mem1, mem2)::code2@code1, res_loc
		|   INT, INT -> (ASM_MOD res_loc, mem1, mem2)::code2@code1, res_loc
		|   FLOAT, INT ->
				let mem_temp = new_memloc FPU in
				(ASM_FPREM res_loc, mem1, mem_temp)::(ASM_FILD mem_temp, mem2)::code2@code1, res_loc
		|   INT, FLOAT ->
				let mem_temp = new_memloc FPU in
				(ASM_FPREM res_loc, mem_temp, mem2)::(ASM_FILD mem_temp, mem1)::code2@code1, res_loc
|	OpBi (AND, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		(ASM_AND res_loc, mem1, mem2)::code2@code1, res_loc
|	OpBi (OR, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		(ASM_OR res_loc, mem1, mem2)::code2@code1, res_loc
|	OpBi (XOR, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		(ASM_XOR res_loc, mem1, mem2)::code2@code1, res_loc
|	OpBi (LT, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		(ASM_LT res_loc, mem1, mem2)::code2@code1, res_loc
|	OpBi (LET, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		(ASM_LET res_loc, mem1, mem2)::code2@code1, res_loc
|	OpBi (EQ, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		(ASM_EQ res_loc, mem1, mem2)::code2@code1, res_loc
|	OpBi (NEQ, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2
		and res_loc = new_memloc () in
		(ASM_NEQ res_loc, mem1, mem2)::code2@code1, res_loc
|	IncompleteFuncCall (name, asts, t) ->


|	PureFuncCall (name, asts, t) -> (* TODO *)
		let params = map selection asts
	  	and mem = new_memloc () in
		(ASM_CALL mem, name)::(MEM_CALL_DATA (map snd params))@(map fst params), mem
|	IfElse (cond, if_true, if_false, t) ->
		let cond_code, boolean = selection cond
		and true_code, true_mem = selection if_true
		and false_code, false_mem = selection if_false
		and true_label = new_memloc ()
		and false_label = new_memloc ()
		and end_label = new_memloc () in
		add_label true_label;
		add_label false_label;
		add_label end_label;
		(ASM_LABEL end_label)::(ASM_MOV true_mem, false_mem)::false_code@(ASM_LABEL false_label)::(ASM_JUMP end_label)::true_code@(ASM_LABEL true_label)::(ASM_JPIF false_label, true_label, boolean)::cond_code, true_mem
|	While (cond, body) ->
		let cond_code, boolean = selection cond
		and body_code, res_loc = selection body
		and body_label = new_memloc ()
		and loop_begin = new_memloc ()
		and end_label = new_memloc () in
		add_label body_label;
		add_label loop_begin;
		add_label end_label;
		(ASM_LABEL end_label)::(ASM_JUMP loop_begin)::body_code@(ASM_LABEL body_label)::(ASM_JPNIF end_label, body_label, boolean)::cond_code@[ASM_LABEL loop_begin], res_loc
|	Block (asts, t) -> begin
		status.variables <- []::status.variables;
		let instrs = map selection asts in
		let instrs_code = map fst instrs in
		let code = List.fold_left (prefix @) [] instrs_code
		and _, mem = get_last instrs in
		status.variables <- tl status.variables;
		code, mem
	end
|	Field (name, t) ->
		[], get_memory_location name
|   Let (name, is_rec, params, ast, t) -> begin
		match t with
			Func _ -> begin (* à revoir pour valeurs fonctionnelles *)
				status.variables <- []::status.variables; (* on crée un nouveau bloc de variables *)
				List.iter (function x -> let _ = add_variable x) params;
				let code, mem = selection ast
				and begin_label = new_memloc () in
				add_label begin_label;
				let final_code = ASM_RETURN::(match get_ast_type ast with
					UNIT -> code
				|	_ -> (MEM_RET_DATA mem)::code
				)@[ASM_LABEL begin_label] in
				status.generated_output <- (name, final_code)::status.generated_output;
				status.variables <- tl status.variables;
				[], (0, 0)
			end
		|	_ ->
				if variable_exists name then
					let loc = get_memory_location name
					and code, mem = selection ast in
					(ASM_MOV loc, mem)::code, mem
				else
					let loc = add_variable name
					and code, mem = selection ast in
					(ASM_MOV loc, mem)::code, mem
	end
;;

(*
letin -> block { let ; instructions dans in }

exécution d'appel de fonctions dans le "main" ?
créer un objet traduisant la transmission d'arguments
compléter l'objet
appeler la fonction

déréférencement ???

*)