type memery_type = CODE | CONST of int64 | FPU | REG | HEAP of int;; (* HEAP:int means offset *)
type memory = int * memory_type;;
type asm_code =
	ASM_NEG of memory
|   ASM_FCHS of memory
|   ASM_NOT of memory
|   ASM_FILD of memory
|	ASM_FISTP of memory
|	ASM_ADD of memory * memory
|	ASM_FADD of memory * memory
|	ASM_SUB of memory * memory
|	ASM_FSUB of memory * memory
|	ASM_IMUL of memory * memory
|	ASM_FMUL of memory * memory
|	ASM_IDIV of memory * memory
|	ASM_FDIV of memory * memory
|	ASM_MOD of memory * memory
|	ASM_FPREM of memory * memory
|	ASM_AND of memory * memory
|	ASM_OR of memory * memory
|	ASM_XOR of memory * memory
;;

type record = Code of asm_code list | Data of int64 list;;
module MemLoc = Map.Make(String);;
type selection_status = {
	mutable generated_output : (int * record) list ;
	mutable variables : ( (*string * *) ((string * memory) list)) list ;
	mutable last_memloc : int ;
	mutable mem_locations : (memory_type * int) MemLoc.t (* utile ? *)
};;
let status = { generated_output = [] ; variables = [] ; last_memloc = -1 ; mem_locations = MemLoc.empty };;

let new_memloc memory_type =
	status.last_memloc <- status.last_memloc + 1;
	status.last_memloc, memory_type
;;

let add_variable name mem_type =
	let loc = new_memloc mem_type in
	let current_block = (name, loc)::(hd status.variables) in
	let new_variables = current_block::(tl status.variables) in
	status.variables <- new_variables;
	loc
;;

let get_memory_representation = function (* TDDO *)
;;

let get_memory_location name =
	let res = ref 0, REG in
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

let rec selection = function
	Const (Integer c, t) -> [], new_memloc (CONST c)
|	Const (Float f, t) -> [], new_memloc (CONST (c_float_to_int64 f))
|	Const (Bool b, t) -> [], new_memloc (CONST (c_bool_to_int64 b))
|	Const (Char c, t) -> [], new_memloc (CONST (c_char_to_int64 c))
|	Const (Pair_const p, t) ->
		let repr = get_memory_representation p
		and loc = new_memloc HEAP in
		status.generated_output <- (fst loc)::status.generated_output;
		[], loc
|	Const (Array_const a, t) ->
		let repr = get_memory_representation a
		and loc = new_memloc HEAP in
		status.generated_output <- (fst loc)::status.generated_output;
		[], loc
|	Const (Sum_const s, t) ->
		let repr = get_memory_representation s
		and loc = new_memloc HEAP in
		status.generated_output <- (fst loc)::status.generated_output;
		[], loc
|   OpUn (NEG, ast, t) ->
		let code, mem = selection ast in
		match t with
			INT -> (ASM_NEG mem)::code, new_memloc REG
		|   FLOAT -> (ASM_FCHS mem)::code, new_memloc FPU
|   OpUn (NOT, ast, t) ->
		let code, mem = selection ast in
		(ASM_NOT mem)::code, new_memloc REG
|   OpBi (ADD, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2 in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FADD mem1, mem2)::code2@code1, new_memloc FPU
		|   INT, INT -> (ASM_ADD mem1, mem2)::code2@code1, new_memloc REG
		|   FLOAT, INT ->
				let mem_temp = new_memloc FPU in
				(ASM_FADD mem1, mem_temp)::(ASM_FILD mem2)::code2@code1, new_memloc FPU (* le type de mémoire de mem2 dans ASM_FILD devrait être HEAP ? *)
		|   INT, FLOAT ->
				let mem_temp = new_memloc FPU in
				(ASM_FADD mem_temp, mem2)::(ASM_FILD mem1)::code2@code1, new_memloc FPU
|	OpBi (SUB, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2 in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FSUB mem1, mem2)::code2@code1, new_memloc FPU
		|   INT, INT -> (ASM_SUB mem1, mem2)::code2@code1, new_memloc REG
		|   FLOAT, INT ->
				let mem_temp = new_memloc FPU in
				(ASM_FSUB mem1, mem_temp)::(ASM_FILD mem2)::code2@code1, new_memloc FPU
		|   INT, FLOAT ->
				let mem_temp = new_memloc FPU in
				(ASM_FSUB mem_temp, mem2)::(ASM_FILD mem1)::code2@code1, new_memloc FPU
|	OpBi (MUL, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2 in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FMUL mem1, mem2)::code2@code1, new_memloc FPU
		|   INT, INT -> (ASM_IMUL mem1, mem2)::code2@code1, new_memloc REG
		|   FLOAT, INT ->
				let mem_temp = new_memloc FPU in
				(ASM_FMUL mem1, mem_temp)::(ASM_FILD mem2)::code2@code1, new_memloc FPU
		|   INT, FLOAT ->
				let mem_temp = new_memloc FPU in
				(ASM_FMUL mem_temp, mem2)::(ASM_FILD mem1)::code2@code1, new_memloc FPU
|	OpBi (DIV, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2 in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FDIV mem1, mem2)::code2@code1, new_memloc FPU
		|   INT, INT -> (ASM_IDIV mem1, mem2)::code2@code1, new_memloc REG
		|   FLOAT, INT ->
				let mem_temp = new_memloc FPU in
				(ASM_FDIV mem1, mem_temp)::(ASM_FILD mem2)::code2@code1, new_memloc FPU
		|   INT, FLOAT ->
				let mem_temp = new_memloc FPU in
				(ASM_FDIV mem_temp, mem2)::(ASM_FILD mem1)::code2@code1, new_memloc FPU
|	OpBi (MOD, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2 in
		match get_ast_type ast1, get_ast_type ast2 with
			FLOAT, FLOAT -> (ASM_FPREM mem1, mem2)::code2@code1, new_memloc FPU
		|   INT, INT -> (ASM_MOD mem1, mem2)::code2@code1, new_memloc REG
		|   FLOAT, INT ->
				let mem_temp = new_memloc FPU in
				(ASM_FPREM mem1, mem_temp)::(ASM_FILD mem2)::code2@code1, new_memloc FPU
		|   INT, FLOAT ->
				let mem_temp = new_memloc FPU in
				(ASM_FPREM mem_temp, mem2)::(ASM_FILD mem1)::code2@code1, new_memloc FPU
|	OpBi (AND, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2 in
		(ASM_AND mem1, mem2)::code2@code1, new_memloc REG
|	OpBi (OR, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2 in
		(ASM_OR mem1, mem2)::code2@code1, new_memloc REG
|	OpBi (XOR, ast1, ast2, t) ->
		let code1, mem1 = selection ast1
		and code2, mem2 = selection ast2 in
		(ASM_XOR mem1, mem2)::code2@code1, new_memloc REG
|	FuncCall (name, asts, t) ->
		let params = map selection asts in
		
	  	match t with
		  	Func _, _ -> 
		|	UNIT -> 
		|	_ -> 

|	Block (asts, t) -> begin
		status.variables <- []::status.variables;
		let instrs = map selection asts in
		let instrs_code = map (function c, _ -> c) instrs in
		let code = List.fold_left (prefix @) [] instrs_code
		and _, mem = get_last instrs in
		status.variables <- tl status.variables;
		code, mem
	end
|	Field (name, t) ->
		[], get_memory_location name
		(*match t with
			(* peut pas être type unit, nan ? *)
			INT | BOOL | CHAR -> [], new_memloc REG
		|	FLOAT | Pair _ | Sum _ | Func _ -> [], new_memloc HEAP*)
|   Let (name, is_rec, params, ast) ->
		match get_ast_type ast with
			UNIT | Func _ ->
				let ... add_variable name (CODE)
		|	INT | FLOAT | BOOL | CHAR -> 
				let ... add_variable name (REG)
		|   _ -> let ... add_variable name (HEAP)
;;

(*
liste et constantes en paramètres d'appel de fonction ?
un let toujours allocation mémoire ou code ? e.g. let x = 3 + y in factorial x
exécution d'appel de fonctions dans le "main" ?
*)