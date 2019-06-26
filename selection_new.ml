type constraint =
	RAX
|	RCX
|	RDX
|	RBX
|	RSI
|	RDI
|	RSP
|	RBP
|	R8
|	R9
|	R10
|	R11
|	R12
|	R13
|	R14
|	R15
|	ZF
|	CF
|	OF
|	SF
|	FPU
|	FPU_0
|	MEM (* sous-entend déréférencement *)
;;
let regs = [ RAX; RCX; RDX; RBX; RSI; RDI; R8; R9; R10; R11; R12; R13; R14; R15 ];; (* on exclue les registres RSP et RBP *)
let flags = [ OF; SF; ZF; CF ];;
let call_constrs = [ RAX; RCX; RDX; R8; R9; R10; R11 ];;

type memory =
	Data of int * (constraint list) (* id * liste de contraintes registres *)
|	Ptr of memory * memory * int (* id * index * offset : utilisé pour déréférencer les pointeurs, base et index doivent avoir une contrainte GPR *)
|	Constraint of constraint list (* utilisé pour les registres | flags | ... détruits par une instruction -> doivent être pris en compte dans le graphe d'interférences *)
;;
let no_mem = Data -1, [];;

type asm_code =
|	MEM_SET_CALL_DATA of memory list (* memory locations that serve as arguments for the function to call *)
|	MEM_GET_CALL_DATA of memory list (* memory locations that describes args in function called *)
|	MEM_SET_RET_DATA of memory (* memory used to return data from a function *)
|	MEM_GET_RET_DATA of memory (* memory from which the program can retrieve the result of a function call *)

|	ASM_NOP
|	ASM_ERROR

(* no flags affected *)
|	ASM_MOV_C of memory * int64 (* mov r64,imm64 *)
|	ASM_MOV of memory * memory (* destination * source *) (* mov rm64,r64 mov r64,rm64 *)

|	ASM_FILD of memory * memory (* destination * source *) (* fild m64int *)
|	ASM_FISTP of memory * memory (* fistp m64int *)
|	ASM_FZERO of memory (* st(i) <- 0 *)
|	ASM_FLD of memory * memory (* fld m64fp *)
|	ASM_FST of memory * memory (* fst(p) m64fp *)

|	ASM_NEG of memory (* neg rm64 *)
|	ASM_FCHS of memory * memory (* fchs(st(0)) *)
|	ASM_NOT of memory * memory (* not rm64 *)

(* destination * opérande 1 * opérande 2 *)
|	ASM_ADD of memory * memory * memory (* add rm64,r64 add r64,rm64 *)
|	ASM_FADD of memory * memory * memory (* fadd (st(0)),m64fp fadd st(0),st(i) fadd st(i),st(0) faddp st(i),st(0) *)
|	ASM_SUB of memory * memory * memory (* sub rm64,r64 sub r64,rm64 *)
|	ASM_FSUB of memory * memory * memory (* fsub (st(0)),m64fp fsub st(0),st(i) fsub st(i),st(0) fsubp st(i),st(0) *)
|	ASM_IMUL of memory * memory * memory (* imul r64,rm64 *)
|	ASM_FMUL of memory * memory * memory (* fmul (st(0)),m64fp fmul st(0),st(i) fmul st(i),st(0) fmulp st(i),st(0) *)
|	ASM_IDIV of memory * memory * memory (* rax <- idiv rm64 (rdx doit être 0) *)
|	ASM_FDIV of memory * memory * memory (* fdiv (st(0)),m64fp fdiv st(0),st(i) fdiv st(i),st(0) fdivp st(i),st(0) *)
|	ASM_MOD of memory * memory * memory (* rdx <- idiv rm64 (rdx doit être 0) *)
|	ASM_FPREM of memory * memory * memory (* st(0) <- fprem (st(0)/st(1)) *)
|	ASM_AND of memory * memory * memory (* _and rm64,r64 _and r64,rm64 *)
|	ASM_OR of memory * memory * memory (* _or rm64,r64 _or r64,rm64 *)
|	ASM_XOR of memory * memory * memory (* _xor rm64,r64 _xor r64,rm64 *)

|	ASM_LT of memory * memory * memory (* cmp rm64,r64 cmp r64,rm64 (sf /= of) *)
|	ASM_LET of memory * memory * memory (* (zf = 1 or sf /= of) *)
|	ASM_EQ of memory * memory * memory (* (zf = 1) *)
|	ASM_NEQ of memory * memory * memory (* (zf = 0) *)
|	ASM_FLT of memory * memory * memory (* (cf = 1) *)
|	ASM_FLET of memory * memory * memory (* (cf = 1 or zf = 1) *)
|	ASM_FEQ of memory * memory * memory (* (zf = 1) *)
|	ASM_FNEQ of memory * memory * memory (* (zf = 0) *)
|	ASM_TEST of memory * memory * memory (* zf <- mem and mem (sf, pf devraient aussi être modifiées en tant que mémoire) *)

|	ASM_JPIF of memory (* valeur à tester *) (* gérer si élement à tester est un flag ou une opérande mémoire : dépend contrainte sur id mémoire*)
|	ASM_JPNIF of memory (* valeur à tester *)

|	ASM_CALL of string
|	ASM_RET
;;

type link = { source: cfg_block; target: cfg_block option };;
type cfg_block = {
	code: asm_code;
	mutable previous: link list;
	mutable next: link option;
	mutable alternate: link option;
	def: memory list;
	use: memory list;
	in_: memory list;
	last_in: memory list;
	out: memory list;
	last_out: memory list;
	process_nb: int;
};;

let new_cfg code def use =
	let cfg = { code = code; previous = []; next = None; alternate = None; def = def; use = use; in_ = []; last_in = []; out = []; last_out = []; process_nb = 0 } in
	cfg.next <- Some { parent = cfg; target = None };
	cfg.alternate <- Some { parent = cfg; target = None };
	cfg
;;

let join_links links target =
	List.iter (function link -> link.target <- Some target) links;
	target.previous <- List.union links target.previous
;;

(* peut changer *)
type var_tree =
	Root
|	Variable of string * memory (* nom * emplacement *)
|	Node of var_tree * (var_tree list) ref (* arbre parent * arbres fils *)
;;

let output = ref [];; (* (cfg_block * cfg_block) list : liste de paire de cfg_blocks représentant le début et la fin de chaque fonction créée *)
let environnement = ref Node Root, [];;
let last_memloc = ref 0;;

let create_local_env () =
	let Node (parent, var_list_ref) = !environnement in
	let new_env = Node ( Node (parent, var_list_ref), ref [] ) in
	var_list_ref := new_env::(!var_list_ref);
	environnement := new_env
;;

let exit_local_env () =
	let Node (parent, _) = !environnement in
	environnement := parent
;;

let new_memloc constraints =
	last_memloc := !last_memloc + 1;
	Data !last_memloc, constraints
;;

let offset mem k = match mem with
	Data id, constr -> Ptr (Data id, constr), no_res, k (* si contraintes pas sur un GPR, ajouter qqch, mais est-ce que cela peut arriver ? *)
|	Ptr base, index, offset -> Ptr base, index, (offset + k)
;;

let constrain constraints =
	List.map (function c -> Data -1, [c]) constraints
;;

let constrain_mem constr = function
	Data id, constrs -> Data id, constr::constrs
|	Ptr base, index, offset -> Ptr base, index, offset
|	Constraint constrs -> constr::constrs
;;

let index mem index = match mem with
	Data id, constr -> Ptr (Data id, constr), index, 0
|	Ptr base, _, o -> Ptr base, index, o
;;

let create_variable name =
	let Node (_, var_list_ref) = !environnement
	and loc = new_memloc [] in (* pas de contrainte mémoire pour les variables ? *)
	var_list_ref := Variable (name, loc) :: (!var_list_ref);
	loc
;;

let variable_location name =
	let search_in = function (* search_in fait un parcours en largeur montant (on ne descend jamais dans des environnements plus locaux *)
	|	ROOT -> raise Not_found
	|	Node parent, list ->
			match List.findopt (function Variable n, loc -> n = name | _ -> false) !list with
				Some Variable _, loc -> loc
			|	_ -> search_in parent;
	in search_in !environnement
;;

let malloc loc size =
	let size_loc = new_memloc regs
	and loc = new_memloc [RAX]
	and boolean = new_memloc [ZF] in
	let size_cfg = new_cfg (ASM_MOV_C size_loc, Int64.from_int (8*size)) [size_loc] []
	and arg_cfg = new_cfg (MEM_SET_CALL_DATA [size_loc]) (constrain [RSP]) [size_loc] (* RSP ??? TODO : vérifier !*)
	and alloc_cfg = new_cfg (ASM_CALL "_malloc") (constrain call_constrs) []
	and get_cfg = new_cfg (MEM_GET_RET_DATA loc) [loc] []
	and test_cfg = newcfg (ASM_TEST boolean, loc, loc) boolean::(constrain flags)  [loc]
	and jmp_cfg = new_cfg (ASM_JPNIF boolean) [] [boolean]
	and err_cfg = new_cfg ASM_ERROR [] [] in
	join_links [size_loc.next] arg_cfg;
	join_links [arg_cfg.next] alloc_cfg;
	join_links [alloc_cfg.next] get_cfg;
	join_links [get_cfg.next] test_cfg;
	join_links [test_cfg.next] jmp_cfg;
	join_links [jmp_cfg.next] err_cfg;
	size_loc, [jmp_cfg.alternate], loc
;;


(* TODO *) 		 
let get_sum_id name =

;;

(* renvoie un cfg correspondant à l'opération effectuée, les liens sortant de ce cfg et l'emplacement mémoire contenant le résultat de l'opération *)
let rec selection = function
	Nothing ->
		let nop_cfg = new_cfg ASM_NOP [] [] in
		nop_cfg, [nop_cfg.next], no_res
|	Error ->
		let err_cfg = new_cfg ASM_ERROR [] [] in
		err_cfg, [err_cfg.next], no_res
|	Const (Integer i, _) ->
		let loc = new_memloc regs in
		let cfg = new_cfg (ASM_MOV_C loc, i) [loc] [] in
		cfg, [cfg.next], loc
|	Const (Float f, _) -> (* passer l'élément au fpu ? -> non, car peut être transmi en paramètre à une fonction -> passage par GPRs et stack *)
		let loc = new_memloc regs in
		let cfg = new_cfg (ASM_MOV_C loc, Int64.bits_of_float f) [loc] [] in
		cfg, [cfg.next], loc
|	Const (Bool b, _) ->
		let int_repr = match b with
			true -> Int64.lognot (Int64.zero)
		| 	false -> Int64.zero in
		let loc = new_memloc regs in
		let cfg = new_cfg (ASM_MOV_C loc, int_repr) [loc] [] in
		cfg, [cfg.next], loc
|	Const (Char c, _) ->
		let loc = new_memloc regs in
		let cfg = new_cfg (ASM_MOV_C loc, Int64.of_int (int_of_char c)) [loc] [] in
		cfg, [cfg.next], loc
|	TupleElement (asts, _) -> (* déréférencement ? *)
		let lst = List.map selection asts
		and loc = new_memloc regs in
		let assigns = List.mapi (function index, (cfg, cfg_end, res) ->
			let copy_cfg = new_cfg (ASM_MOV (offset loc i), res) [] [loc; res] in
			join_links cfg_end copy_cfg;
			cfg, [copy_cfg.next]
		) lst
		and alloc_cfg, alloc_end = malloc loc, List.length asts in
		let final_cfg, final_end = List.fold_left (function (fst_cfg, fst_end), (snd_cfg, snd_end) -> (* pas besoin de l'emplacement du résultat de chaque cfg ici car celui-ci est stocké dans l'objet *)
			join_links fst_end snd_cfg;
			fst_cfg, snd_end
		) (alloc_cfg, alloc_end) assigns
		in
		final_cfg, final_end, loc
|	ArrayElement (asts, _) -> (* déréférencement ? *)
		let lst = List.map selection asts
		and loc = new_memloc regs in
		let assigns = List.mapi (function index, (cfg, cfg_end, res) ->
			let copy_cfg = new_cfg (ASM_MOV (offset loc (i+1)), res) [] [loc; res] in (* l'assignement est décalé car le premier élément du tableau contient sa taille *)
			join_links cfg_end copy_cfg;
			cfg, [copy_cfg.next]
		) lst
		and alloc_cfg, alloc_end = malloc loc List.length asts
		and size_loc = new_memloc regs in
		let size_cfg = new_cfg (ASM_MOV_C size_loc, Int64.of_int (List.length asts)) [size_loc] [] in
		let mov_cfg = new_cfg (ASM_MOV (offset loc 0), size_loc) [] [loc; size_loc] in (* on place la talle du tableau en r64 puis on le place en m64 *)
		join_links [size_cfg.next] mov_cfg;
		let final_cfg, final_end = List.fold_left (function (fst_cfg, fst_end), (snd_cfg, snd_end) -> (* pas besoin de l'emplacement du résultat de chaque cfg ici car celui-ci est stocké dans l'objet *)
			join_links fst_end snd_cfg;
			fst_cfg, snd_end
		) (alloc_cfg, alloc_end) (size_cfg, [mov_cfg.next])::assigns
		in
		final_cfg, final_end, loc
|	SumElement (name, ast, _) -> (* déréférencement ? *)
		if ast = Nothing then
			let loc = new_memloc regs in
			let alloc_cfg, alloc_end = malloc loc 1
			and id_cfg = new_cfg (ASM_MOV_C (offset loc 0), get_sum_id name) [] [loc] in
			join_links alloc_end id_cfg;
			alloc_cfg, [id_cfg.next], loc
		else
			let content_cfg, content_end, content_loc = selection ast
			and loc = new_memloc () in
			let alloc_cfg, alloc_end = malloc loc 2
			and mov_cfg = new_cfg (ASM_MOV (offset loc 1), content_loc) [] [loc; content_loc]
			and id_cfg = new_cfg (ASM_MOV_C (offset loc 0), get_sum_id name) [] [loc] in
			join_links content_end alloc_cfg;
			join_links alloc_end mov_cfg;
			join_links [mov_cfg.next] id_cfg;
			content_cfg, [id_cfg.next], loc

		 
(*
|	FuncElement ->
*)


|	Match (element, matchs, _) ->
		let rec test_pattern loc = function
			ConstPattern (Integer i) -> (* idem *)
				let const_loc = new_memloc regs
				and boolean = new_memloc [ZF] in
				let const_cfg = new_cfg (ASM_MOV_C const_loc, i) [const_loc] []
				and test_cfg = new_cfg (ASM_EQ boolean, loc, const_loc) boolean::constrain flags [loc; const_loc] in
				join_links [const_cfg.next] test_cfg;
				const_cfg, [test_cfg.next], boolean
		|	ConstPattern (Float f) -> (* idem /!\ float *)
				let const_loc = new_memloc regs
				and fpu_loc = new_memloc [FPU]
				and boolean = new_memloc [ZF] in
				let const_cfg = new_cfg (ASM_MOV_C const_loc, i) [const_loc] []
				and ld_fpu_cfg = new_cfg (ASM_FLD fpu_loc, const_loc) [fpu_loc] [const_loc]
				and test_cfg = new_cfg (ASM_FEQ boolean, loc, fpu_loc) boolean::constrain flags [loc; fpu_loc] in
				join_links [const_cfg.next] ld_fpu_cfg;
				join_links [ld_fpu_cfg.next] test_cfg;
				const_cfg, [test_cfg.next], boolean
		|	ConstPattern (Bool b) -> (* idem *)
				let const_loc = new_memloc regs
				and boolean = new_memloc [ZF] in
				let const_cfg = new_cfg (ASM_MOV_C const_loc, i) [const_loc] []
				and test_cfg = new_cfg (ASM_EQ boolean, loc, const_loc) boolean::constrain flags [loc; const_loc] in
				join_links [const_cfg.next] test_cfg;
				const_cfg, [test_cfg.next], boolean
		|	ConstPattern (Char c) -> (* simple test valeur *)
				let const_loc = new_memloc regs
				and boolean = new_memloc [ZF] in
				let const_cfg = new_cfg (ASM_MOV_C const_loc, i) [const_loc] []
				and test_cfg = new_cfg (ASM_EQ boolean, loc, const_loc) boolean::constrain flags [loc; const_loc] in
				join_links [const_cfg.next] test_cfg;
				const_cfg, [test_cfg.next], boolean
		|	TuplePattern patterns -> (* idem array, sauf aucun test de taille *)
				let pattern_cfg = List.mapi (function i, (pattern) ->
					let ptr_elem = new_memloc regs in
					let deref_cfg = new_cfg (ASM_MOV ptr_elem, offset loc (i+1)) [ptr_elem] [loc]
					and test_cfg, test_end, test_res = test_pattern ptr_elem pattern in
					join_links [deref_cfg.next] test_cfg;
					deref_cfg, test_end, test_res
				) patterns in
				let test_cfg, test_end, test_res, wrong_links = List.fold_left (function ((fst_cfg, fst_end, fst_res), wrong_links), (snd_cfg, snd_end, snd_res) ->
					let test_fst_cfg = new_cfg (ASM_JPNIF fst_res) [] [fst_res] in
					join_links fst_end test_fst_cfg;
					join_links [test_fst_cfg.next] snd_cfg;
					(fst_cfg, snd_end, snd_res), (test_fst_cfg.alternate)::wrong_links
				) (hd pattern_cfg, []) (tl pattern_cfg) in
				let false_cfg = new_cfg (ASM_MOV_C test_res, Int64.zero) [test_res] [] in
				join_links [size_cfg.next] test_size_cfg;
				join_links [test_size_cfg.next] jmp_cfg;
				join_links (jmp_cfg.next)::wrong_links false_cfg;
				join_links [jmp_cfg.alternate] test_cfg;
				size_cfg, (false_cfg)::test_end, test_res
		|	ArrayPattern patterns -> (* on teste taille, puis on teste pattern 1, si bon on continue à 2, sinon on relâche, etc *)
				let nb_patterns = List.length patterns
				and size = new_memloc regs
				and boolean = new_memloc [ZF] in
				let size_cfg = new_cfg (ASM_MOV_C size, nb_patterns) [size] []
				and test_size_cfg = new_cfg (ASM_NEQ boolean, size, offset loc 0) boolean::constrain flags [size; loc]
				and jmp_cfg = new_cfg (ASM_JPNIF boolean) [] [boolean]
				if nb_patterns = 0 then
					join_links [size_cfg.next] test_size_cfg;
					size_cfg, [test_size_cfg.next], boolean
				else
					let pattern_cfg = List.mapi (function i, (pattern) ->
						let ptr_elem = new_memloc regs in
						let deref_cfg = new_cfg (ASM_MOV ptr_elem, offset loc (i+1)) [ptr_elem] [loc]
						and test_cfg, test_end, test_res = test_pattern ptr_elem pattern in
						join_links [deref_cfg.next] test_cfg;
						deref_cfg, test_end, test_res
					) patterns in
					let test_cfg, test_end, test_res, wrong_links = List.fold_left (function ((fst_cfg, fst_end, fst_res), wrong_links), (snd_cfg, snd_end, snd_res) ->
						let test_fst_cfg = new_cfg (ASM_JPNIF fst_res) [] [fst_res] in
						join_links fst_end test_fst_cfg;
						join_links [test_fst_cfg.next] snd_cfg;
						(fst_cfg, snd_end, snd_res), (test_fst_cfg.alternate)::wrong_links
					) (hd pattern_cfg, []) (tl pattern_cfg) in
					let false_cfg = new_cfg (ASM_MOV_C test_res, Int64.zero) [test_res] [] in
					join_links [size_cfg.next] test_size_cfg;
					join_links [test_size_cfg.next] jmp_cfg;
					join_links (jmp_cfg.next)::wrong_links false_cfg;
					join_links [jmp_cfg.alternate] test_cfg;
					size_cfg, (false_cfg)::test_end, test_res
		|	SumPattern name, pattern -> (* si name correspond, on teste pattern correspondant avec [loc+1] *)
				let id = new_memloc regs
				and boolean = new_memloc [ZF]
				and ptr_elem = new_memloc regs in
				let id_cfg = new_cfg (ASM_MOV_C id, get_sum_id name) [id] []
				and test_id_cfg = new_cfg (ASM_NEQ boolean, id, offset loc 0) boolean::constrain flags [id; loc]
				and jmp_cfg = new_cfg (ASM_JPNIF boolean) [] [boolean]
				and deref_cfg = new_cfg (ASM_MOV ptr_elem, offset loc 1) [ptr_elem] [loc]
				and pattern_cfg, pattern_end, pattern_res = test_pattern ptr_elem pattern in (* la location transmise à test_pattern est toujours une location Data, jamais qqch de déréférencé (d'où la permission plus haut de déréférencer loc pour obtenir l'id de l'objet) *)
				let false_cfg = new_cfg (ASM_MOV_C pattern_res, Int64.zero) [pattern_res] [] in
				join_links [id_cfg.next] test_id_cfg;
				join_links [test_id_cfg.next] jmp_cfg;
				join_links [jmp_cfg.next] false_cfg;
				join_links [jmp.alternate] deref_cfg;
				join_links [deref_cfg.next] pattern_cfg;
				id_cfg, (false_cfg.next)::pattern_end, pattern_res
		|	VarPattern name -> (* renvoie toujours vrai et ajoute la variable par son nom (ne remplace pas les variables de même nom) *)
				let res = new_memloc regs in
				let true_cfg = new_cfg (ASM_MOV_C res, Int64.lognot (Int64.zero)) [res] [] in
				create_variable name;
				true_cfg, [true_cfg.next], res
		|	AnyPattern -> (* renvoie toujours vrai *)
				let res = new_memloc regs in
				let true_cfg = new_cfg (ASM_MOV_C res, Int64.lognot (Int64.zero)) [res] [] in
				true_cfg, [true_cfg.next], res
		in
		let elem_cfg, elem_end, elem_res = selection element
		and err_cfg = new_cfg ASM_ERROR [] [] in
		let patterns_cfgs = List.map (function pattern, conseq ->
			create_local_env ();
			let test_cfg, test_end, test_res = test_pattern elem_res pattern
			and conseq_cfg, conseq_end, conseq_res = selection conseq in
			let jmp_cfg = new_cfg (ASM_JPNIF test_res) [] [test_res] in
			join_links test_end jmp_cfg;
			join_links [jmp_cfg.next] conseq;
			test_cfg, conseq_end, [jmp_cfg.alternate], conseq_res (* on retourne ici le lien qui terminera tout le match si la conséquence est exécutée, et on retourne aussi le lien qui pointera vers un autre test si celui-ci échoue *)
		) matchs
		in
		let test_cfg, test_end, test_wrong, test_res = List.fold_left (function (fst_cfg, fst_end, fst_wrong, fst_res), (snd_cfg, snd_end, snd_wrong, snd_res) ->
			let mov_res = new_cfg (ASM_MOV fst_res, snd_res) [fst_res] [snd_res] in
			join_links fst_wrong snd_cfg;
			join_links snd_end mov_cfg;
			fst_cfg, (mov_cfg.next)::fst_end, snd_wrong, fst_res
		) (hd pattern_cfg) (tl patterns_cfgs)
		in
		join_links test_wrong err_cfg;
		test_cfg, test_end, test_res
|	ArrayAccess (vect, index, _) -> (* déréférencement ? *)
		let vect_cfg, vect_end, vect_res = selection vect
		and index_cfg, index_end, index_res = selection index
		and bool_0_loc = new_memloc [SF; OF]
		and bool_1_loc = new_memloc [ZF; SF]
		and bool_2_loc = new_memloc [ZF]
		and res = new_memloc regs
		and zero_loc = new_memloc regs in
		let sup_bound_cfg = new_cfg (ASM_LT bool_0_loc, index_res, (offset vect_res 0)) bool_0_loc::(constrain flags) [index_res; vect_res]
		and inf_bound_cfg = new_cfg (ASM_LET bool_1_loc, zero_loc, index_res) bool_1_loc::(constrain flags) [zero_loc; index_res]
		and zero_cfg = new_cfg (ASM_MOV_C zero_loc, 0) [zero_loc] []
		and cond_cfg = new_cfg (ASM_AND bool_2_loc, bool_0_loc, bool_1_loc) bool_2_loc::(constrain flags) [bool_0_loc; bool_1_loc]
		and jmp_cfg = new_cfg (ASM_JPNIF bool_2_loc) [] [bool_2_loc]
		and err_cfg = new_cfg ASM_ERROR [] []
		and element_loc = index vect_res index_res in
		let element_cfg = new_cfg (ASM_MOV res, (offset element_loc 1)) [res] [element_loc] in
		join_links vect_end index_cfg;
		join_links index_end sup_bound_cfg;
		join_links [sup_bound_cfg.next] zero_cfg;
		join_links [zero_cfg.next] inf_bound_cfg;
		join_links [inf_bound_cfg.next] jmp_cfg;
		join_links [jmp_cfg.next] err_cfg;
		join_links [jmp_cfg.alternate] element_cfg;
		vect_cfg, [element_cfg.next], res
|	OpUn (NEG, ast, t) ->
		let op_cfg, op_end, res = selection ast in
		let cfg = match t with
			INT -> new_cfg (ASM_NEG res) res::(constrain flags) [res]
		|	FLOAT -> new_cfg (ASM_FCHS res) [res] [res]
		in
		join_links op_end cfg;
		op_cfg, [cfg.next], res
|	OpUn (NOT, ast, t) ->
		let op_cfg, op_end, res = selection ast in
		let cfg = (ASM_NOT res) [res] [res] in
		join_links op_end cfg;
		op_cfg, [cfg.next], res
|	OpBi (ADD, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc () in
		match get_ast_type ast1, get_ast_type ast2 with
			INT, INT -> begin
				let cfg = new_cfg (ASM_ADD res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* add est commutatif, on pourrait donc choisir d'écraser fst_res ou snd_res en fonction de celui qui a le moins de lifetime (ou autre méthode : moins d'occurence, ...) on choisit donc de mettre aucune définition pour l'instant autre que res, qui sera remplacé par le choix déterminé par la liveness analysis *)
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, FLOAT -> begin
				let cfg = new_cfg (ASM_FADD res, fst_res, snd_res) [res] [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, INT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, snd_res) [cast_res] [snd_res]
				and cfg = new_cfg (ASM_FADD res, fst_res, cast_res) [res] [fst_res; cast_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		|	INT, FLOAT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, fst_res) [cast_res] [fst_res]
				and cfg = new_cfg (ASM_FADD res, cast_res, snd_res) [res] [cast_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		end
|	OpBi (SUB, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc () in
		match get_ast_type ast1, get_ast_type ast2 with
			INT, INT -> begin
				let cfg = new_cfg (ASM_SUB fst_res, snd_res) [fst_res] [fst_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], fst_res
				end
		|	FLOAT, FLOAT -> begin
				let cfg = new_cfg (ASM_FSUB res, fst_res, snd_res) [res] [fst_res; snd_res] in (* idem, choix entre st(0) et st(i) *)
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, INT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, snd_res) [cast_res] [snd_res]
				and cfg = new_cfg (ASM_FSUB res, fst_res, cast_res) [res] [fst_res; cast_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		|	INT, FLOAT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, fst_res) [cast_res] [fst_res]
				and cfg = new_cfg (ASM_FSUB res, cast_res, snd_res) [res] [cast_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		end
|	OpBi (MUL, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc () in
		match get_ast_type ast1, get_ast_type ast2 with
			INT, INT -> begin
				let cfg = new_cfg (ASM_IMUL res, fst_res, snd_res) [res] [fst_res; snd_res] in (* idem, un seul type d'instruction supporté ici cependant : imul r64, r/m64 *)
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, FLOAT -> begin
				let cfg = new_cfg (ASM_FMUL res, fst_res, snd_res) [res] [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, INT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, snd_res) [cast_res] [snd_res]
				and cfg = new_cfg (ASM_FMUL res, fst_res, cast_res) [res] [fst_res; cast_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		|	INT, FLOAT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, fst_res) [cast_res] [fst_res]
				and cfg = new_cfg (ASM_FMUL res, cast_res, snd_res) [res] [cast_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		end
|	OpBi (DIV, ast1, ast2, _) -> begin (* penser à vérifier connexions si division par zéro ! *)
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc () in
		let err_cfg = new_cfg ASM_ERROR [] []
		and bool_loc = new_memloc () in
		let jmp_cfg = new_cfg (ASM_JPIF bool_loc) [] [bool_loc]
		join_links fst_end snd_cfg;
		join_links snd_end zero_cfg;
		join_links [jmp_cfg.next] err_cfg;
		match get_ast_type ast1, get_ast_type ast2 with
			INT, INT -> begin
				let zero_loc = new_memloc regs in
				let zero_cfg = new_cfg (ASM_MOV_C zero_loc, INT64.zero) [zero_loc] []
				and test_cfg = new_cfg (ASM_NEQ bool_loc, snd_res, zero_loc) bool_loc::(constrain flags) [snd_res; zero_loc]
				and cfg = new_cfg (ASM_IDIV fst_res, snd_res) fst_res::(constrain [RDX]) [fst_res; snd_res] in (* détruit automatiquement RDX:RAX *)
				join_links [zero_cfg.next] test_cfg;
				join_links [jmp_cfg.alternate] cfg;
				fst_cfg, [cfg.next], fst_res
				end
		|	FLOAT, FLOAT -> begin
				let zero_loc = new_memloc [FPU] in
				let zero_cfg = new_cfg (ASM_FZERO zero_loc) [zero_loc] []
				and test_cfg = new_cfg (ASM_FNEQ bool_loc, snd_res, zero_loc) bool_loc::(constrain flags) [snd_res; zero_loc]
				and cfg = new_cfg (ASM_FDIV res, fst_res, snd_res) [res] [fst_res; snd_res] in (* idem *)
				join_links [zero_cfg.next] test_cfg;
				join_links [jmp_cfg.alternate] cfg
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, INT -> begin
				let zero_loc = new_memloc [FPU]
				and cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, snd_res) [cast_res] [snd_res]
				and zero_cfg = new_cfg (ASM_FZERO zero_loc) [zero_loc] []
				and test_cfg = new_cfg (ASM_FNEQ bool_loc, cast_res, zero_loc) bool_loc::(constrain flags) [cast_res; zero_loc]
				and cfg = new_cfg (ASM_FDIV res, fst_res, cast_res) [res] [fst_res; cast_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		|	INT, FLOAT -> begin
				let zero_loc = new_memloc [FPU]
				and cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, fst_res) [cast_res] [fst_res]
				and zero_cfg = new_cfg (ASM_FZERO zero_loc) [zero_loc] []
				and test_cfg = new_cfg (ASM_FNEQ bool_loc, cast_res, zero_loc) bool_loc::(constrain flags) [cast_res; zero_loc]
				and cfg = new_cfg (ASM_FDIV res, cast_res, snd_res) [res] [cast_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		end
|	OpBi (MOD, ast1, ast2, _) -> begin (* penser à vérifier connexions si division par zéro ! *)
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc () in
		let err_cfg = new_cfg ASM_ERROR [] []
		and bool_loc = new_memloc () in
		let jmp_cfg = new_cfg (ASM_JPIF bool_loc) [] [bool_loc]
		join_links fst_end snd_cfg;
		join_links snd_end zero_cfg;
		join_links [jmp_cfg.next] err_cfg;
		match get_ast_type ast1, get_ast_type ast2 with
			INT, INT -> begin
				let zero_loc = new_memloc regs in
				let zero_cfg = new_cfg (ASM_MOV_C zero_loc, Int64.zero) [zero_loc] []
				and test_cfg = new_cfg (ASM_NEQ bool_loc, snd_res, zero_loc) bool_loc::(constrain flags) [snd_res; zero_loc]
				and cfg = new_cfg (ASM_MOD fst_res, snd_res) fst_res::(constrain [RDX]) [fst_res; snd_res] in
				join_links [zero_cfg.next] test_cfg;
				join_links [jmp_cfg.alternate] cfg;
				fst_cfg, [cfg.next], fst_res
				end
		|	FLOAT, FLOAT -> begin
				let zero_loc = new_memloc [FPU] in
				let zero_cfg = new_cfg (ASM_FZERO zero_loc) [zero_loc] []
				and test_cfg = new_cfg (ASM_FNEQ bool_loc, snd_res, zero_loc) bool_loc::(constrain flags) [snd_res; zero_loc]
				and cfg = new_cfg (ASM_FPREM res, fst_res, snd_res) [res] [fst_res; snd_res] in
				join_links [zero_cfg.next] test_cfg;
				join_links [jmp_cfg.alternate] cfg
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, INT -> begin
				let zero_loc = new_memloc [FPU] in
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, snd_res) [cast_res] [snd_res]
				and zero_cfg = new_cfg (ASM_FZERO zero_loc) [zero_loc] []
				and test_cfg = new_cfg (ASM_FNEQ bool_loc, cast_res, zero_loc) bool_loc::(constrain flags) [cast_res; zero_loc]
				and cfg = new_cfg (ASM_FPREM res, fst_res, cast_res) [res] [fst_res; cast_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		|	INT, FLOAT -> begin
				let zero_loc = new_memloc [FPU] in
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, fst_res) [cast_res] [fst_res]
				and zero_cfg = new_cfg (ASM_FZERO zero_loc) [zero_loc] []
				and test_cfg = new_cfg (ASM_FNEQ bool_loc, cast_res, zero_loc) bool_loc::(constrain flags) [cast_res; zero_loc]
				and cfg = new_cfg (ASM_FPREM res, cast_res, snd_res) [res] [cast_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		end
|	OpBi (AND, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc () in
		let cfg = new_cfg (ASM_AND res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
		join_links fst_end snd_cfg;
		join_links snd_end cfg;
		fst_cfg, [cfg.next], res
		end
|	OpBi (OR, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc () in
		let cfg = new_cfg (ASM_OR res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
		join_links fst_end snd_cfg;
		join_links snd_end cfg;
		fst_cfg, [cfg.next], res
		end
|	OpBi (XOR, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc () in
		let cfg = new_cfg (ASM_XOR res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
		join_links fst_end snd_cfg;
		join_links snd_end cfg;
		fst_cfg, [cfg.next], res
		end
|	OpBi (LT, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc flags in
		match get_ast_type ast1, get_ast_type ast2 with
			INT, INT -> begin
				let cfg = new_cfg (ASM_LT res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, FLOAT -> begin
				let cfg = new_cfg (ASM_FLT res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, INT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, snd_res) [cast_res] [snd_res]
				and cfg = new_cfg (ASM_FLT res, fst_res, cast_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		|	INT, FLOAT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, fst_res) [cast_res] [fst_res]
				and cfg = new_cfg (ASM_FLT res, cast_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		end
|	OpBi (LET, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc flags in
		match get_ast_type ast1, get_ast_type ast2 with
			INT, INT -> begin
				let cfg = new_cfg (ASM_LET res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, FLOAT -> begin
				let cfg = new_cfg (ASM_FLET res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, INT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, snd_res) [cast_res] [snd_res]
				and cfg = new_cfg (ASM_FLET res, fst_res, cast_res) res::(constrain flags) [fst_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		|	INT, FLOAT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, fst_res) [cast_res] [fst_res]
				and cfg = new_cfg (ASM_FLET res, cast_res, snd_res) res::(constrain flags)  [fst_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		end
|	OpBi (EQ, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc flags in
		match get_ast_type ast1, get_ast_type ast2 with
			INT, INT -> begin
				let cfg = new_cfg (ASM_EQ res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, FLOAT -> begin
				let cfg = new_cfg (ASM_FEQ res, fst_res, snd_res) res::(constrain flags)  [fst_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, INT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, snd_res) [cast_res] [snd_res]
				and cfg = new_cfg (ASM_FEQ res, fst_res, cast_res) res::(constrain flags)  [fst_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		|	INT, FLOAT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, fst_res) [cast_res] [fst_res]
				and cfg = new_cfg (ASM_FEQ res, cast_res, snd_res) res::(constrain flags) [fst_res; snd_res] in
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		end
|	OpBi (NEQ, ast1, ast2, _) -> begin
		let fst_cfg, fst_end, fst_res = selection ast1
		and snd_cfg, snd_end, snd_res = selection ast2
		and res = new_memloc flags in
		match get_ast_type ast1, get_ast_type ast2 with
			INT, INT -> begin
				let cfg = new_cfg (ASM_NEQ res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, FLOAT -> begin
				let cfg = new_cfg (ASM_FNEQ res, fst_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cfg;
				fst_cfg, [cfg.next], res
				end
		|	FLOAT, INT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, snd_res) [cast_res] [snd_res]
				and cfg = new_cfg (ASM_FNEQ res, fst_res, cast_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		|	INT, FLOAT -> begin
				let cast_res = new_memloc [FPU] in
				let cast_cfg = new_cfg (ASM_FILD cast_res, fst_res) [cast_res] [fst_res]
				and cfg = new_cfg (ASM_FNEQ res, cast_res, snd_res) res::(constrain flags) [fst_res; snd_res] in (* idem *)
				join_links fst_end snd_cfg;
				join_links snd_end cast_cfg;
				join_links [cast_cfg.next] cfg;
				fst_cfg, [cfg.next], res
				end
		end

|	FuncValue (name, params, _)

|	FuncValueCompletion (name, params, _)

|	ValueCall (name, params, _)

|	FuncCall (name, params, t) -> begin
		let cfg, cfg_end =
			let call_cfg = new_cfg (ASM_CALL name) (constrain call_constrs) [] in
			if params != [] then
				let lst = List.map selection params in
				let param_cfg = List.fold_left (function (fst_cfg, fst_end, _), (snd_cfg,  snd_end, _) ->
					join_links fst_end snd_cfg;
					fst_cfg, snd_end, no_res
				) (hd lst) (tl lst)
				and params_loc = List.map (function _, _, loc -> loc) lst in
				let set_params_cfg = new_cfg (MEM_SET_CALL_DATA params_loc) (constrain [RSP]) params_loc in
				join_links [params_cfg.next] call_cfg;
				param_cfg, [call_cfg.next]
			else
				call_cfg, [call_cfg.next]
		in
		if t != UNIT then
			let ret_loc = new_memloc regs in
			let ret_cfg = new_cfg (MEM_GET_RET_DATA ret_loc) [ret_loc] [] in
			join_links cfg_end ret_cfg;
			cfg, [ret_cfg.next], ret_loc
		else
			cfg, cfg_end, no_res
|	IfElse (cond, if_true, if_false, _) ->
		let cond_cfg, cond_end, boolean = selection cond
		and true_cfg, true_end, true_res = selection if_true
		and false_cfg, false_end, false_res = selection if_false in
		let mov_cfg = new_cfg (ASM_MOV true_res, false_res) [true_res] [false_res]
		and if_cfg = new_cfg (ASM_JPIF boolean) [] [boolean] in
		join_links cond_end if_cfg;
		join_links [if_cfg.next] true_cfg;
		join_links [if_cfg.alternate] false_cfg;
		join_links false_end mov_cfg;
		cond_cfg, mov_cfg.next::true_end, true_res
|	While (cond, body, _) ->
		let cond_cfg, cond_end, boolean = selection cond
		and body_cfg, body_end, body_res = selection body in
		let if_cfg = new_cfg (ASM_JPNIF boolean) [] [boolean] in
		join_links cond_end if_cfg;
		join_links [if_cfg.next] body_cfg;
		join_links body_end cond_cfg;
		cond_cfg, [if_cfg.alternate], no_res
|	Block (asts, _) -> begin
		match asts with
			[] ->
				let nop_cfg = new_cfg ASM_NOP [] [] in
				nop_cfg, [nop_cfg.next], no_res
		|	_ ->
				create_local_env ();
				let lst = List.map selection asts in
				exit_local_env ();
				List.fold_left (function (fst_cfg, fst_end, _), (snd_cfg, snd_end, res) ->
					join_links fst_end snd_cfg;
					fst_cfg, snd_end, res
				) (List.hd lst) (List.tl lst)
		end
|	Field (name, _) ->
		let nop_cfg = new_cfg ASM_NOP [] [] in
		nop_cfg, [nop_cfg.next], variable_location name
|	Let (name, _, params, def, _) -> begin
		match params with
			[] -> (* ce n'est pas une fonction, mais une valeur (fonctionnelle ou non) *)
				let loc =
					try variable_location name
					with Not_found -> create_variable name
				in
				let def_cfg, def_end, def_res = selection def in
				let assign_cfg = new_cfg (ASM_MOV loc, def_res) [loc] [def_res] in
				join_links def_end assign_cfg;
				def_cfg, [assign_cfg.next], no_res (* il n'y a aucun résultat à un assignement, car il est de type unit *)
		|	l -> (* c'est une fonction, on calcule son code et on le place dans l'output *)
				create_local_env ();
				let params_loc = List.map create_variable params; (* ajout des variables comme paramètres *)
				let def_cfg, def_end, def_res = selection def in
				exit_local_env ();
				let get_params_cfg = new_cfg (MEM_GET_CALL_DATA params_loc) params_loc []
				and set_ret_data_cfg = new_cfg (MEM_SET_RET_DATA def_res) (constrain [RAX]) [def_res]
				and ret_cfg = new_cfg ASM_RET [] [] in
				join_links [get_params_cfg.next] def_cfg;
				join_links def_end set_ret_data_cfg;
				join_links [set_ret_data_cfg.next] ret_cfg;
				(* on ajoute le cfg à l'output *)
				output := (get_params_cfg, ret_cfg)::(!output);
				(* on retourne un cfg n'ayant aucuen influence sur le code *)
				let nop_cfg = new_cfg ASM_NOP [] [] in
				nop_cfg, [nop_cfg.next], no_res
		end
;;

(*

let v = [| 0; 1; 2; 3 |];;
let do_smth () =
	let i = ref 0 in
	while !i < 3 && v.(!i) = 0 do
		incr i
	done;
	v.(!i) <- 0
;;

let linfunc a b =
	let apply x = a + x * b in
	do_smth ();
	apply
;;
v;;
let id = linfunc 0 1   (* affecte v *);;
id 3  (* affecte pas v *);;

let res = linfunc 0 1 3  (* affecte v *);;

*)



(**)