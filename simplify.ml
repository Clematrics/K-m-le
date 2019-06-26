let apply_unary_op op const = match op with
	NEG ->
		match const with ->
			Integer i -> neg i, INT
		|	Float f -> -. f, FLOAT
|	NOT ->
		match const with
			Bool b -> not b, BOOL
;;

let apply_binary_op op const1 const2 = match const1, const2 with
	Integer i, Float f | Float f, Integer i ->
		apply_binary_op op (Float f) (Float (float_of_int i))
|	Integer i1, Integer i2 -> match op with
		ADD -> Int64.add i1 i2
	|	SUB -> Int64.sub i1 i2
	|	MUL -> Int64.mul i1 i2
	|	DIV -> Int64.div i1 i2
	|	MOD -> Int64.rem i1 i2
	|	AND -> Int64.logand i1 i2
	|	OR -> Int64.logor i1 i2
| 	Float f1, Float f2 -> match op with
		ADD -> f1 +. f2
	|	SUB -> f1 -. f2
	|	MUL -> f1 *. f2
	|	DIV -> f1 /. f2
	|	MOD -> mod_float f1 f2
|	Bool b1, Bool b2 -> match op with
		AND -> b1 && b2
	|	OR -> b1 || b2
	|	XOR -> (b1 && not b2) || (b2 && not b1)
;;

(* Reste à simplifier la tail recursion *)
let rec simplify_ast = function
	Const c -> Const c
|	Construct (t, asts) -> Construct (t, simplify_asts asts)
|	OpUn (o, ast, t) -> begin
		let s_ast = simplify_ast ast in
		match s_ast with
			Const (c, _) -> Const (apply_unary_op o c)
		|	_ -> OpUn (o, s_ast, t)
	end
|	OpBi (o, ast1, ast2, t) -> begin
		let s_ast1 = simplify_ast ast1
		and s_ast2 = simplify_ast ast2 in
		match s_ast1, s_ast2 with
			Const (c1, _) , Const (c2, _) -> Const ((apply_binary_op o c1 c2), t)
		|	_, _ -> OpBi (o, s_ast1, s_ast2, t)
	end
|	FuncCall (s, ast, t) ->
		FuncCall (s, simplify_asts asts, t)
|	IfElse (cond, if_true, if_false, t) -> begin
		let new_cond in simplify_ast cond in
		match new_cond with
			Const (true, BOOL) -> simplify_ast if_true
		|	Const (false, BOOL) -> simplify_ast if_false
		|	ast -> IfElse (ast, simplify_ast if_true, simplify_ast if_false, t)
	end
|	While (cond, body) -> While (simplify_ast cond, simplify_ast body)
|	Block (b, t) -> begin
		let b_simple = simplify_asts b in
		match b_simple with
			[a] -> a
		|	instrs -> Block (simplify_instructions instrs, t)
	end
|	Let (s, false, l, ast, t) -> Let (s, l, simplify_ast ast, t)
|	Let (s, true, l, ast, t) -> Let (optimize_tail_recursion (s, l, simplify_ast ast, t))

and simplify_asts = function
	[] -> []
|	h::t ->
		let ast = simplify_ast h in
		ast::(simplify_asts t)
;;

(* doit convertir les parametres des appels de fonction en changement de variables *)
let rec optimize_tail_recursion name params = function
	Const c -> Const c

;;