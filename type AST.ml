type opun = NEG | NOT;;
type opbi = ADD | SUB | MUL | DIV | MOD | AND | OR | XOR | LT | LET | EQ | NEQ;;

type const = Integer of int64 | Float of float | Bool of bool | Char of char;;

(* decorateur de type, array pas obligatoire techniquement car un sum-type, nan ? *)
type value_type = UNIT | INT | FLOAT | BOOL | CHAR | Tuple of value_type list | Array of value_type | Abstract of int | Sum of (string * value_type) list | Func of value_type * value_type | FuncValue of value_type * value_type;;

type pattern =
	ConstPattern of const
|	TuplePattern of pattern list
|	ArrayPattern of pattern list
|	SumPattern of string * pattern list
|	VarPattern of string
|	AnyPattern
;;

type ast =
	Nothing (* équivalent au () de OCaml *)
|	Error (* en cas de division par zéro, accès à un élément d'un tableau out-of-bound, erreur de matching ... *)
|	Const of const * value_type
|	TupleElement of ast list * value_type
|	ArrayElement of ast list * value_type
|	SumElement of string * ast * value_type
|	FuncElement of string * (ast list) * value_type

|	Match of ast * (pattern * ast list) * value_type (* sert pour le pattern matching *)

|	ArrayAccess of ast * ast * value_type (* tableau à accéder * expression de l'index * type retourné *)
|	OpUn of opun * ast * value_type
|	OpBi of opbi * ast * ast * value_type
|	IncompleteFuncCall of string * (ast list) * value_type
|	PureFuncCall of string * (ast list) * value_type

|	IfElse of ast * ast * ast * value_type
|	While of ast * ast

|	Block of ast list * value_type (* le 'in' créerai un nouveau bloc où le let est la première instruction du bloc *)
|	Field of string * value_type
|	Let of string * bool * (( string (* * value_type *) ) list) * ast * value_type (* le booléen définit si le type est récursif, value_type; type de la variable définie *)
;;

let get_ast_type = function
	Nothing -> UNIT
|	Error -> UNIT
|	Const _, t -> t
|	TupleElement _, t -> t
|	ArrayElement _, t -> t
|	SumElement _, _, t -> t
|	Match _, _, t -> t
|	ArrayAccess _, _, t -> t
|	OpUn _, _, t -> t
|	OpBi _, _, _, t -> t
|	FuncCall _, _, t -> t
|	IfElse _, _, t -> t
|	While _, _ -> UNIT
|	Block _, t -> t
|	Field _, t -> t
|	Let _ -> UNIT
;;