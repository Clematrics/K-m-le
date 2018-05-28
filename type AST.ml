type opun = NEG | NOT;;
type opbi = ADD | SUB | MUL | DIV | MOD | AND | OR | XOR | LT | LET | EQ | NEQ;;

type const = Integer of int64 | Float of float | Bool of bool | Char of char | Pair_const of const list | Array_const of const vect | Sum_const of string * const;;

(* decorateur de type, array pas obligatoire techniquement car un sum-type, nan ? *)
type value_type = UNIT | INT | FLOAT | BOOL | CHAR | Pair of value_type list | Array of value_type | Abstract of int | Sum of (string * value_type) list | Func of value_type * value_type ;;

type ast =
|	Const of const * value_type
|   Construct of value_type * ast list

|	OpUn of opun * ast * value_type
|	OpBi of opbi * ast * ast * value_type
|	FuncCall of string * (ast list) * value_type

|	IfElse of ast * ast * ast
|   While of ast * ast

|	Block of ast list * value_type (* le 'in' créerai un nouveau bloc où le let est la première instruction du bloc *)
|	Field of string * value_type
|	Let of string * bool * (( string * value_type ) list) * ast (* le booléen définit si le type est récursif *)
;;

let get_ast_type = function
    Const _, t -> t
|   OpUn _, _, t -> t
|   OpBi _, _, _, t -> t
|   FuncCall _, _, t -> t
|   If (* TODO *)
|   While (* TODO *)
|   Block l -> get_ast_type (get_last l)
|   Field _, t -> t
|   Let _ -> UNIT
;;