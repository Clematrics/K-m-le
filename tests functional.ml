let v = [| 1 ; 1 ; 2 ; 3 |];;

let do_smth () =
	try
		for i = 0 to 3 do
			if v.(i) != 0 then begin
				v.(i) <- 0;
				raise Exit
			end
		done
	with
		Exit -> ()
;;

let muladd a b c = a * b + c;;
let sum = muladd 1;;
let inc x = do_smth (); sum 1 x;;
let sub x = do_smth (); sum x (-1);;

let x = sum 3 4;;
let y = inc 4;;
let z = sub y;;
let alias = muladd;;
let alias_bis = inc;;

let arr = List.map muladd [ 0 ; 1 ; 2 ; 3 ];;
inc 67;;
v;;
w;;
let w = v;;
v.(3) <- 0;;

let rec map f = function
	[] -> []
|	h::t -> (f h)::(map f t)
;;