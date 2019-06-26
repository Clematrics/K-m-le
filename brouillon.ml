let rec est_terminale = function
    1 -> 2 (* constante *)
|   2 -> est_terminale 1 (* appel à la fonction *)
|   n -> if n mod 2 = 0 then
            est_terminale (n // 2) (* termine par un appel à la fonction *)
        else
            do_something ();
            est_terminale (n - 1) (* termine par un appel à la fonction *)
;;

let rec non_terminale = function
    1 -> []
|   n -> n::(non_terminale (n//2))
;;
(* non terminal car la dernière instruction n'est pas un appel seul à la fonction*)
(* étude du CFG ? *)


let x = begin match x with 1 -> 0 | n -> 1
end
;;

let curry f x y =
    f (x, y)
;;

let uncurry f (x, y) =
    f x y
;;

let muladd a b c = a * b + c;; (* fonction à 3 paramètres *)
let sum = muladd 1;; (* valeur fonctionnelle *)
let inc = do_smth (); sum 1;; (* exécute qqch lors assignement, mais valeur fonctionnelle *)
let inc x = do_smth (); sum 1 x;; (* exécute qqch en plus lors appel, pas une valeur fonctionnelle *)
let sub x = sum x (-1);; (* valeur fonctionnelle *)

let x = sum 3 4;;
let y = inc 4;;
let z = sub y;;
let alias = muladd;; (* valeur fonctionnelle *)
let alias_bis = inc;; (* valeur fonctionnelle *)


(*
sum x y
^^^
valeur fonctionnelle ? -> complète truc puis remplissage objet d'appel puis (éventuelle) exécution
sum peut produire qqch à côté


- création objet -> si fonction appelée directement existe et enregistrée
- remplissage objet avec paramètres existants
- appel une fois l'objet complet

- description complète d'une fonction : appel
- description incomplète (création objet)
-> alias et alias_bis sont des objets
-> map muladd [ 0 ; 1 ; 2 ; 3 ];; (on sait pas si objet complet ou incomplet)

let rec map f = function
    Nil -> Nil
|   Node(h, t) -> Node(f h, map f t)
;;

- toujours fonction en paramètre sont des objets, et test dynamique pour savoir si complets ?
*)