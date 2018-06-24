(* Les fonctions suivantes servent à reconnaître des types de caractère (sur le modèle du C++) *)

let iswhite c = (* Reconnaît les "blancs" *)
  (c = ' ' || c = '\n' || c = '\t');;

let isletter c = (* Reconnaît les lettres, majuscules et minuscules *)
  let n = int_of_char c in 
  ((65 <= n && n <= 90)
   || (97 <= n && n <= 122));;

let isnumber c = (* Reconnait les chiffres *)
  let n = int_of_char c in 
  (48 <= n && n <= 57);;

let isalphanum c = (* Reconnaît les caractères alphanumériques *)
  (isnumber c || isletter c);;
  
let valid_id c = (* Reconnaît les caractères valides pour un identificateur : les alphanumériques et l'underscore *)
  let n = int_of_char c in 
  (isalphanum c || n = 95);;

let table_symboles = ['.';'&';'+';'=';'-';'*';'/';'\"';'%';'|';'<';'>';'_' ; ';'];;

let issym c =
  List.mem c table_symboles;;

(* Je recode do_list *)

let rec do_list f = function
  |[] -> ()
  |t::q -> f t; do_list f q;;
