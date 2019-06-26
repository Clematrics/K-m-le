(*
#load "dynlink.cma";;
#load "camlp4o.cma";;
et
#use "topfind";;
#camlp4r;;
ne fonctionne pas dans le top-level avec la commande "ocaml test.ml" :(
*)
open Array

open Int64

open Map

open Stream

open String

module Dict = Map.Make(String)

let to_str c = String.make 1 c

let str_tl str = let len = (String.length str) - 1 in String.sub str 1 len

let rec is_in elem =
  function | [] -> false | h :: t -> (h = elem) || (is_in elem t)

let hd = function | [] -> raise (Invalid_argument "Empty list") | h :: t -> h

let tl = function | [] -> raise (Invalid_argument "Empty list") | h :: t -> t

let rec any_except c stream =
  if (Stream.next stream) <> c then any_except c stream else ()

let rec spaces (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some (' ' | '\t' | '\n' | '\r' | '\012') ->
      (Stream.junk __strm;
       let _ =
         (try spaces __strm with | Stream.Failure -> raise (Stream.Error ""))
       in ())
  | _ -> ()

let is_symbol =
  function
  | ':' | ',' | '(' | ')' | '+' | '-' | '*' | '/' | '%' | '^' | '&' | '|' |
      '~' | '@' | '>' | '<' | '[' | ';' | ']' -> true
  | _ -> false

let is_str_symbol str = is_symbol str.[0]

let kick_comments stream =
  let rec kick stream =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some '/' ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some '/' ->
                (Stream.junk __strm;
                 let _ =
                   (try any_except '\n' __strm
                    with | Stream.Failure -> raise (Stream.Error ""))
                 in
                   (try kick __strm
                    with | Stream.Failure -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | Some c ->
          (Stream.junk __strm;
           let next =
             (try kick __strm
              with | Stream.Failure -> raise (Stream.Error ""))
           in Stream.icons c next)
      | _ -> Stream.sempty
  in (spaces stream; kick stream)

let str_catcher stream =
  let rec aux str stream =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some '\\' ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some c ->
                (Stream.junk __strm; aux (str ^ ("\\" ^ (to_str c))) stream)
            | _ -> raise (Stream.Error "")))
      | Some c when c <> '"' ->
          (Stream.junk __strm; aux (str ^ (to_str c)) stream)
      | _ -> str
  in aux "" stream

let split stream =
  let rec aux str (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some (' ' | '\t' | '\n' | '\r' | '\012') ->
        (Stream.junk __strm;
         let next =
           (try aux "" __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in
           if str <> ""
           then Stream.lcons (fun _ -> String.lowercase_ascii str) next
           else next)
    | Some '"' ->
        (Stream.junk __strm;
         let str_content =
           (try str_catcher __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in
           (match Stream.peek __strm with
            | Some '"' ->
                (Stream.junk __strm;
                 let next =
                   (try aux "" __strm
                    with | Stream.Failure -> raise (Stream.Error ""))
                 in
                   Stream.icons "\""
                     (Stream.icons str_content (Stream.icons "\"" next)))
            | _ -> raise (Stream.Error "")))
    | Some c when is_symbol c ->
        (Stream.junk __strm;
         let next =
           (try aux "" __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in
           Stream.lcons (fun _ -> String.lowercase_ascii str)
             (Stream.lcons (fun _ -> to_str c) next))
    | Some c ->
        (Stream.junk __strm;
         (try aux (str ^ (to_str c)) __strm
          with | Stream.Failure -> raise (Stream.Error "")))
    | _ -> Stream.sempty
  in aux "" stream

let get_directive stream =
  let rec get_args last (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some ")" -> (Stream.junk __strm; [])
    | Some "," ->
        (Stream.junk __strm;
         let args =
           (try get_args "" __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in
           if last = ""
           then failwith "UN ARGUMENT EST DECLARE SANS NOM"
           else
             (* TODO : indiquer où / créer exception spécifique au problème *)
             if is_in last args
             then
               failwith "UN ARGUMENT EST DECLARE DEUX FOIS AVEC LE MEME NOM"
             else (* TODO : idem *) last :: args)
    | Some arg ->
        (Stream.junk __strm;
         let args =
           (try get_args arg __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in
           if last <> ""
           then
             failwith
               "DEUX ARGUMENTS SONT DECLARES COTE A COTE SANS SEPARATEUR"
           else (* TODO : exception spécifique + emplacement *) args)
    | _ -> failwith "FIN DE FICHIER ATTEINT, DIRECTIVE NON RESOLUE" in
  (* TODO : idem *)
  let check_args (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some "(" ->
        (Stream.junk __strm;
         (try get_args "" __strm
          with | Stream.Failure -> raise (Stream.Error "")))
    | _ -> [] in
  let rec get_expr (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some "#end" -> (Stream.junk __strm; Stream.sempty)
    | Some elem ->
        (Stream.junk __strm;
         let expr =
           (try get_expr __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons elem expr)
    | _ -> failwith "FIN DE FICHIER ATTEINT, DIRECTIVE NON RESOLUE" in
  (* TODO : idem *)
  let name =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some ("#end" | "(" | ")") ->
          (Stream.junk __strm;
           failwith "DIRECTIVE INCOMPLETE OU NOM INVALIDE")
      | (* TODO classique des exc *) Some str -> (Stream.junk __strm; str)
      | _ -> failwith "FIN DE FICHIER ATTEINT, DIRECTIVE NON RESOLUE" in
  (* TODO : idem *)
  let args = check_args stream in (name, args, (get_expr stream))

let label_counter = ref 0

let next_label () = (incr label_counter; !label_counter)

let apply_directive dir stream =
  let (name, args, expr) = dir in
  let dict = ref Dict.empty in
  let rec presolve_labels (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some str when
        (str.[0] = '#') &&
          (((String.length str) > 0) && ((str_tl str) <> "use"))
        ->
        (Stream.junk __strm;
         let next =
           (try presolve_labels __strm
            with | Stream.Failure -> raise (Stream.Error "")) in
         let label = str_tl str in
         let value =
           (try Dict.find label !dict
            with
            | Not_found ->
                (dict := Dict.add label (next_label ()) !dict;
                 !label_counter))
         in Stream.lcons (fun _ -> "__reserved" ^ (string_of_int value)) next)
    | Some str ->
        (Stream.junk __strm;
         let next =
           (try presolve_labels __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons str next)
    | _ -> Stream.sempty in
  let rec solve_args dict_ (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some str ->
        (Stream.junk __strm;
         let next =
           (try solve_args dict_ __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in
           (match Dict.find_opt str dict_ with
            | Some res -> Stream.icons res next
            | None -> Stream.icons str next))
    | _ -> Stream.sempty in
  let rec apply stream_ =
    let (__strm : _ Stream.t) = stream_
    in
      match Stream.peek __strm with
      | Some str when str = name ->
          (Stream.junk __strm;
           if args = []
           then
             (let (__strm : _ Stream.t) = stream_
              in
                match Stream.peek __strm with
                | Some "(" ->
                    (Stream.junk __strm;
                     let (__strm : _ Stream.t) = stream_
                     in
                       (match Stream.peek __strm with
                        | Some ")" ->
                            (Stream.junk __strm;
                             dict := Dict.empty;
                             Stream.lapp (fun _ -> presolve_labels expr)
                               (Stream.slazy (fun _ -> apply stream_)))
                        | _ ->
                            Stream.icons "("
                              (Stream.slazy (fun _ -> apply stream_))))
                | _ ->
                    (dict := Dict.empty;
                     Stream.lapp (fun _ -> presolve_labels expr)
                       (Stream.slazy (fun _ -> apply stream_))))
           else
             (let (__strm : _ Stream.t) = stream_
              in
                match Stream.peek __strm with
                | Some "(" ->
                    (Stream.junk __strm;
                     let args_dict = ref Dict.empty in
                     let rec
                       presolve_args args_ prestream (__strm : _ Stream.t) =
                       (match Stream.peek __strm with
                        | Some ")" when args_ = [] ->
                            (Stream.junk __strm;
                             Stream.lapp
                               (fun _ -> solve_args !args_dict expr)
                               (Stream.slazy (fun _ -> apply stream_)))
                        | Some ")" when args_ <> [] ->
                            (Stream.junk __strm;
                             Stream.iapp prestream
                               (Stream.icons ")"
                                  (Stream.slazy (fun _ -> apply stream_))))
                        | Some "," when args_ = [] ->
                            (Stream.junk __strm;
                             Stream.iapp prestream
                               (Stream.icons ")"
                                  (Stream.slazy (fun _ -> apply stream_))))
                        | Some "," ->
                            (Stream.junk __strm;
                             (try
                                presolve_args (tl args_)
                                  (Stream.iapp prestream (Stream.ising ","))
                                  __strm
                              with
                              | Stream.Failure -> raise (Stream.Error "")))
                        | Some str ->
                            (Stream.junk __strm;
                             let next =
                               (try
                                  presolve_args (tl args_)
                                    (Stream.iapp prestream (Stream.ising str))
                                    __strm
                                with
                                | Stream.Failure -> raise (Stream.Error ""))
                             in
                               (args_dict :=
                                  Dict.add (hd args_) str !args_dict;
                                next))
                        | _ ->
                            failwith
                              "ERREUR FIN DE FICHIER ATTEINT, APPEL D'UNE DIRECTIVE NON COMPLETE")
                     in
                       (* TODO : idem *)
                       presolve_args args Stream.sempty stream_)
                | _ -> apply stream_))
      | Some str ->
          (Stream.junk __strm;
           let next =
             (try apply __strm
              with | Stream.Failure -> raise (Stream.Error ""))
           in Stream.icons str next)
      | _ -> raise Stream.Failure
  in apply stream

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
let f2i = Int64.bits_of_float

let i2f = Int64.float_of_bits

let ( <^ ) a b = (Int64.compare a b) < 0

let ( =^ ) a b = (Int64.compare a b) = 0

let add_i_f i f = (Int64.to_float i) +. f

let add_f_i f i = add_i_f i f

let sub_i_f i f = (Int64.to_float i) -. f

let sub_f_i f i = -. (sub_i_f i f)

let mul_i_f i f = (Int64.to_float i) *. f

let mul_f_i f i = mul_i_f i f

let div_i_f i f = (Int64.to_float i) /. f

let div_f_i f i = f /. (Int64.to_float i)

let pow_i_f i f = (Int64.to_float i) ** f

let pow_f_i f i = f ** (Int64.to_float i)

let rec int64_pow x =
  function
  | k when k <^ Int64.zero -> 0L
  | (* div_f_i 1. (Int64_pow x (Int64.neg k)) *) 0L -> 1L
  | 1L -> x
  | k when (Int64.rem k 2L) =^ 0L ->
      int64_pow (Int64.mul x x) (Int64.shift_right_logical k 1)
  | k -> Int64.mul x (int64_pow x (Int64.pred k))

type expr_type = | Int_ | Float_

type ast_Op =
  | And
  | Or
  | Xor
  | Not
  | Shl
  | Shr
  | Add
  | Sub
  | Mul
  | Div
  | Pow
  | Abs
  | Neg

type ast =
  | BiOp of expr_type * ast * ast_Op * ast
  | UnOp of expr_type * ast_Op * ast
  | Int of int64
  | Float of float
  | Epsilon

let get_ast_type =
  function
  | BiOp (t, _, _, _) -> t
  | UnOp (t, _, _) -> t
  | Int _ -> Int_
  | Float _ -> Float_

let rec evaluate =
  function
  | Int i -> i
  | Float f -> f2i f
  | UnOp (t, op, tree) ->
      (match (op, t) with
       | (Not, _) -> Int64.lognot (evaluate tree)
       | (Abs, Int_) -> Int64.abs (evaluate tree)
       | (Abs, Float_) -> f2i (abs_float (i2f (evaluate tree)))
       | (Neg, Int_) -> Int64.neg (evaluate tree)
       | (Neg, Float_) -> f2i (-. (i2f (evaluate tree)))
       | (_, _) -> failwith "opérateur non défini pour le type suivant")
  | (* TODO : exc *) BiOp (_, l, op, r) ->
      (match (op, (get_ast_type l), (get_ast_type r)) with
       | (And, Int_, Int_) -> Int64.logand (evaluate l) (evaluate r)
       | (Or, Int_, Int_) -> Int64.logor (evaluate l) (evaluate r)
       | (Xor, Int_, Int_) -> Int64.logxor (evaluate l) (evaluate r)
       | (Shl, Int_, Int_) ->
           Int64.shift_left (evaluate l) (Int64.to_int (evaluate r))
       | (Shr, Int_, Int_) ->
           Int64.shift_right (evaluate l) (Int64.to_int (evaluate r))
       | (Add, Int_, Int_) -> Int64.add (evaluate l) (evaluate r)
       | (Add, Int_, Float_) -> f2i (add_i_f (evaluate l) (i2f (evaluate r)))
       | (Add, Float_, Int_) -> f2i (add_f_i (i2f (evaluate l)) (evaluate r))
       | (Add, Float_, Float_) ->
           f2i ((i2f (evaluate l)) +. (i2f (evaluate r)))
       | (Sub, Int_, Int_) -> Int64.sub (evaluate l) (evaluate r)
       | (Sub, Int_, Float_) -> f2i (sub_i_f (evaluate l) (i2f (evaluate r)))
       | (Sub, Float_, Int_) -> f2i (sub_f_i (i2f (evaluate l)) (evaluate r))
       | (Sub, Float_, Float_) ->
           f2i ((i2f (evaluate l)) -. (i2f (evaluate r)))
       | (Mul, Int_, Int_) -> Int64.mul (evaluate l) (evaluate r)
       | (Mul, Int_, Float_) -> f2i (mul_i_f (evaluate l) (i2f (evaluate r)))
       | (Mul, Float_, Int_) -> f2i (mul_f_i (i2f (evaluate l)) (evaluate r))
       | (Mul, Float_, Float_) ->
           f2i ((i2f (evaluate l)) *. (i2f (evaluate r)))
       | (Div, Int_, Int_) -> Int64.div (evaluate l) (evaluate r)
       | (Div, Int_, Float_) -> f2i (div_i_f (evaluate l) (i2f (evaluate r)))
       | (Div, Float_, Int_) -> f2i (div_f_i (i2f (evaluate l)) (evaluate r))
       | (Div, Float_, Float_) ->
           f2i ((i2f (evaluate l)) /. (i2f (evaluate r)))
       | (Pow, Int_, Int_) -> int64_pow (evaluate l) (evaluate r)
       | (Pow, Int_, Float_) -> f2i (pow_i_f (evaluate l) (i2f (evaluate r)))
       | (Pow, Float_, Int_) -> f2i (pow_f_i (i2f (evaluate l)) (evaluate r))
       | (Pow, Float_, Float_) ->
           f2i ((i2f (evaluate l)) ** (i2f (evaluate r)))
       | (_, _, _) ->
           failwith "opérateur non disponible avec les types suivants")

(* TODO : exc  *)
let is_number_type = ref Int_

let is_number_int = ref Int64.zero

let is_number_float = ref 0.

let is_number str =
  try (is_number_int := Int64.of_string str; is_number_type := Int_; true)
  with
  | Failure _ ->
      (try
         (is_number_float := float_of_string str;
          is_number_type := Float_;
          true)
       with | Failure _ -> false)

let to_hex (int_ : int64) = Printf.sprintf "%Lx" int_

let assemble_trees l_tree op r_tree =
  if r_tree = Epsilon
  then l_tree
  else
    (let (t, u) = ((get_ast_type l_tree), (get_ast_type r_tree)) in
     let v = if (t = Float_) || (u = Float_) then Float_ else Int_
     in BiOp (v, l_tree, op, r_tree))

let try_parse_expr stream =
  let rec rule stream =
    let l_tree = expr stream
    and (Some op, r_tree) = rule_tail stream
    in assemble_trees l_tree op r_tree
  and rule_tail (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some "&" ->
        (Stream.junk __strm;
         let tree =
           (try rule __strm with | Stream.Failure -> raise (Stream.Error ""))
         in ((Some And), tree))
    | Some "|" ->
        (Stream.junk __strm;
         let tree =
           (try rule __strm with | Stream.Failure -> raise (Stream.Error ""))
         in ((Some Or), tree))
    | Some "@" ->
        (Stream.junk __strm;
         let tree =
           (try rule __strm with | Stream.Failure -> raise (Stream.Error ""))
         in ((Some Xor), tree))
    | Some "<" ->
        (Stream.junk __strm;
         (match Stream.peek __strm with
          | Some "<" ->
              (Stream.junk __strm;
               let tree =
                 (try rule __strm
                  with | Stream.Failure -> raise (Stream.Error ""))
               in ((Some Shl), tree))
          | _ -> raise (Stream.Error "")))
    | Some ">" ->
        (Stream.junk __strm;
         (match Stream.peek __strm with
          | Some ">" ->
              (Stream.junk __strm;
               let tree =
                 (try rule __strm
                  with | Stream.Failure -> raise (Stream.Error ""))
               in ((Some Shr), tree))
          | _ -> raise (Stream.Error "")))
    | _ -> (None, Epsilon)
  and expr stream =
    let l_tree = term stream
    and (Some op, r_tree) = expr_tail stream
    in assemble_trees l_tree op r_tree
  and expr_tail (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some "+" ->
        (Stream.junk __strm;
         let tree =
           (try expr __strm with | Stream.Failure -> raise (Stream.Error ""))
         in ((Some Add), tree))
    | Some "-" ->
        (Stream.junk __strm;
         let tree =
           (try expr __strm with | Stream.Failure -> raise (Stream.Error ""))
         in ((Some Sub), tree))
    | _ -> (None, Epsilon)
  and term stream =
    let l_tree = factor stream
    and (Some op, r_tree) = term_tail stream
    in assemble_trees l_tree op r_tree
  and term_tail (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some "*" ->
        (Stream.junk __strm;
         let tree =
           (try term __strm with | Stream.Failure -> raise (Stream.Error ""))
         in ((Some Mul), tree))
    | Some "/" ->
        (Stream.junk __strm;
         let tree =
           (try term __strm with | Stream.Failure -> raise (Stream.Error ""))
         in ((Some Div), tree))
    | _ -> (None, Epsilon)
  and factor stream =
    let l_tree = atom stream
    and (Some op, r_tree) = factor_tail stream
    in assemble_trees l_tree op r_tree
  and factor_tail (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some "^" ->
        (Stream.junk __strm;
         let tree =
           (try factor __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in ((Some Pow), tree))
    | _ -> (None, Epsilon)
  and atom (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some "(" ->
        (Stream.junk __strm;
         let tree =
           (try rule __strm with | Stream.Failure -> raise (Stream.Error ""))
         in
           (match Stream.peek __strm with
            | Some ")" -> (Stream.junk __strm; tree)
            | _ -> raise (Stream.Error "")))
    | Some "~" ->
        (Stream.junk __strm;
         let tree =
           (try atom __strm with | Stream.Failure -> raise (Stream.Error ""))
         in UnOp ((get_ast_type tree), Not, tree))
    | Some "+" ->
        (Stream.junk __strm;
         let tree =
           (try atom __strm with | Stream.Failure -> raise (Stream.Error ""))
         in UnOp ((get_ast_type tree), Abs, tree))
    | Some "-" ->
        (Stream.junk __strm;
         let tree =
           (try atom __strm with | Stream.Failure -> raise (Stream.Error ""))
         in UnOp ((get_ast_type tree), Neg, tree))
    | Some str when is_number str ->
        (Stream.junk __strm;
         (match !is_number_type with
          | Int_ -> Int !is_number_int
          | Float_ -> Float !is_number_float))
    | _ -> failwith "A NUMBER OR A LITERAL WAS EXPECTED HERE" in
  (* TODO : idem *)
  let tree = rule stream
  in Stream.lcons (fun _ -> to_hex (evaluate tree)) stream

let rec solve_expr stream =
  let rec aux (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some str when is_number str ->
        (Stream.junk __strm;
         let (__strm : _ Stream.t) = try_parse_expr (Stream.icons str stream)
         in
           (match Stream.peek __strm with
            | Some nmb ->
                (Stream.junk __strm;
                 let next =
                   (try solve_expr __strm
                    with | Stream.Failure -> raise (Stream.Error ""))
                 in Stream.icons nmb next)
            | _ -> raise Stream.Failure))
    | Some (("(" | ")" | "~" | "+" | "-" as str)) ->
        (Stream.junk __strm;
         let (__strm : _ Stream.t) = try_parse_expr (Stream.icons str stream)
         in
           (match Stream.peek __strm with
            | Some nmb ->
                (Stream.junk __strm;
                 let next =
                   (try solve_expr __strm
                    with | Stream.Failure -> raise (Stream.Error ""))
                 in Stream.icons nmb next)
            | _ -> raise Stream.Failure))
    | Some ("<" | ">" | "*" | "/" | "%" | "^" | "&" | "|" | "@") ->
        (Stream.junk __strm;
         failwith
           "EXPRESSION DETECTEE MAIS COMMENCEE PAR UN OPERATEUR INATTENDU")
    | (* TODO : idem *) Some str ->
        (Stream.junk __strm;
         let next =
           (try solve_expr __strm
            with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons str next)
    | _ -> Stream.sempty
  in aux stream

(* return a stream of words containing a valid assembler code, without any directive left. It also return the list of the 'use' directives declared in this file, including all 'use' directives imorted in this file *)
let rec solve_file file_path files_path =
  (if Filename.is_relative file_path
   then failwith "les chemins relatifs ne sont pas encore supportés"
   else ();
   (* TODO : exc *)
   let chn = open_in file_path in
   let clean_stream = kick_comments (Stream.of_channel chn) in
   let split_stream = split clean_stream in
   let def_directives = ref [] in
   let rec solve stream =
     let (__strm : _ Stream.t) = stream
     in
       match Stream.peek __strm with
       | Some "#private" ->
           (Stream.junk __strm;
            let directive =
              (try get_directive __strm
               with | Stream.Failure -> raise (Stream.Error ""))
            in solve (apply_directive directive stream))
       | Some "#def" ->
           (Stream.junk __strm;
            let directive =
              (try get_directive __strm
               with | Stream.Failure -> raise (Stream.Error ""))
            in
              (def_directives := directive :: !def_directives;
               solve (apply_directive directive stream)))
       | Some "#use" ->
           (Stream.junk __strm;
            (match Stream.peek __strm with
             | Some "\"" ->
                 (Stream.junk __strm;
                  (match Stream.peek __strm with
                   | Some str ->
                       (Stream.junk __strm;
                        (match Stream.peek __strm with
                         | Some "\"" ->
                             (Stream.junk __strm;
                              if is_in str files_path
                              then
                                failwith
                                  ("CYCLE CREE LORS DE L'UTILISATION DE #USE : "
                                     ^
                                     ((hd files_path) ^
                                        (" CALL " ^
                                           (str ^
                                              ", QUI EST DEJA EN COURS DE RESOLUTION"))))
                              else ();
                              (* TODO : idem *)
                              let (file_stream, imported_dir) =
                                solve_file str (file_path :: files_path)
                              in
                                (def_directives :=
                                   imported_dir @ !def_directives;
                                 let rec solve_imported stream_ =
                                   (function
                                    | [] -> stream_
                                    | dir :: l ->
                                        solve_imported
                                          (apply_directive dir stream_) l)
                                 in
                                   solve (solve_imported stream imported_dir)))
                         | _ -> raise (Stream.Error "")))
                   | _ -> raise (Stream.Error "")))
             | _ -> raise (Stream.Error "")))
       | Some str ->
           (Stream.junk __strm;
            let next =
              (try solve __strm
               with | Stream.Failure -> raise (Stream.Error ""))
            in Stream.icons str next)
       | _ -> Stream.sempty
   in ((solve_expr (solve split_stream)), (!def_directives)))

let data = ref Dict.empty

(* TODO ; possibilité de mettre des références de données dans des déclarations de données *)
let data_catcher stream =
  let rec get_data last stream =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some str when is_number str ->
          (Stream.junk __strm;
           if last <> None
           then failwith "données mal formatées"
           else (* TODO : idem *) get_data (Some !is_number_int) stream)
      | Some ";" ->
          (Stream.junk __strm;
           let data_ =
             (try get_data None __strm
              with | Stream.Failure -> raise (Stream.Error ""))
           in
             (match last with
              | Some int_ -> int_ :: data_
              | None -> failwith "données mal formatées"))
      | (* TODO : idem *) Some "]" -> (Stream.junk __strm; [])
      | _ -> failwith "erreur, donnée non reconnue" in
  (* TODO : idem *)
  let name =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some str when is_str_symbol str ->
          (Stream.junk __strm; failwith "SYMBOLE RESERVE")
      | Some str -> (Stream.junk __strm; str)
      | _ -> failwith "FIN DE FICHIER ATTEINT"
  in
    (* TODO *)
    try ignore (Dict.find name !data)
    with
    | Not_found ->
        (failwith "données déjà déclaréés";
         (* TODO *)
         data := Dict.add name (get_data None stream) !data)

let imm_res = ref 0L

let is_imm str =
  match Int64.of_string_opt str with
  | Some x -> (imm_res := x; true)
  | None -> false

let reg_res = ref 0L

let is_reg =
  function
  | "ext" -> (reg_res := 25L; true)
  | "sr" -> (reg_res := 27L; true)
  | "ir" -> (reg_res := 26L; true)
  | "er" -> (reg_res := 28L; true)
  | "edr" -> (reg_res := 29L; true)
  | str when str.[0] = 'r' ->
      (match int_of_string_opt (str_tl str) with
       | None -> false
       | Some x ->
           if x <= 29 then (reg_res := Int64.of_int x; true) else false)
  | _ -> false

type token =
  | Stop
  | Sys
  | Call
  | Ret
  | Jmp
  | Jz
  | Jnz
  | Lt
  | Let
  | Eq
  | Load
  | Store
  | Push
  | Pop
  | And
  | Or
  | Xor
  | Shl
  | Shr
  | Add
  | Sub
  | Mul
  | Div
  | Bcnt
  | Rev
  | Msb
  | Cvrt
  | Ftrc
  | Flt
  | Flet
  | Fadd
  | Fsub
  | Fmul
  | Fdiv
  | Imm of int64
  | Ptr of string
  | Reg of int64
  | Label of string

let rec lexer stream =
  let (__strm : _ Stream.t) = stream
  in
    match Stream.peek __strm with
    | Some "data" -> (Stream.junk __strm; data_catcher stream; lexer stream)
    | Some "stop" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Stop next)
    | Some "sys" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Sys next)
    | Some "call" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Call next)
    | Some "ret" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Ret next)
    | Some "jmp" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Jmp next)
    | Some "jz" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Jz next)
    | Some "jnz" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Jnz next)
    | Some "lt" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Lt next)
    | Some "let" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Let next)
    | Some "eq" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Eq next)
    | Some "load" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Load next)
    | Some "store" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Store next)
    | Some "push" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Push next)
    | Some "pop" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Pop next)
    | Some "and" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons And next)
    | Some "or" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Or next)
    | Some "xor" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Xor next)
    | Some "shl" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Shl next)
    | Some "shr" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Shr next)
    | Some "add" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Add next)
    | Some "sub" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Sub next)
    | Some "mul" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Mul next)
    | Some "div" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Div next)
    | Some "bcnt" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Bcnt next)
    | Some "rev" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Rev next)
    | Some "msb" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Msb next)
    | Some "cvrt" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Cvrt next)
    | Some "ftrc" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Ftrc next)
    | Some "flt" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Flt next)
    | Some "flet" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Flet next)
    | Some "fadd" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Fadd next)
    | Some "fsub" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Fsub next)
    | Some "fmul" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Fmul next)
    | Some "fdiv" ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.icons Fdiv next)
    | Some str when is_imm str ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.lcons (fun _ -> Imm !imm_res) next)
    | Some str when is_reg str ->
        (Stream.junk __strm;
         let next =
           (try lexer __strm with | Stream.Failure -> raise (Stream.Error ""))
         in Stream.lcons (fun _ -> Reg !reg_res) next)
    | Some str ->
        (Stream.junk __strm;
         let (__strm : _ Stream.t) = stream
         in
           (match Stream.peek __strm with
            | Some ":" ->
                (Stream.junk __strm;
                 Stream.icons (Label str)
                   (Stream.slazy (fun _ -> lexer stream)))
            | _ ->
                Stream.icons (Ptr str) (Stream.slazy (fun _ -> lexer stream))))
    | _ -> Stream.sempty

(* TODO : vérifier syntaxe fonctions Map *)
let wait_for_lbl = ref Dict.empty

let lbl = ref Dict.empty

let register_ptr ptr pos =
  try
    let (lst, p) = Dict.find ptr !lbl
    in lbl := Dict.add ptr ((pos :: lst), p) !lbl
  with
  | Not_found ->
      let lst_ = (try Dict.find ptr !wait_for_lbl with | Not_found -> [])
      in wait_for_lbl := Dict.add ptr (pos :: lst_) !wait_for_lbl

let register_label lbl_ pos =
  match Dict.find_opt lbl_ !lbl with
  | None ->
      let lst = (try Dict.find lbl_ !wait_for_lbl with | Not_found -> [])
      in
        (wait_for_lbl := Dict.remove lbl_ !wait_for_lbl;
         lbl := Dict.add lbl_ (lst, pos) !lbl)
  | Some _ -> failwith "label déjà déclaré"

(* TODO : exc *)
let make_instr opcode regs imms =
  let rec head_instr i =
    function
    | [] -> (*Int64.logor (Int64.shift_left debug 32)*)
        Int64.shift_left (Int64.rem opcode 127L) 25
    | (* TODO : debug *) h :: t ->
        Int64.logor (Int64.shift_left h (i * 5)) (head_instr (i - 1) t)
  in (((head_instr 4 regs) :: imms), ((List.length imms) + 1))

let rec code_gen pos stream =
  let rule_0 offset stream =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some (Reg reg) ->
          (Stream.junk __strm; make_instr (Int64.add offset 0L) [ reg ] [])
      | Some (Imm imm) ->
          (Stream.junk __strm; make_instr (Int64.add offset 1L) [] [ imm ])
      | Some (Ptr ptr) ->
          (Stream.junk __strm;
           register_ptr ptr (pos + 1);
           make_instr (Int64.add offset 1L) [] [ 0L ])
      | _ -> raise Stream.Failure
  and rule_1 offset stream =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some (Reg reg1) ->
          (Stream.junk __strm;
           let (__strm : _ Stream.t) = stream
           in
             (match Stream.peek __strm with
              | Some (Reg reg2) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg3) ->
                        (Stream.junk __strm;
                         make_instr (Int64.add offset 0L)
                           [ reg1; reg2; reg3 ] [])
                    | _ -> raise (Stream.Error "")))
              | Some (Imm imm1) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg2) ->
                        (Stream.junk __strm;
                         make_instr (Int64.add offset 1L) [ reg1; reg2 ]
                           [ imm1 ])
                    | _ -> raise (Stream.Error "")))
              | Some (Ptr ptr) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg2) ->
                        (Stream.junk __strm;
                         register_ptr ptr (pos + 1);
                         make_instr (Int64.add offset 1L) [ reg1; reg2 ]
                           [ 0L ])
                    | _ -> raise (Stream.Error "")))
              | _ -> raise Stream.Failure))
      | Some (Imm imm) ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg1) ->
                (Stream.junk __strm;
                 (match Stream.peek __strm with
                  | Some (Reg reg2) ->
                      (Stream.junk __strm;
                       make_instr (Int64.add offset 2L) [ reg1; reg2 ]
                         [ imm ])
                  | _ -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | Some (Ptr ptr) ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg1) ->
                (Stream.junk __strm;
                 (match Stream.peek __strm with
                  | Some (Reg reg2) ->
                      (Stream.junk __strm;
                       register_ptr ptr (pos + 1);
                       make_instr (Int64.add offset 2L) [ reg1; reg2 ] [ 0L ])
                  | _ -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | _ -> raise Stream.Failure
  and rule_2 offset stream =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some (Reg reg1) ->
          (Stream.junk __strm;
           let (__strm : _ Stream.t) = stream
           in
             (match Stream.peek __strm with
              | Some (Reg reg2) ->
                  (Stream.junk __strm;
                   let (__strm : _ Stream.t) = stream
                   in
                     (match Stream.peek __strm with
                      | Some (Reg reg3) ->
                          (Stream.junk __strm;
                           make_instr (Int64.add offset 0L)
                             [ reg1; reg2; reg3 ] [])
                      | Some (Imm imm) ->
                          (Stream.junk __strm;
                           make_instr (Int64.add offset 1L) [ reg1; reg2 ]
                             [ imm ])
                      | Some (Ptr ptr) ->
                          (Stream.junk __strm;
                           register_ptr ptr (pos + 1);
                           make_instr (Int64.add offset 1L) [ reg1; reg2 ]
                             [ 0L ])
                      | _ -> raise Stream.Failure))
              | Some (Imm imm1) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg2) ->
                        (Stream.junk __strm;
                         make_instr (Int64.add offset 2L) [ reg1; reg2 ]
                           [ imm1 ])
                    | _ -> raise (Stream.Error "")))
              | Some (Ptr ptr) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg2) ->
                        (Stream.junk __strm;
                         register_ptr ptr (pos + 1);
                         make_instr (Int64.add offset 2L) [ reg1; reg2 ]
                           [ 0L ])
                    | _ -> raise (Stream.Error "")))
              | _ -> raise Stream.Failure))
      | Some (Imm imm1) ->
          (Stream.junk __strm;
           let (__strm : _ Stream.t) = stream
           in
             (match Stream.peek __strm with
              | Some (Reg reg1) ->
                  (Stream.junk __strm;
                   let (__strm : _ Stream.t) = stream
                   in
                     (match Stream.peek __strm with
                      | Some (Reg reg2) ->
                          (Stream.junk __strm;
                           make_instr (Int64.add offset 3L) [ reg1; reg2 ]
                             [ imm1 ])
                      | Some (Imm imm2) ->
                          (Stream.junk __strm;
                           make_instr (Int64.add offset 4L) [ reg1 ]
                             [ imm1; imm2 ])
                      | Some (Ptr ptr) ->
                          (Stream.junk __strm;
                           register_ptr ptr (pos + 2);
                           make_instr (Int64.add offset 4L) [ reg1 ]
                             [ imm1; 0L ])
                      | _ -> raise Stream.Failure))
              | Some (Imm imm2) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg) ->
                        (Stream.junk __strm;
                         make_instr (Int64.add offset 5L) [ reg ]
                           [ imm1; imm2 ])
                    | _ -> raise (Stream.Error "")))
              | Some (Ptr ptr) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg) ->
                        (Stream.junk __strm;
                         register_ptr ptr (pos + 1);
                         make_instr (Int64.add offset 5L) [ reg ]
                           [ imm1; 0L ])
                    | _ -> raise (Stream.Error "")))
              | _ -> raise Stream.Failure))
      | Some (Ptr ptr) ->
          (Stream.junk __strm;
           register_ptr ptr (pos + 1);
           let (__strm : _ Stream.t) = stream
           in
             (match Stream.peek __strm with
              | Some (Reg reg1) ->
                  (Stream.junk __strm;
                   let (__strm : _ Stream.t) = stream
                   in
                     (match Stream.peek __strm with
                      | Some (Reg reg2) ->
                          (Stream.junk __strm;
                           make_instr (Int64.add offset 3L) [ reg1; reg2 ]
                             [ 0L ])
                      | Some (Imm imm1) ->
                          (Stream.junk __strm;
                           make_instr (Int64.add offset 4L) [ reg1 ]
                             [ 0L; imm1 ])
                      | Some (Ptr ptr2) ->
                          (Stream.junk __strm;
                           register_ptr ptr2 (pos + 2);
                           make_instr (Int64.add offset 4L) [ reg1 ]
                             [ 0L; 0L ])
                      | _ -> raise Stream.Failure))
              | Some (Imm imm1) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg) ->
                        (Stream.junk __strm;
                         make_instr (Int64.add offset 5L) [ reg ]
                           [ 0L; imm1 ])
                    | _ -> raise (Stream.Error "")))
              | Some (Ptr ptr2) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg) ->
                        (Stream.junk __strm;
                         register_ptr ptr2 (pos + 2);
                         make_instr (Int64.add offset 5L) [ reg ] [ 0L; 0L ])
                    | _ -> raise (Stream.Error "")))
              | _ -> raise Stream.Failure))
      | _ -> raise Stream.Failure
  and rule_3 offset stream =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some (Reg reg1) ->
          (Stream.junk __strm;
           let (__strm : _ Stream.t) = stream
           in
             (match Stream.peek __strm with
              | Some (Reg reg2) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg3) ->
                        (Stream.junk __strm;
                         make_instr (Int64.add offset 0L)
                           [ reg1; reg2; reg3 ] [])
                    | _ -> raise (Stream.Error "")))
              | Some (Imm imm1) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg2) ->
                        (Stream.junk __strm;
                         make_instr (Int64.add offset 1L) [ reg1; reg2 ]
                           [ imm1 ])
                    | _ -> raise (Stream.Error "")))
              | Some (Ptr ptr) ->
                  (Stream.junk __strm;
                   (match Stream.peek __strm with
                    | Some (Reg reg2) ->
                        (Stream.junk __strm;
                         register_ptr ptr (pos + 1);
                         make_instr (Int64.add offset 1L) [ reg1; reg2 ]
                           [ 0L ])
                    | _ -> raise (Stream.Error "")))
              | _ -> raise Stream.Failure))
      | _ -> raise Stream.Failure in
  let (code, size) =
    let (__strm : _ Stream.t) = stream
    in
      match Stream.peek __strm with
      | Some Stop -> (Stream.junk __strm; make_instr 0L [] [])
      | Some Sys ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Imm imm) ->
                (Stream.junk __strm; make_instr 1L [] [ imm ])
            | _ -> raise (Stream.Error "")))
      | Some Call ->
          (Stream.junk __strm;
           (try rule_0 2L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Ret ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Imm imm) ->
                (Stream.junk __strm; make_instr 4L [] [ imm ])
            | _ -> raise (Stream.Error "")))
      | Some Jmp ->
          (Stream.junk __strm;
           (try rule_0 5L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Jz ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg) ->
                (Stream.junk __strm;
                 (try rule_0 7L __strm
                  with | Stream.Failure -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | Some Jnz ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg) ->
                (Stream.junk __strm;
                 (try rule_0 9L __strm
                  with | Stream.Failure -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | Some Lt ->
          (Stream.junk __strm;
           (try rule_1 11L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Let ->
          (Stream.junk __strm;
           (try rule_1 14L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Eq ->
          (Stream.junk __strm;
           (try rule_1 17L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Load ->
          (Stream.junk __strm;
           (try rule_1 20L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Store ->
          (Stream.junk __strm;
           (try rule_2 23L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Push ->
          (Stream.junk __strm;
           (try rule_0 29L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Pop ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg) ->
                (Stream.junk __strm; make_instr 31L [ reg ] [])
            | _ -> raise (Stream.Error "")))
      | Some And ->
          (Stream.junk __strm;
           (try rule_3 32L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Or ->
          (Stream.junk __strm;
           (try rule_3 34L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Xor ->
          (Stream.junk __strm;
           (try rule_3 36L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Shl ->
          (Stream.junk __strm;
           (try rule_1 38L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Shr ->
          (Stream.junk __strm;
           (try rule_1 41L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Add ->
          (Stream.junk __strm;
           (try rule_3 44L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Sub ->
          (Stream.junk __strm;
           (try rule_1 46L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Mul ->
          (Stream.junk __strm;
           (try rule_3 49L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Div ->
          (Stream.junk __strm;
           (try rule_1 51L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Bcnt ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg1) ->
                (Stream.junk __strm;
                 (match Stream.peek __strm with
                  | Some (Reg reg2) ->
                      (Stream.junk __strm; make_instr 54L [ reg1; reg2 ] [])
                  | _ -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | Some Rev ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg1) ->
                (Stream.junk __strm;
                 (match Stream.peek __strm with
                  | Some (Reg reg2) ->
                      (Stream.junk __strm; make_instr 55L [ reg1; reg2 ] [])
                  | _ -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | Some Msb ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg1) ->
                (Stream.junk __strm;
                 (match Stream.peek __strm with
                  | Some (Reg reg2) ->
                      (Stream.junk __strm; make_instr 56L [ reg1; reg2 ] [])
                  | _ -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | Some Cvrt ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg1) ->
                (Stream.junk __strm;
                 (match Stream.peek __strm with
                  | Some (Reg reg2) ->
                      (Stream.junk __strm; make_instr 57L [ reg1; reg2 ] [])
                  | _ -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | Some Ftrc ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some (Reg reg1) ->
                (Stream.junk __strm;
                 (match Stream.peek __strm with
                  | Some (Reg reg2) ->
                      (Stream.junk __strm; make_instr 58L [ reg1; reg2 ] [])
                  | _ -> raise (Stream.Error "")))
            | _ -> raise (Stream.Error "")))
      | Some Flt ->
          (Stream.junk __strm;
           (try rule_1 59L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Flet ->
          (Stream.junk __strm;
           (try rule_1 62L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Fadd ->
          (Stream.junk __strm;
           (try rule_3 65L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Fsub ->
          (Stream.junk __strm;
           (try rule_1 67L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Fmul ->
          (Stream.junk __strm;
           (try rule_3 70L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some Fdiv ->
          (Stream.junk __strm;
           (try rule_1 72L __strm
            with | Stream.Failure -> raise (Stream.Error "")))
      | Some (Label lbl) ->
          (Stream.junk __strm; register_label lbl pos; ([], pos))
      | _ -> failwith "Problème dans l'assembleur" in
  let (next, last_pos) = code_gen (pos + size) stream
  in ((code @ next), last_pos)

let solve_data last_pos =
  let get _ = true in
  let rec aux pos =
    try
      let (key, lst) = Dict.find_first get !data
      in
        (data := Dict.remove key !data;
         register_label key pos;
         lst @ (aux ((List.length lst) + pos)))
    with | Not_found -> []
  in aux last_pos

let solve_labels code =
  if not (Dict.is_empty !wait_for_lbl)
  then
    failwith "Un ou plusieurs addresses sont utilisées mais non déclarées"
  else (* TODO : exc *)
    (let rec aux () =
       let get _ = true
       in
         try
           let (key, (lst, pos)) = Dict.find_first get !lbl in
           let rec solve_label =
             function
             | [] -> ()
             | h :: t -> (code.(h) <- Int64.of_int pos; solve_label t)
           in solve_label lst
         with | Not_found -> ()
     in (aux (); code))

(* TODO Si une parse failure arrive car on arrive en fin de fichier et une règle est incomplète, on peut comparer l'emplacement obtenu avec celui de la fin du fichier pour indiquer que celui-ci n'est pas terminé *)
let assembler file_path =
  try
    let (solved_stream, _) = solve_file file_path [ file_path ] in
    (* obtenir un placement absolu *)
    let lex_stream = lexer solved_stream in
    let (code, last_pos) = code_gen 0 lex_stream in
    let solved_data = solve_data last_pos in
    let final_code = solve_labels (Array.of_list (code @ solved_data))
    in Array.iter (fun (x : int64) -> Printf.printf "%Lx" x) final_code
  with | _ -> failwith "Error"


