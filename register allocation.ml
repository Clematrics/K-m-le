let union l1 l2 = match l1 with
	[] -> l2
|	h::t when List.mem h l2-> union t l2
|	h::t -> union t (h::l2)
;;

let rec difference l1 l2 = match l1 with
	[] -> []
|	h::t when List.mem h l2 -> difference t l2 (* predicate mem *)
|	h::t -> h::(difference t l2)
;;

let distinct_set l1 l2 =
	difference l1 l2 = [] && difference l2 l1 = []
;;

let liveness_analysis cfg cfg_last =
	let changed = ref true
	and i = ref 0 in
	while !changed do
		let rec browse = function (* browse cfg for i-th iteration *)
			[] -> ()
		|	h::t when h.process_nb = !i -> browse t (* élément déjà traité à étape i *)
		|	h::t ->
				h.last_in <- h.in_;
				h.last_out <- h.out
				h.in_ <- union h.def (difference h.out h.def);
				h.out <- List.fold_left union [] (successor h);
				if distinct_set h.in_ h.last_in || distinct_set h.out h.last_out then
					changed := true;
				browse (i+1) (List.append h.previous t)
		in
		changed := false;
		browse [cfg_last];
		incr i
	done
;;

(* gros graphe avec beaucoup de variables : préférer liste d'adjacence *)
(* parcours en largeur : pour marquer les nodes déjà parcourues, on assigne -1 à process_nb *)
let interference_graph cfg =
	let graph = [] in
	let 
	let rec browse = function
		[] -> ()
	|	h::t ->
		if h.process_nb != -1 then begin
			h.process_nb <- -1;
			if h.next = None && h.alternate = None then
				browse t
			else if h.next = None then
				browse (h.alternate)::t
			else if h.alternate = None then
				browse (h.next)::t
			else
				browse (h.next)::(h.alternate)::t
		end
		else
			browse t
	in
	browse [cfg];
	graph
;;