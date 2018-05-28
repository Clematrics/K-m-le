let get_last = function
    [] -> raise Not_found
|   [x] -> x
|   h::t -> get_last t
;;