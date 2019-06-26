exception RelativePath;;


let is_space c = function
    ' ' | '\t' | '\r' | '\012' -> true
|   _ -> false
;;

let is_symbol c = function
  	'#' | ':' | ',' | '(' | ')' | '+' | '-' | '*' | '/' | '%' | '^' | '&' | '|' | '~' | '@' | '>' | '<' | '[' | ';' | ']' -> true
|	_ -> false
;;

let split stream =
    let rec aux current_stream current_word = parser
        [< ''\n' ; next = aux [< >] "" >] ->
            if current_word  = "" then
                current_stream::next
            else
                [< current_stream ; 'current_word >]::next
    |   [< 'c when is_space c ; next = aux [< current_stream ; 'current_word >] "" >] -> next
    in aux [< >] "" stream
;;

let assemble pos = function
    [] ->
| h::t -> begin
        let aux = parser
            [< >]

        in let new_pos, inter_code = aux h
        in inter_code::(assemble new_pos t)
    end
;;






let assembler file_path =


    if Filename.is_relative file_path then
		raise RelativePath;
    let chn = open_in file_path in
    let raw_instr = split (Stream.of_channel chn) in
    let code = assemble 0 raw_instr
;;