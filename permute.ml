(* let my_list = ["a";"b";"c";"d"]  *)
(* output: [[1;2;3];[1;3;2];[2;1;3];[2;3;1];[3;1;2];[3;2;1]] *)

(* MONAD-ISH *)
let concat_map f list = List.concat (List.map f list) 


(* func (ele, list of ele's) -->  List[List[ele's]]) *)      
let rec all_insertions ele list_of_eles = match list_of_eles with
  | [] -> [[ele]]
  | first::rest -> [ele::first::rest] @ List.map (fun list -> first::list) (all_insertions ele rest)

let rec permute list = match list with
  | [] -> [[]]
  | first::rest -> 
      let rest_permutations = permute rest in
      concat_map (fun rest_permutation -> all_insertions first rest_permutation) rest_permutations


let () = Printf.printf "Answer: \n[%s]\n" ( (String.concat "], \n[") (List.map (String.concat ", ") (permute my_list) )) 



