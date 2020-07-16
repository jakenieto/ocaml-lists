
(* input: [1;2;3] -> [[]; [1]; [2]; [3]; [1;2]; [1;3]; [2;3] [1;2;3]]*)
let my_list = ["a";"b";"c"]

(* returns a list of lists with the first element listed with  *)
let insert_ele ele list = match list with
  | [] -> [[]]
  | list -> List.map (fun rest_ele ->  ele::rest_ele) list


let rec power_set list = match list with
  | [] -> [[]]
  | first::rest -> 
    let rest_power_set = power_set rest in
    rest_power_set @ insert_ele first rest_power_set 


    let () = Printf.printf "Answer: \n[%s]\n" ( (String.concat "], \n[") (List.map (String.concat ", ") (power_set my_list) )) 
