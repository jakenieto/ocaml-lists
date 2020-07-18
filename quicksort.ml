let my_list = [6;4;5;2;3;0;9;8;7;1]

let rec quicksort list = match list with
  | [] -> [] 
  | first::rest -> 
    let l_set = List.filter (function ele -> (ele < first)) rest in
    let g_set = List.filter(function ele -> (ele >= first)) rest in
    (quicksort l_set) @ [first] @ (quicksort g_set)
  

