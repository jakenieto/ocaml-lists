
let rec quicksort list = match list with
  | [] -> [] 
  | first::rest -> 
    let l_set = List.filter (function ele -> (ele < first)) rest  in
    let g_set = List.filter(function ele -> (ele >= first)) rest in
    (quicksort l_set) @ [first] @ (quicksort g_set)