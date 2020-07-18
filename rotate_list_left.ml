let my_list = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]

let rec rotate_list list n = match (list, n) with 
  | (_,0) -> list
  | ([], n) -> []
  | (first::rest, n) -> (rotate_list (rest @ [first]) (n - 1)) 
  