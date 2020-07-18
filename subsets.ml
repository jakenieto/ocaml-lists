(* 
    Helper function that consumes a list and element and returns a 
    boolean for whether the list contains the element. 

    filters the list by removing the element and returns if the size of the filtered 
    list is not the same as the original list
*)
let has_ele_2 ele list = (List.length list) != List.length (List.filter (fun ele2 -> ele2 = ele) list)


(* 
    Helper function that consumes a list and element and returns a 
    boolean for whether the list contains the element.   

    recursively checks the list
*)
let rec has_ele ele list = match list with
  | [] -> false
  | first::rest -> (ele = first) || (has_ele ele rest)

(* 
    consumes a list and determines if it's a set i.e. it only contains 
    one of each unique element.
*)
let rec is_set set = match set with 
  | [] -> true
  | first::rest -> (not (has_ele first rest)) && (is_set rest)

  (* 
    consumes two lists and ensures they are both sets. If so, it determines if one is 
    a subset of the other by recursively checking that each element in list a
    is an element in list b. 
  *)
let rec is_subset a b = match (a,b) with
  | ([],[]) 
  | ([],_) -> true
  | (_,[]) -> false
  | (first::rest, b) when (is_set a && is_set b) -> (has_ele first b) && (is_subset rest b)
  | (first::rest, b) -> false
