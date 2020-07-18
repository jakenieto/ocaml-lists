# ocaml-lists - Learning OCaml

  This is my 2nd project in OCaml. I implemented the a function to compute the power set and set of all permutations of an input list.

## Power set
```
let rec power_set list = match list with
  | [] -> [[]]
  | first::rest -> 
    let rest_power_set = power_set rest in
    rest_power_set @ insert_ele first rest_power_set 
```
## Permutations

```
let concat_map f list = List.concat (List.map f list) 

let rec permute list = match list with
  | [] -> [[]]
  | first::rest -> 
      let rest_permutations = permute rest in
      concat_map (fun rest_permutation -> all_insertions first rest_permutation) rest_permutations
```

## Subsets

```
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

```
## Prefix

```
(* consumes list a and b and returns boolean whether a is a prefix of b or not *)
let rec is_prefix a b = match (a,b) with
  | ([],_) -> true
  | (_,[]) -> false
  | (first1::rest1, first2::rest2) -> first1 = first2 && (is_prefix rest1 rest2)

```
## Quicksort
```
let rec quicksort list = match list with
  | [] -> [] 
  | first::rest -> 
    let l_set = List.filter (function ele -> (ele < first)) rest in
    let g_set = List.filter(function ele -> (ele >= first)) rest in
    (quicksort l_set) @ [first] @ (quicksort g_set)
```
## Flatten 
```
(* Here is the 'node' type that allows for lists to be nested *)
type 'a node =
  | One of 'a 
  | Many of 'a node list
  

let rec flatten list = match list with 
  | [] -> []
  | first::rest -> match first with 
    | One(a) -> a::flatten rest
    | Many(b) -> flatten b @ flatten rest
```
## Rotate
```
let rec rotate_list list n = match (list, n) with 
  | (_,0) -> list
  | ([], n) -> []
  | (first::rest, n) -> (rotate_list (rest @ [first]) (n - 1)) 
  
```
## Inspiration
I would like to thank Michael Delmonaco for his help in teaching me OCaml and giving me the inspiration for this project. 
<https://quasarbright.github.io>
