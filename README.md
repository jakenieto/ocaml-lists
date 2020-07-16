# ocaml-nats - Learning OCaml

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
## Inspiration
I would like to thank Michael Delmonaco for his help in teaching me OCaml and giving me the inspiration for this project. 
<https://quasarbright.github.io>
