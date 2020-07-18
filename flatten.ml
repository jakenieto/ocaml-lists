type 'a node =
  | One of 'a 
  | Many of 'a node list

let my_list = [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]

let rec flatten list = match list with 
  | [] -> []
  | first::rest -> match first with 
    | One(a) -> a::flatten rest
    | Many(b) -> flatten b @ flatten rest