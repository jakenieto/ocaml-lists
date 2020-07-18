
(* consumes list a and b and returns boolean whether a is a prefix of b or not *)
let rec is_prefix a b = match (a,b) with
  | ([],_) -> true
  | (_,[]) -> false
  | (first1::rest1, first2::rest2) -> first1 = first2 && (is_prefix rest1 rest2)
