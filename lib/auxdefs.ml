(* Auxiliary function defintions *)

(* test if list is duplicate free *)
let rec duplicate_free = function
| [] -> true
| x :: xs -> not (List.mem x xs) && duplicate_free xs

let rec remove_duplicates_sorted = function
| x :: y :: r -> if x = y then remove_duplicates_sorted (y::r) else x:: (remove_duplicates_sorted (y::r))
| xs -> xs

(* Update an association list with a key-value pair by replacing an existing key *)
let rec update_assoc k v = function 
| [] -> [(k, v)]
| (x, w)::xs -> if k = x then (k, v)::xs else (x, w)::update_assoc k v xs

(* List product: every element of first list combined with every element of second list *)
let list_prod xs ys = List.concat_map (fun x -> List.map (fun y -> (x, y)) ys) xs

(* First list is a non-strict subset of second list.
   Both lists have to be sorted and without duplicates. *)
let rec is_subset_sorted = function
| ([], _) -> true
| (_, []) -> false
| (x::xs, y::ys) -> 
  if x = y 
  then is_subset_sorted (xs, ys)
  else if x < y then false
  else is_subset_sorted (x::xs, ys)

(* merge two sorted lists to produce a sorted list again, at the same time removing duplicates *)
  let rec merge_sorted = function
  | (xs, []) -> xs
  | ([], ys) -> ys
  | (x::xs, y::ys) -> 
    if x = y 
      then x::(merge_sorted (xs, ys))
      else if x < y
      then x::(merge_sorted (xs, y::ys))
      else y::(merge_sorted (x::xs, ys))

