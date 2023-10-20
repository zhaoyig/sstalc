(* Convert individual reg_asgn_item to kv pair *)
(* let reg_asgn_item_to_pair (x : reg_asgn_item) =
  match x with
  | RegAsgnItem (r, t) -> (r, t)  *)

(* Inverse of ^ *)
(* let pair_to_reg_asgn_item (x : (reg * ty)) =
  let (r, t) = x in
  RegAsgnItem (r, t) *)

(** Update all elements with key `k` to have value `v` in a assoc list `l`
    If not present, insert this new element *)
let rec update_assoc_list k v l =
  match l with
  | [] -> [(k, v)]
  | h :: t -> 
    let (kk, _) = h in
    if kk == k then (kk, v) :: update_assoc_list k v t
    else h :: update_assoc_list k v t