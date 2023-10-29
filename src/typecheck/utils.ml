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
    if kk = k then (kk, v) :: t
    else h :: update_assoc_list k v t

(* get the first element in a three tuple *)
let get_1 (a,_,_) = a

let get_2 (_,a,_) = a

let get_3 (_,_,a) = a
