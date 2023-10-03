(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker,                        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Generic Identifiers
 * To be used for variabels, type constructor variables, labels, etc.
 * Has uniquefication features.
 *)

 type identifier = string * int;;  (* n=-1 means print source only *)

 let id_make s n =
   if n<0 then invalid_arg "Identifier.id_make" else (s,n)
 ;;
 
 let counter = ref (-1);;
 
 let id_new s = incr counter; (s,!counter);;
 let id_of_string s = (s,-1);;
 let id_unique (s,_) = incr counter; (s,!counter);;
 
 let id_to_string (s,n) =
   if n=(-1) then s else s^"$"^(string_of_int n)
 ;;
 let id_to_source (s,_) = s;;
 let id_prn fmt id = Format.pp_print_string fmt (id_to_string id);;
 
 (* N.B. order of compares is probably more efficient *)
 let id_compare (s1,n1) (s2,n2) =
   if n1=n2 then
     compare s1 s2
   else if n1<n2 then -1
   else 1
 ;;
 
 let mk_id s =
  let l = String.length s in
  let i = ref (l - 1) in
  while !i>=0 && s.[!i]<>'$' do decr i done;
  if !i<0 || !i=l-1 then
    id_of_string s
  else
    id_make (String.sub s 0 !i) (int_of_string (String.sub s (!i+1) (l- !i-1)))
;;
