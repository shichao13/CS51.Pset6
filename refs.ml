(* NAMES:
 *
 * Partner 1's name: Michael Ma
 * Partner 1's code.seas account: mma
 *
 * (Leave blank if you are working alone)
 * Partner 2's name: Allen Chen
 * Partner 2's code.seas account: _______
 *)

open Core.Std

(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)
let has_cycle (lst : 'a mlist) : bool =
  (* Use runner technique stepping once and twice to detect cycle. *)
  let rec runner (onestep : 'a mlist) (twostep : 'a mlist) : bool =
    match onestep, twostep with
    (* No cycle if Nil. *)
    | Nil, _ -> false
    | _, Nil -> false
    | Cons(_, a), Cons(_,b) ->
      match !b with
      | Nil -> false
      (* The two pointers reference the same thing. *)
      | Cons(_, c) -> (phys_equal (!a) (!c)) || runner !a !c in
  runner lst lst  
;;   

(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

(* Tests *)
assert(has_cycle list1a = false);;
assert(has_cycle list1b = false);;
assert(has_cycle list1 = false);;
assert(has_cycle list2 = true);;


(*>* Problem 1.2 *>*)
(* Write a function flatten that flattens a list (removes its cycles if it
 * has any) destructively. Again, you may want a recursive helper function and
 * you shouldn't worry about space. *)
let flatten (lst : 'a mlist) : unit =
  let rec runner (onestep : 'a mlist) (twostep : 'a mlist) : unit =
    match onestep, twostep with
    (* No action if no cycle. *)
    | Nil, _ -> ()
    | _, Nil -> ()
    | Cons(_,a), Cons(_,b) ->
      match !b with
      | Nil -> ()
      (* Remove cycle by pointing to Nil. *)
      | Cons(_,c) -> if (phys_equal (!a) (!c)) then (a := Nil) else runner !a !c in
  runner lst lst  
;;   

(* Tests *)
assert(flatten list1b; list1b = Cons(2,{contents = Cons(2,{contents = Nil})}));;
assert(flatten list2; list2 = Cons(1,{contents = Cons(2,{contents = Nil})}));;  

(*>* Problem 1.3 *>*)
(* Write mlength, which finds the number of nodes in a mutable list. *)
let mlength (lst : 'a mlist) : int =
  (* Recursively counts nodes. *)
  let rec count (dlst : 'a mlist) (cnt : int) : int =
    match dlst with
    | Nil -> cnt
    | Cons(_,a) ->
      match !a with
      | Nil -> cnt+1
      | Cons(_,b) -> count !b (cnt+2) in 
  (* Flatten list if there's a cycle before counting. *)
  if has_cycle lst then count (flatten lst; lst) 0 else count lst 0
;;

(* Tests *)
assert(mlength list1a = 1);;
assert(mlength list1 = 3);;
assert(mlength list2 = 2);;


(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 120
