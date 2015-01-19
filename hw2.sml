(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

fun all_except_option(str: string, sl: string list) =
  let
    fun includes([], str) = false
      | includes(s::lst, str) = if same_string(s,str) then true
                                else includes(lst, str)
    fun delete ([], str) = []
      | delete (s::lst, str) = if same_string(s,str) then delete(lst, str)
                               else s::delete(lst,str)
  in
    if includes(sl,str) then SOME (delete(sl,str))
    else NONE
  end

fun get_substitutions1(lst: string list list, str: string) =
  case lst of
    [] => []
  | l::lst' => case all_except_option(str, l) of
                 NONE => get_substitutions1(lst', str)
               | SOME x => x @ get_substitutions1(lst', str)

(* fun get_substitutions2(lst: string list list, str: string) = *)
(*   let *)
(*     (* add local helper with tail recursion *) *)
(*   in *)
(*     (* add code *) *)
(*   end *)

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)
