(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

fun all_except_option(str: string, sl: string list) =
  let
    fun search(head, tail) =
      case tail of
        [] => (false, [])
      | t :: tail' =>
          if same_string(t,str) then (true, head @ tail')
          else search(head @ [t], tail')
  in
    case search([], sl) of
      (true, ls) => SOME ls
    | _ => NONE
  end

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
