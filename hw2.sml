(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

fun all_except_option(str: string, lst: string list) =
  case lst of
    []     => NONE
  | s::lst' => if same_string(s,str) then SOME lst'
               else case all_except_option(str, lst') of
                     NONE   => NONE
                   | SOME x => SOME (s::x)

fun get_substitutions1(lst: string list list, str: string) =
  case lst of
    [] => []
  | l::lst' => case all_except_option(str, l) of
                 NONE   => get_substitutions1(lst', str)
               | SOME x => x @ get_substitutions1(lst', str)

fun get_substitutions2(lst: string list list, str: string) =
  let
    fun aux(lst, str, acc) =
      case lst of
        [] => acc
      | l::lst' => case all_except_option(str, l) of
                     NONE   => aux(lst', str, acc)
                   | SOME x => aux(lst', str, acc @ x)
  in
    aux(lst, str, [])
  end

fun similar_names(lst: string list list, {first=f,middle=m,last=l}) =
  let
    fun build_names(subs: string list) =
      case subs of
        [] => []
      | name::subs' => {first=name,middle=m,last=l}::build_names(subs')
  in
    {first=f, middle=m, last=l}::build_names(get_substitutions1(lst,f))
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

fun card_color(c: card) =
  case c of
    (Clubs, _)  => Black
  | (Spades, _) => Black
  | _           => Red

fun card_value(c: card) =
  case c of
    (_, Num x) => x
  | (_, Ace)   => 11
  | _          => 10

fun remove_card(cs: card list, c: card, e: exn) =
  case cs of
    []     => raise e
  | x::cs' => if x=c then cs' else x::remove_card(cs', c, e)

fun all_same_color(cs: card list) =
  case cs of
    x::y::rest => if card_color(x)=card_color(y) then all_same_color(y::rest)
                  else false
  | _  => true (* empty or one element lists are both true *)

fun sum_cards(cs: card list) =
  let
    fun aux(cs, acc) =
      case cs of
        [] => acc
      | c::cs' => aux(cs', acc+card_value(c))
  in
    aux(cs, 0)
  end

fun score(cs: card list, goal: int) =
  let
    fun prelim_score(sum, goal) =
      if sum > goal then 2 * (sum - goal)
      else goal - sum
  in
    case all_same_color(cs) of
      false => prelim_score(sum_cards(cs), goal)
    | true  => prelim_score(sum_cards(cs), goal) div 2
  end

fun officiate(cl: card list, ml: move list, goal: int) =
  let
    fun process(cl: card list, hc: card list, ml: move list, goal: int) =
      case ml of (* check for remaining moves *)
        []     => score(hc, goal)
      | m::ml' => case m of (* check for type of move *)
                    Discard c => process(cl, remove_card(hc, c, IllegalMove), ml', goal)
                  | Draw      => case cl of (* check for empty draw pile *)
                                   [] => score(hc, goal)
                                 | y::cl' => if sum_cards(y::hc) > goal then score(y::hc, goal)
                                             else process(cl', y::hc, ml', goal)
  in
    process(cl, [], ml, goal)
  end
