(* Tests for assignment 2 *)

(*
 * Version 1.3
*
* Changelog:
* 2015-01-20  dmg  <dmg@uvic.ca>
*       * fixed test11_6, test11_8, test11_12 (game should stop when the sum
*          of the cards---not the score--is above the goal)
*
* 2015-01-19  dmg  <dmg@uvic.ca>
*	* fixed test11_10
*
* 2015-01-17  dmg  <dmg@uvic.ca>
*	* fixed test7_1
*
*)

(* Place your tests at the end of the file *)

use "hw2.sml";

val test1_1=all_except_option("3",["4","9","10"]) = NONE
val test1_2=all_except_option("3",["4","9","3","10"]) = SOME ["4","9","10"]
val test1_3=all_except_option("3",[]) = NONE
val test1_4=all_except_option("3",["3","4","9","10"])  = SOME ["4","9","10"]
val test1_5=all_except_option("3",["4","9","10","3"]) = SOME ["4","9","10"]
val test1_6=all_except_option("3",["3"]) = SOME []

val test2_1=get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                               "Fred")
            = ["Fredrick","Freddie","F"]
val test2_2=get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
                               "Jeff")
            = ["Jeffrey","Geoff","Jeffrey"]
val test2_3=get_substitutions1([["Neo","New"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
                               "Tank")
            = ["Panzer","Sherman","Container"]
val test2_4=get_substitutions1([["Neo","New", "Nuovo"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
                               "Neo")
            = ["New","Nuovo"]

val test3_1=get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                               "Fred")
            = ["Fredrick","Freddie","F"]
val test3_2=get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
                               "Jeff")
            = ["Jeffrey","Geoff","Jeffrey"]
val test3_3=get_substitutions2([["Neo","New"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
                               "Tank")
            = ["Panzer","Sherman","Container"]
val test3_4=get_substitutions1([["Neo","New", "Nuovo"],["Panzer","Tank","Sherman"],["Tank", "Container"],["Epoch","Era"]],
                               "Neo")
            = ["New","Nuovo"]

val test4_1=similar_names([
                             ["Thomas", "Neo"],
                             ["Batman", "Hulk","Bruce"],
                             ["Spiderman", "Peter"]
                         ], {first="Bruce", middle = "(whoknows)", last="Wayne"}) =
            [{first="Bruce",last="Wayne",middle="(whoknows)"},
             {first="Batman",last="Wayne",middle="(whoknows)"},
             {first="Hulk",last="Wayne",middle="(whoknows)"}]

val test4_2=similar_names([
                             ["Fred","Fredrick"],
                             ["Elizabeth","Betty"],
                             ["Freddie","Fred","F"]
                         ], {first="Fred", middle="W", last="Smith"}) =
            [{first="Fred",last="Smith",middle="W"},
             {first="Fredrick",last="Smith",middle="W"},
             {first="Freddie",last="Smith",middle="W"},
             {first="F",last="Smith",middle="W"}]

val ClubAce = (Clubs,Ace)
val DiamondsJack = (Diamonds,Jack)
val Hearts10 = (Hearts, Num 10)
val Spades5 = (Spades,Num 5)

val test5_1= card_color(ClubAce) = Black
val test5_2= card_color(DiamondsJack) = Red
val test5_3= card_color(Hearts10) = Red
val test5_4= card_color(Spades5) = Black

val test6_1= card_value(ClubAce) = 11
val test6_2= card_value(DiamondsJack) = 10
val test6_3= card_value(Hearts10) = 10
val test6_4= card_value(Spades5) = 5
val test6_5= card_value(Spades, Queen) = 10
val test6_6= card_value(Spades, King) = 10

exception notFound

val cards1 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 4), (Clubs, Num 4)]
val cards2 = []
val cards3 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 5), (Clubs, Num 9)]
val cards4 = [(Clubs, Ace), (Clubs, Num 10), (Clubs, Num 5), (Clubs, Num 2)]
val cards5 = [(Diamonds, Ace), (Diamonds, Num 10), (Diamonds, Queen), (Diamonds, Jack), (Diamonds,King)]

val test7_1 = remove_card(cards3, (Clubs, Ace), notFound) = [(Diamonds,Num 10),(Spades,Num 5),(Clubs,Num 9)]
val test7_2 = remove_card(cards1, (Spades, Num 4), notFound) = [(Clubs, Ace), (Diamonds, Num 10), (Clubs, Num 4)]
val test7_3 = remove_card(cards3, (Clubs, Num 9), notFound) = [(Clubs,Ace),(Diamonds,Num 10),(Spades,Num 5)]
val test7_4 = remove_card(cards5, (Diamonds, Ace), notFound) = [(Diamonds, Num 10), (Diamonds, Queen), (Diamonds, Jack), (Diamonds,King)]
val test7_5 = remove_card(cards2, (Clubs, Ace), notFound) = [] handle notFound => true

val test8_1 = all_same_color(cards1) = false
val test8_2 = all_same_color(cards2) = true
val test8_3 = all_same_color(cards3) = false
val test8_4 = all_same_color(cards5) = true
val test8_5 = all_same_color(cards5) = true


val test9_1 = sum_cards(cards1) = 29
val test9_2 = sum_cards(cards2) = 0
val test9_3 = sum_cards(cards3) = 35
val test9_4 = sum_cards(cards4) = 28
val test9_5 = sum_cards(cards5) = 51

val test10_1 = score(cards1, 1) = 28 * 2
val test10_2 = score(cards2, 28) = 14 (* empty list is conssidered same color *)
val test10_3 = score(cards3, 35) = 0
val test10_4 = score(cards4, 28) = 0
val test10_5 = score([(Spades, Num 2)], 28) = 13
val test10_6 = score([(Diamonds, Ace), (Diamonds, Num 10)],20) = 1

val test11_1 = officiate(cards3, [], 10) = 5
val test11_2 = officiate(cards3, [Draw], 10) = 1
val test11_3 = officiate(cards3, [Draw], 5) = 6
val test11_4 = officiate(cards5, [Draw, Draw], 0) = 11
val test11_5 = officiate(cards3, [Draw, Draw], 15) = 12
val test11_6 = officiate(cards3, [Draw, Draw, Draw], 15) = 12
val test11_7 = officiate(cards3, [Draw, Draw, Draw, Draw], 35) = 0
val test11_8 = officiate(cards3, [Draw, Draw, Draw, Discard (Spades, Num 5)], 15) = 12
val test11_9 = officiate(cards5, [Draw, Draw, Draw, Discard (Spades, Num 2)], 45) = 10 handle IllegalMove => true
val test11_10 = officiate(cards2, [Draw], 10) = 5
val test11_11 = officiate(cards2, [Discard (Spades, Ace)], 10) = 5 handle IllegalMove => true
val test11_12 = officiate(cards3, [Draw, Discard (Spades, Num 7)], 10) = 1

(*********************************************************************** *)
(* Your tests go after this *)

val JackOfDiamonds = (Diamonds, Jack)
val TwoOfSpades = (Spades, Num 2)
val test_cards = [(Spades, Num 2), (Clubs, Queen), (Hearts, Queen)]
val test_cards2 = [(Spades, Num 2), (Clubs, Queen), (Spades, Jack)]

val test1_0=all_except_option("7",["6","7"]) = SOME ["6"]
val test2_0=get_substitutions1([["Charles","Charlie"],["Richard","Dick"]],
                               "Charles")
            = ["Charlie"]
val test3_0=get_substitutions2([["Charles","Charlie"],["Richard","Dick"]],
                               "Charles")
            = ["Charlie"]
val test4_0=similar_names([
                             ["Wade", "Deadpool"],
                             ["Hank", "Antman"],
                             ["Deathstroke", "Slade", "Terminator"]
                         ], {first="Slade", middle = "Joseph", last="Wilson"}) =
            [{first="Slade",last="Wilson",middle="Joseph"},
             {first="Deathstroke",last="Wilson",middle="Joseph"},
             {first="Terminator",last="Wilson",middle="Joseph"}]
val test5_0= card_color(JackOfDiamonds) = Red
val test6_0= card_value(TwoOfSpades) = 2
val test7_0 = remove_card(test_cards, (Hearts, Queen), notFound) = [(Spades,Num 2),(Clubs,Queen)]
val test8_0 = all_same_color(test_cards) = false
val test8_01 = all_same_color(test_cards2) = true
val test9_0 = sum_cards(test_cards) = 22
val test10_0 = score(test_cards, 18) = 4 * 2
val test11_0 = officiate(test_cards, [Draw, Draw], 4) = 8
