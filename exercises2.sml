(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

	      (* put your solutions for problem 2 here *)
fun all_except_option(str,list)=
    case list of
	[] => NONE
      | x::list' => if same_string(x,str)  then
			
		      SOME( list')
							      
		    else case  all_except_option(str,list') of
			     NONE=>NONE
			   | SOME list'=>SOME(x::list')
					     
fun get_substitutions1(list,str)=
    case list of
	[]=>[]
      | x::list' => case all_except_option(str,x) of
			NONE=>[]@get_substitutions1(list',str)
		      | SOME x => x@ get_substitutions1(list',str)
fun get_substitutions2(list, str)=
    let fun helper(list,str,acc)=
	    case list of
	    []=>acc
	     | x::list' =>case all_except_option(str,x) of
			      NONE=>acc@get_substitutions1(list',str)
			    | SOME x =>acc@x@get_substitutions1(list',str)
							   
    in
	helper(list,str,[])
    end
	
fun similar_names(list,{first=x,last=y,middle=z})=
    let fun print(list1)=
	    case list1 of
	    []=>[]
	  | name::list1' => [{first=name,last=y,middle=z}]@print(list1')
    in print(x::get_substitutions1(list,x))
    end
	
					   

fun card_color c =
    case  c of		 
       (Clubs,_) =>Black
      | (Spades,_) =>Black
      |_ =>Red
 
	
fun card_value c=
    case c of
	(_,Num i)=>i
      | (_,Ace) =>11
      | _ => 10
		 
fun remove_card(list,c,ex)=
	       case list of
		   []=>[]
		 | c1::list' => if c1=c
			       then list'
			       else
				   case remove_card(list',c,ex) of
				       []=> raise ex
                                     | list' =>c::list'
fun sum_cards(list)=
    let fun sum(list,acc)=
	    case list of
		[]=> 0
	       | x::list' =>card_value(x)+acc+sum(list',acc)
    in
	sum(list,0)
    end
	
fun all_same_color(list1)=
    case list1 of
	[]=>true
	 | x::[] =>true 
      | u::(u1::list1') =>if  card_color(u)= card_color(u1) andalso all_same_color(u1::list1')
		    then true
		    else  false
    
fun score(list,goal)=
    if sum_cards(list)=goal
    then 0
    else  if all_same_color(list)andalso sum_cards(list)> goal
    then(3*(sum_cards(list)-goal))div 2
    else if all_same_color(list)andalso sum_cards(list)< goal
    then (goal-sum_cards(list))div 2
    else if all_same_color(list) = false andalso sum_cards(list)> goal
    then(3*(sum_cards(list)-goal))
    else if all_same_color(list) = false andalso sum_cards(list)<goal
    then(goal-sum_cards(list))
    else goal

fun officiate(cards,moves,goal)=
    let fun helper(cards,moves,hold,goal)=
	    case (moves,cards) of
		([],[])=>score(hold,goal)
	      | ([],_) =>score(hold,goal)
	      | (_,[]) =>score(hold,goal)
	  | (move::moves',card::cards') =>
			 case move of
			     Discard i=> helper(cards,moves',remove_card(hold,i,IllegalMove),goal)
			   | Draw  =>if  sum_cards(card::hold)> goal
				     then score(card::hold,goal)
				     else helper(cards',moves',hold,goal)

					      
						    
    in
	helper(cards,moves,[],goal)
    end
	(*
  fun officiate(cards,moves,goal)=
    let fun helper(cards,hold)=
	case moves of
	    []=>[]
	  | x::moves' =>
	    if x = Discard i
	    then remove_card(hold,card,NoCards)
			else  let fun add(cards,hold)=
			      case cards of
			      []=>[]
			       |   card::cards'=>card@hold			
			      in
				  add(cards,hold)
			      end
				  				
				  helper(cards,moves',hold)
    in
	helper(cards,moves,[])
    end
*)
	    
	    
