(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

			       (**** you can put all your code here ****)
val only_capitals= List.filter(fn x => Char.isUpper(String.sub(x,0)))
			      
fun longest_string1 xs=
    List.foldl (fn (x,y) => if String.size x <= String.size y
			    then y
			    else x) ""  xs
fun longest_string2 xs=
    List.foldl (fn (x,y) => if String.size x < String.size y
			    then y
			    else x) ""  xs
fun helper x y = if String.size  x> String.size y then x
		 else y
			  
fun longest_string_helper f ss=
    List.foldl (fn(x,y) => if f(String.size x,String.size y)
			  then y
			   else x) "" ss
fun longest_string3 ss= longest_string_helper (fn (x,y)=> x<=y) ss
fun longest_string4 ss= longest_string_helper (fn (x,y)=> x<y) ss

fun longest_capitalized xs=
    (longest_string3 o only_capitals)  xs
fun rev_string xs=
    (String.implode o List.rev o String.explode) xs
(*('a -> 'b option)-> 'a list -> 'b (fn x => if x > 3 then SOME x else NONE)[1,2,3,4,5]=4
fun first_answer f ss=
    case  List.foldl (fn x=>if f( x) then SOME x  else NONE) NONE ss of
	NONE=> raise NoAnswer
	 | SOME x => x 	

	       
				  
							*)	  
fun first_answer f xs=
    case xs of
	[]=> raise NoAnswer
      | x::xs' =>case  f x of
		     NONE => first_answer f xs'
		  | SOME x1 => x1 
(*('a -> 'b list option) -> 'a list -> 'b list option*)
fun all_answers f xs=
    let fun helper(f, xs, acc) =
	    case xs of
	[]=> SOME acc
	     | x::xs' => case f x of
			     NONE=> NONE
			   | SOME lst =>  helper( f, xs', acc@lst)
    in
	helper (f, xs, [])
    end
fun longest_string3 ss= longest_string_helper (fn (x,y)=> x<=y) ss
fun count_wildcards(xs)= g (fn x=>1)(fn y=>0) xs
fun count_wild_and_variable_lengths (xs)= g (fn x=>1) (fn y=> String.size y) xs
fun count_some_var(pair: string*pattern)=g (fn x=>0) (fn y=> if y = (#1 pair)
							    then 1 else 0) (#2 pair)
fun check_pat (p1)=
    let fun helper(p)=
	case p of
	  Wildcard =>[]
         | Variable s=>[s]
	 | UnitP=>[]
	 | ConstP x =>[]
	 | TupleP list=> List.foldl(fn(p,acc)=>acc @ helper(p)) [] list
	 | ConstructorP(s, pat)=> helper(pat)
    in
	let fun uni(patt)=
		case patt of
		    []=>true
		  | p::(p1::patt') => if p= p1
				      then false
				      else uni(patt')
	in
	    uni(helper(p1))
		end

    end

fun match (valu,pat) =
    case (valu,pat) of
	      (_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,valu)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				  then all_answers match (ListPair.zip(vs,ps))
				  else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
						   then match(v,p)
                                                   else NONE
      | _ => NONE

									
			
