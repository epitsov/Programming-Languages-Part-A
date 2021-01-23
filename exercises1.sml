fun is_older(date1 : int*int*int,date2 : int*int*int)=
    let 
        val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1
        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        y1 < y2 orelse (y1=y2 andalso m1 < m2)
                orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
    end 
		    
fun number_in_month(xs : (int*int*int) list , x : int)=
    if null xs
    then 0
    else if #2 (hd xs) = x
    then 1+number_in_month(tl(xs),x)	      
    else number_in_month(tl(xs),x)
			
fun number_in_months(xs : (int*int*int) list, ys: int list)=
    if null ys
    then 0
    else if null xs
    then 0		      
    else number_in_months(xs,tl ys)+ number_in_month(xs,hd ys)
				
			    	   
fun dates_in_month( xs : (int*int*int) list, x : int)=
    if null xs
    then []
    else if #2 (hd xs) = x
    then hd xs :: dates_in_month(tl(xs),x)
    else dates_in_month(tl(xs),x)
		       

fun dates_in_months(xs : (int*int*int) list , ys: int list)=
    if null ys
    then []
    else dates_in_month(xs,hd ys)@dates_in_months(xs,tl ys)
			
fun get_nth(xs: string list, x:int)=
    if null xs
    then ""
    else if x = 1
    then hd xs
    else get_nth(tl(xs), x-1)
		
fun date_to_string(xs :(int*int*int))=
    let val ys = ["January", "February", "March","April","May","June","July","August","September","October","November","December"]
    in
	get_nth(ys,#2 xs)^" "^Int.toString(#3 xs)^", "^Int.toString(#1xs)
					     
    end
	
fun number_before_reaching_sum(sum: int, xs: int list)=
    if sum - hd xs <= 0
    then 0
    else 1+ number_before_reaching_sum (sum - hd xs, tl xs)
		       
fun what_month(x: int)=
    let val  xs= [31,28,31, 30,31,30,31,31,30,31,30,31]
    in number_before_reaching_sum(x,xs) + 1
    end
	
fun month_range(x:int, y:int)=
    if x > y
    then []
    else
	if x=y
	then[what_month(y)]
	else what_month(x)::month_range(x+1,y)
				   
fun oldest(xs: (int*int*int) list)=
    if null xs
    then NONE
    else
	let
	    fun oldest2(xs: (int*int*int) list)=
		if null(tl xs)
		then hd xs
		else let val  old = oldest2(tl xs)
		     in
			 if #1 (hd xs) < #1 old
			 then hd xs
			 else if #1 (hd xs) = #1 old andalso #2 (hd xs) < #2 old
			 then hd xs
			 else if #1 (hd xs) = #1 old andalso #2 (hd xs) = #2 old andalso #3(hd xs) < #3 old						 
			 then hd xs
			 else old
		     end
	in SOME (oldest2 xs)
	end
	    
					  
			
