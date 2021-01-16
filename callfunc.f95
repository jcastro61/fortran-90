program calling_func

   real :: a, r
	
   a = 2.0
   do while (a < 3.0)
      r = area_of_circle(a) 
      Print *, "The area of a circle with radius ", a, " is", r
      a = a + 0.1
   end do 
	
end program calling_func


! this function computes the area of a circle with radius r  
function area_of_circle (r)  
implicit none      

   ! dummy arguments        
   real :: area_of_circle   
   
   ! local variables 
   real :: r     
   real :: pi
   
   pi = 4 * atan (1.0)     
   area_of_circle = pi * r**2  
   
end function area_of_circle