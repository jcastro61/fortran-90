program factorial
   implicit none
   
   integer (kind=8) :: i, f, myfactorial
   i = 0
	
   do i=0,15
      f = myfactorial(i)
      Print *, "The value of factorial ", i, " is ", f
   end do
		
end program factorial
	
	! computes the factorial of n (n!)      
recursive function myfactorial (n) result(fac)
	! function result     
   implicit none
	! dummy arguments     
   integer (kind=8) :: fac
   integer (kind=8), intent (in) :: n     
		
   select case (n)         
      case (0:1)         
         fac = 1         
      case default    
         fac = n * myfactorial (n-1)  
   end select 
		
end function myfactorial