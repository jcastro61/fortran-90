program power
   implicit none
   integer :: r
	
   write (*,*) "r=", Pow10(23)

CONTAINS
FUNCTION  Pow10(n) result(v)
   implicit NONE
   INTEGER, intent(in) :: n
   integer :: v
   integer :: num 
   v=0
   num = n; 
   do while (num /=0)
      v = v+1
      num =  num/10
   end do
END FUNCTION

end program power