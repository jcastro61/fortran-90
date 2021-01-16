program collatz
   implicit none
   
   integer, parameter                 :: elems_number=20
   integer, dimension(0:elems_number) :: steps
   integer ::i, start=0, max_range=0, count_steps

   interface 
      subroutine printBarChart(a,i)
         integer, dimension(:) :: a
         integer,intent (in) :: i         
      end subroutine printBarChart
   end interface 

   write(*,"(a)",advance="no") "Enter starting number: "
   Read *,start
   print *," "
   
   do i=1,elems_number
      steps(i) = count_steps(start+i-1)
      if (steps(i) > max_range) then 
         max_range = steps(i) 
      end if
   end do

   call printBarChart (steps,max_range)
	
end program collatz
	
	! computes the number of steps to reach 1
function count_steps(n) result(step)
   implicit none
	! dummy arguments     
   integer step
   integer, intent (in) :: n 
   integer :: x
   x=n
   step=0
   do while (x /= 1)
      step = step+1
      if ( mod(x,2) == 0 ) then
         x = x / 2
      else
         x = (x*3) + 1
      end if
   enddo

end function count_steps

subroutine printBarChart(steps,max)
   implicit none
   
   integer,dimension (:) :: steps
   integer, intent (in) :: max
   integer::i, arraySize, max_range=0;
   arraySize = size(steps)
      
   max_range=max
   do while (max_range > 0)
      do i=1,arraySize
         if( steps(i) >= max_range ) then
            write(*,"(a)",advance="no") "##"
         else 
            write(*,"(a)",advance="no") "  "
         end if
      end do
      print *," "
      max_range = max_range - 1
   enddo 
      
end subroutine printBarChart