program  xytab
   implicit none
	!constructs a table of z=x/y for values of x from 1 to 2 and 
	!y from 1 to 4 in  steps of .5
   real :: x, y, z 
   print *, '           x                y                z'

   x = 1.0
   do while (x <= 2.0)
      y = 1.0
      do while (y <= 4.0)
         z = x/y
         print *, x,y,z
         y = y + 0.5
         ! if (y > 3.5) goto 100
      end do
      x = x + 0.5
   end do
! 100  continue

end  program xytab