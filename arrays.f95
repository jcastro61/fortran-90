                  
program arrays
   implicit none

   integer, parameter              :: elems_number=20
   real, parameter                 :: delta=1.0/elems_number
   real, dimension(0:elems_number) :: abscissa
   real, dimension(0:elems_number) :: temperature
   integer                         :: e

   print "(A)", "Sleeping for 5"
   call sleep(5)

! array computations
! array constructor
   abscissa = [(delta*e, e=0, elems_number)]

! elemental function operating on each array element
   temperature = sin(abscissa)/2.0

   print "(A)", "Abscissa, Temperature"
   do e=0, elems_number
      print "(E15.6, 1X, E15.6)", abscissa(e), temperature(e)
   enddo

end program  arrays