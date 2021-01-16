module worker
contains

subroutine main
   implicit none 
   integer, parameter :: ARRAY_SIZE = 13
   real, dimension(:), allocatable :: numbers, roots

   allocate(numbers(ARRAY_SIZE))
   allocate(roots(ARRAY_SIZE))
   numbers = (/1,2,3,4,5,6,7,8,9,10,12,14,17/)
	
   call set_array(a=numbers, b=roots)
   call print_array(numbers)
   call print_array(roots)
	
end subroutine main

subroutine set_array(a, b)
   implicit none
   real, dimension(:), intent(in)  :: a
   real, dimension(:), intent(out) :: b
   integer :: i=0;

   do i=1,size(a)
      b(i) = sqrt(a(i))
   end do 

end subroutine set_array

subroutine print_array(a)
   implicit none
   real, dimension(:), intent(in) :: a
   integer :: i=0;

   do i=1,size(a)
      print *, a(i) 
   end do 
	
end subroutine print_array

end module worker

program loops
   use worker
   call main()
end program loops