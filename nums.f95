module nums
   type number
      real :: n
   end type number
 
   type number_parent
      type(number), pointer :: p
   contains
      procedure, pass(this) :: get_number
   end type number_parent
 
 contains

   function get_number(this) result(n)
      class(number_parent) :: this
      type(number), allocatable, target :: n
      allocate(n)
      n % n = 1.0
      this % p => n
   end function get_number
 
 end module nums
 
 program main
   use nums
   implicit none
 
   type(number) :: n
   type(number_parent) :: np
 
   n = np%get_number()
 
   print *, n%n
   print *, np%p%n
 
   n%n = n%n + 1.0
 
   print *, n%n
   print *, np%p%n
 
 end program main