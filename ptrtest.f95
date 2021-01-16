program test
   type ptr
      double precision, pointer :: p(:) => null()
   end type ptr

   integer :: i, n=5
   type(ptr), dimension(3) :: ptrs
   double precision, pointer :: a(:), b(:), c(:)

   ptrs(1)%p => a
   ptrs(2)%p => b
   ptrs(3)%p => c

   do i=1,3
      allocate(ptrs(i)%p(n))
      ptrs(i)%p = 0d0
   enddo

   write(6, *) ptrs(1)%p
   write(6, *) a

end program test
