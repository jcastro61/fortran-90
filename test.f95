program test
   implicit none

   double precision:: c1,c2,rate
   integer::ci,cj,cr,cm,i
   integer,parameter::max_iter=10000000 !10^7

   c1=0.0d+0
   c2=0.0d+0

   CALL system_clock(count_rate=cr)
   CALL system_clock(count_max=cm)
   rate = REAL(cr)

   CALL SYSTEM_CLOCK(ci)
   do i=1,max_iter
      c1=c1+log(DBLE(i))
      c2=c2+log(dble(i))
   end do
   CALL SYSTEM_CLOCK(cj)
   WRITE(*,*) "system_clock : ",(cj - ci)/rate

   print*, c1
end program test