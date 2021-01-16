!/************************************************************************
!Largest prime factor
!Problem 3
!
!The prime factors of 13195 are 5, 7, 13 and 29.
!What is the largest prime factor of the number 600851475143 ? (6857)         
!*************************************************************************/
program largestprime  
   implicit none
   integer(kind=8) :: n=0,maxfactor=0;
   integer(kind=8) :: num = 600851475143_8;
   !integer(kind=8) :: num = 13195;
   logical   :: is_prime, is_factor;

   do n=1,int(sqrt(real(num)))
      if ( (is_factor(num,n) .eqv. .true.) .and. ( is_prime(n) .eqv. .true. ) ) then
         print *,n, " is a factor of ",num," and is prime "
         if (n > maxfactor) then 
            maxfactor = n 
         end if
      end if
   end do
   
   print *
   print *,"Largest prime factor of ",num,"is ",maxfactor

end program largestprime

function is_factor(n,f) result(bfactor)
   implicit none; 
   integer(kind=8), intent (in) :: n,f;  
   logical         :: bfactor 

   bfactor = .false.
   if ( mod(n,f) == 0 ) then 
      bfactor = .true. 
   end if
end function is_factor

function is_prime(n) result(bPrime)
   implicit none

   integer(kind=8), intent (in) :: n
   logical         :: bPrime;
   integer(kind=8) :: m,i
	
   bprime = .true.;
   !m = n / 2;
   m = int8(sqrt(real(n)))
   if (n == 1 .or. n == 2 .or. n == 3) bprime = .true.

   do  i = 2,m
      if ( mod(n,i) == 0) then  
         bPrime = .false.
         exit;  
      end if
   end do
end function is_prime