!/********************************************************************************************
!    Multiples of 3 and 5
!
!    Problem 1
!
!    If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 
!    3, 5, 6 and 9. The sum of these multiples is 23.
!
!    Find the sum of all the multiples of 3 or 5 below 1000. (233168)
!*********************************************************************************************/
program euler_001
    implicit none
    integer(kind=4) :: n=0;
    integer(kind=4) :: lim = 1000;
    integer(kind=4) :: sum =0;

    do n=1, lim -1
       if ( ( mod(n,3) .eq. 0 ) .OR. ( mod(n,5) .eq. 0)) then
            sum = sum + n  
       end if
    end do
    
    print *,"The sum of all the multiples of 3 or 5 below ",lim," is ", sum
 
end program euler_001