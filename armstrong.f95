! ---------------------------------------------------------------
! This program computes all Armstrong numbers in the range of
! 1 to given limit.  An Armstrong number is a number such that the sum
! of its digits raised to the third power is equal to the number
! itself.  For example, 371 is an Armstrong number, since
! 3**3 + 7**3 + 1**3 = 371.
! ---------------------------------------------------------------
module the_armstrong_worker
   contains
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

      subroutine calc_numbers(limit)
         IMPLICIT  NONE
         INTEGER, intent(in) :: limit ! upper limit
         INTEGER :: Count             ! a counter
         INTEGER :: num               ! armstrong num 
         INTEGER :: sum, rem, k, mag  ! summary, remainder, k, and magnitude
        
         Count = 0
         WRITE (*,*) 'List of Armstrong numbers from 1 to ', limit
	
         DO num=1, limit
            sum =0
            rem =0
            k = num
            mag = Pow10(num)
        
           do while (k /=0)             ! not eq zero
              rem=mod(k,10)
              sum = sum + rem**mag
              k=k/10
           END DO
		 
           IF (sum .EQ. num) THEN
               Count = Count + 1        
               WRITE(*,*) 'Armstrong number ', Count, ': ', num
           END IF
         END DO  
      end subroutine calc_numbers
endmodule the_armstrong_worker
	
program ArmstrongNumbers
   use the_armstrong_worker
      call calc_numbers(2000000)
end program ArmstrongNumbers
