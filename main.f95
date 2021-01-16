program main
   implicit none
 
   integer :: NN, KK, MM
   integer, allocatable, dimension(:,:) :: id
   complex*16, allocatable, dimension(:) :: phase
   complex*16 :: phase_base(3)
   real*8, allocatable, dimension(:,:) :: wave_base
 
   complex*16, allocatable, dimension(:,:) :: wave
   integer :: i, j
	
   NN = 1000
   KK = 200
   MM = 200
 
   allocate(id(MM,3))
   allocate(phase(KK))
   allocate(wave_base(KK, NN*(NN+1)/2 ))
   allocate(wave(NN, NN))
 
   id(:,:) = 2
   phase_base(:) = (1.0d0,1.0d0)
   wave_base(:,:) = 1.0d0
   phase(:) = (1.0d0,1.0d0)
 
   call  noise_wave(NN, KK, MM, id, phase, phase_base, wave_base, wave)
 
   do i=1,20
      do j=1,20
      print *,wave(i,j)
      end do 
   end do

   deallocate(id)
   deallocate(phase)
   deallocate(wave_base)
   deallocate(wave)
 
end program main
 
subroutine noise_wave(NN, KK, MM, id, phase_1, phase_base, wave_base, wave)
   implicit none
   integer, intent(in) :: NN, KK, MM
   integer, intent(in), dimension(MM, 3) :: id
   complex*16, intent(in) :: phase_1(KK)
   complex*16, intent(in) :: phase_base(3)
   real*8,  intent(in) :: wave_base(KK, NN*(NN+1)/2 )
   complex*16, intent(out) :: wave(NN, NN)
   integer :: i, j, k, p, n
   integer :: x, y, z
   complex*16 :: phase_2, phase_2_conjg
 
   do p = 1, MM
      x = id(p, 1)
      y = id(p, 2)
      z = id(p, 3)
 
      phase_2 = (phase_base(1) ** x) * (phase_base(2) ** y) * (phase_base(3) ** z)
      phase_2_conjg = conjg(phase_2)
 
      n = 0
      do j = 1, NN
         do i = 1, j   ! upper triangle
            n = n + 1
            do k = 1, KK
               wave(i,j) = wave(i,j) + wave_base(k,n) * phase_1(k) * phase_2_conjg
            end do
            wave(j,i) = conjg(wave(i,j) )
          end do
      end do
   end do
 
end subroutine noise_wave