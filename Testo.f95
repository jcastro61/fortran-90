PROGRAM Testo
   IMPLICIT NONE
   INTEGER, PARAMETER :: ncelMAX=6800
   REAL*8  :: dt
   LOGICAL :: MagON,BFmesh
   INTEGER :: nslice_Ver, nslice_verzr, nslice_Edg, nslice_edgzr
   INTEGER ::  nFluid, nChrge, nCel, nBCs, nEdg, nVer , nCelzr, nBCszr, nEdgzr, &
               nVerzr, MinSliceCel, MaxSliceCel, MinSliceCelzr, MaxSliceCelzr
   TYPE cell_obj
      INTEGER*4 verNo(4),edgNo(4)
      INTEGER*4 Bln   !BField Mesh
      REAL*8 f_ei,f_en,f_egyro,f_wall,f_ex, f_wallBC
   END TYPE cell_obj
   TYPE(cell_obj) cel(ncelMAX)

   dt = 2.0E-8
   MagON = .TRUE.
   BFmesh = .TRUE.
   nslice_Ver = 1
   nslice_verzr = 2
   nslice_Edg = 3
   nslice_edgzr = 4
   nFluid = 3
   nChrge = 3
   nCel = 3000
   nBCs = 4000
   nEdg = 5000
   nVer = 6000
   nCelzr = 7000
   nBCszr = 8000
   nEdgzr = 9000
   nVerzr = 10000
   MinSliceCel = 100
   MaxSliceCel = 200
   MinSliceCelzr = 300
   MaxSliceCelzr = 400
   cel(1)%verNo(1) = 1
   cel(1)%verNo(2) = 2
   cel(1)%verNo(3) = 3
   cel(1)%verNo(4) = 4
   cel(1)%edgNo(1) = 1
   cel(1)%edgNo(2) = 2
   cel(1)%edgNo(3) = 3
   cel(1)%edgNo(4) = 4
   cel(1)%Bln = 1000
   cel(1)%f_ei = 2.0E6
   cel(1)%f_en = 2.0E7
   cel(1)%f_egyro = 2.0E8
   cel(1)%f_wall = 3.0E6
   cel(1)%f_ex = 3.0E7
   cel(1)%f_wallBC = 3.0E8
	
   OPEN(UNIT=10,FILE='restartData_TEST',FORM='UNFORMATTED')
   REWIND(10)
   WRITE(10) dt, MagON, nslice_Ver, nslice_Edg, nslice_verzr, nslice_edgzr, BFmesh
   WRITE(10) nFluid, nChrge, nCel, nBCs, nEdg, nVer, & 
      nCelzr, nBCszr, nEdgzr, nVerzr, &
      MinSliceCel, &
      MaxSliceCel, &
      MinSliceCelzr, &
      MaxSliceCelzr
   WRITE(10) cel
   CLOSE(10)
	
   OPEN(UNIT=10,FILE='restartData_TEST',FORM='UNFORMATTED')
   REWIND(10)
   READ(10) dt,MagON,nslice_Ver,nslice_Edg, nslice_verzr, nslice_edgzr, BFmesh
   PRINT *, "First line = ", dt,MagON,nslice_Ver,nslice_Edg, nslice_verzr, nslice_edgzr, BFmesh
   READ(10) nFluid, nChrge, nCel, nBCs, nEdg, nVer , nCelzr, nBCszr, nEdgzr, nVerzr, MinSliceCel, & 
      MaxSliceCel,MinSliceCelzr,MaxSliceCelzr
   PRINT *, "Second line = ", nFluid, nChrge, nCel, nBCs, nEdg, nVer , nCelzr, nBCszr, nEdgzr, & 
      nVerzr, MinSliceCel, MaxSliceCel,MinSliceCelzr,MaxSliceCelzr
   READ(10) cel
   PRINT *, "cel(1), verNo(4),edgNo(4) = ",cel(1)%verNo(1),cel(1)%verNo(2),cel(1)%verNo(3), &
      cel(1)%verNo(4),cel(1)%edgNo(1),cel(1)%edgNo(2),cel(1)%edgNo(3),cel(1)%edgNo(4)
   PRINT *, "cel(1), Bln = ",cel(1)%Bln
   PRINT *, "cel(1), f_ei,f_en,f_egyro,f_wall,f_ex,f_wallBC = ",cel(1)%f_ei,cel(1)%f_en, &
      cel(1)%f_egyro,cel(1)%f_wall,cel(1)%f_ex,cel(1)%f_wallBC
   ! PAUSE 'DONE'
 END PROGRAM Testo