PROGRAM scale_analysis
    USE, INTRINSIC :: iso_fortran_env, ONLY: real32, real64, FILE_STORAGE_SIZE
    IMPLICIT NONE
    
    ! DECLARE PARAMETERS
    INTEGER, PARAMETER :: SP = real32       ! SINGLE-PRECISION REAL
    INTEGER, PARAMETER :: DP = real64       ! DOUBLE-PRECISION REAL
    INTEGER, PARAMETER :: nx = 1024         ! TOTAL NUMBER OF GRID POINTS
    INTEGER, PARAMETER :: iskip = 2
    
     
    ! DECLARE ALLOCATABLE ARRAYS
    REAL(SP), ALLOCATABLE, DIMENSION(:,:,:) :: STMP, T, RHO
    
    ! DECLARE VARIABLES FOR SUMMATION
    REAL(SP) :: rho_sum, rhoT_sum, T_sum
    
    ! DECLARE VARIABLES FOR REYNOLDS-AVERAGING
    REAL(SP) :: rho_avg, rhoT_avg, T_avg
    
    ! DECLARE VARIABLES FOR FAVRE-AVERAGING
    REAL(SP) :: T_favg
    
    INTEGER :: i, j, k
    
    CHARACTER(LEN=100) :: outfile, varname, infile4, infile5
    CHARACTER(LEN=100) :: filename
    
    !--------------------------------------------------------------------------
    ! DEMONSTRATE READ_DATA
    !--------------------------------------------------------------------------
    
    ALLOCATE(T(nx/iskip, nx/iskip, nx/iskip))
    ALLOCATE(RHO(nx/iskip, nx/iskip, nx/iskip))
    ALLOCATE(STMP(nx, nx, nx))

    STMP = 0.0_SP
    
    !--------------------------------------------------------------------------
    ! INITIALIZATION OF SUMMATION VARIABLES
    !--------------------------------------------------------------------------
    rho_sum = 0.0;     rhoT_sum = 0.0;     
    T_sum = 0.0;        
    T_avg = 0.0;        T_favg = 0.0;
    
    !--------------------------------------------------------------------------
    ! DATA IMPORT -START
    !--------------------------------------------------------------------------
    varname = 'Z1_dil_inertHIT'
    outfile = 'terms_in_Kolla(Z1_inertHIT).txt'
    infile4 = 'E:\AUTOIGNITION\Z1\Temperature_inertHIT.bin'
    infile5 = 'E:\AUTOIGNITION\Z1\Density_inertHIT.bin'
    
    
    
    OPEN(44, file=TRIM(ADJUSTL(infile4)), status='old', access='stream', &
         form='unformatted')
    READ(44) stmp
    CLOSE(44)
    DO k = 1, nx/iskip
        DO j = 1, nx/iskip
            DO i = 1, nx/iskip
                T(i,j,k) = stmp(1+iskip*(i-1), 1+iskip*(j-1), 1+iskip*(k-1)) 
            END DO
        END DO
    END DO
    WRITE(*, '(A)') 'Data4 is successfully read ... '
    
    OPEN(55, file=TRIM(ADJUSTL(infile5)), status='old', access='stream', &
         form='unformatted')
    READ(55) stmp
    CLOSE(55)
    DO k = 1, nx/iskip
        DO j = 1, nx/iskip
            DO i = 1, nx/iskip
                RHO(i,j,k) = stmp(1+iskip*(i-1), 1+iskip*(j-1), 1+iskip*(k-1)) 
            END DO
        END DO
    END DO
    WRITE(*, '(A)') 'Data5 is successfully read ... '
    
    DEALLOCATE(stmp)
    
    !--------------------------------------------------------------------------
    ! COMPUTE U_RMS
    !--------------------------------------------------------------------------

    DO k = 1, nx/iskip
        DO j = 1, nx/iskip
            DO i = 1, nx/iskip
                rho_sum = rho_sum + RHO(i,j,k)
            END DO
        END DO
    END DO

    rho_avg = rho_sum / REAL((nx/iskip)**3)

    
    !--------------------------------------------------------------------------
    ! COMPUTE TEMPERATRUE TAYLOR LENGTH SCALE(LAMBDA_T)
    ! C.TOWERY, DETONATION INITIATION BY COMPRESSIBLE TURBULENCE THERMODYNAMIC
    ! FLUCTUATIONS, CNF, 2019
    !--------------------------------------------------------------------------

    DO k = 1, nx/iskip
        DO j = 1, nx/iskip
            DO i = 1, nx/iskip
                T_sum = T_sum + T(i,j,k)
                rhoT_sum = rhoT_sum + ( RHO(i,j,k)*T(i,j,k) )
            END DO
        END DO
    END DO
    
    T_avg = T_sum / REAL((nx/iskip)**3)
    rhoT_avg = rhoT_sum / REAL((nx/iskip)**3)
    T_favg = rhoT_avg / rho_avg
    
    DO k = 1, nx/iskip
        DO j = 1, nx/iskip
            DO i = 1, nx/iskip
                rhoT_sum = rhoT_sum + RHO(i,j,k)*( (T(i,j,k) - T_favg)**2 )
            END DO
        END DO
    END DO
    
    rhoT_avg = rhoT_sum / REAL((nx/iskip)**3)
   
    T_avg = SQRT( rhoT_avg / rho_avg )
    
    write(*,*) T_favg

    write(*,*) T_avg,T_favg

END PROGRAM scale_analysis