module MKL_wrapper
    Contains   
	subroutine PRINT_MATRIX( DESC, M, N, A, LDA )
        CHARACTER*(*)    DESC
        INTEGER          M, N, LDA
        INTEGER          M1, N1
        COMPLEX          A( LDA, * )
      
        INTEGER          I, J
      
        WRITE(*,*)
        WRITE(*,*) DESC
        M1 = M
        N1 = N
        if (M1>5) M1 = 5
        if (N1>5) N1 = 5  
        DO I = 1, M1
           WRITE(*,9998) ( A( I, J ), J = 1, N1 )
        END DO
        
  9998 FORMAT( 11(:,1X,'(',F6.2,',',F6.2,')') )
      end  
 
    subroutine random_z(N, Mat)
    !
    !   Generate Random matrix Z
    !
        implicit none
        integer N
        complex(8), dimension (*) :: Mat

        integer, dimension( 4 )  ::	ISEED
        real T1,T2
        print *, 'Init random matrix ', N
        ISEED = (/ 1, 2, 3, 4 /)
        call cpu_time(T1) 
        call zlarnv	(1, ISEED, N, mat)	
        call cpu_time(T2)
        print *, "init matrix time = ", T2-T1         
        print *


    end subroutine random_z

    subroutine mkl_le_solver(A, B)
    !
    ! Solve the complex equations A*X = B.
    !
    !     .. Parameters ..
	    INTEGER          N, NRHS
	    PARAMETER        ( NRHS = 1 )
	    INTEGER          LDA, LDB

    !     .. Local Scalars ..
	    INTEGER          INFO
	
    !     .. Local Arrays ..
	    INTEGER, dimension (:), allocatable ::   IPIV
	    complex(8), dimension (:,:) :: A
	    complex(8), dimension (:,:) :: B	

	    integer, dimension( 4 )  ::	ISEED 

        N = size(B)
        !print *, N
        LDA = N
        LDB = N 
   
        ISEED = (/ 1, 2, 3, 4 /)

        allocate ( IPIV(N) )

        !CALL PRINT_MATRIX( 'A matrix', N, N, A, LDA )

        !Solve the equations A*X = B.
        CALL ZGESV ( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
    
        IF( INFO.GT.0 ) THEN
            WRITE(*,*)'The diagonal element of the triangular factor of A,'
            WRITE(*,*)'U(',INFO,',',INFO,') is zero, so that'
            WRITE(*,*)'A is singular; the solution could not be computed.'
            STOP
        END IF

    end subroutine mkl_le_solver

 end module MKL_wrapper