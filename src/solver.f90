program solver
    use Utils
    use Types
    use MKL_wrapper
    use gausets
    implicit none
    integer NMAX 
    PARAMETER        ( NMAX = 10000 )
    complex(8), dimension(:,:), allocatable :: RA, RB

	complex(8), dimension (:,:), allocatable :: MA
	complex(8), dimension (:,:), allocatable :: MB	    

	complex(DPT), dimension (:,:), allocatable :: A
	complex(DPT), dimension (:,:), allocatable :: B	
    
	integer, dimension( 4 )  ::	ISEED

    CHARACTER(len=32) :: arg
    integer stat
    integer N
    
    real time_init, time_mkl, time_gauset
    real T1,T2
    print *, 'Solve the complex equations A*X = B.'
    print *, 'and calc exec time'
    CALL getarg(1, arg)
    READ(arg,*,iostat=stat)N
    if (stat<0) then
       print *, "set default size"
       N = 4
    end if
    print *, "N=", N 

    allocate ( RA(NMAX, NMAX) )
    allocate ( RB(NMAX, 1) )

    call random_z(NMAX*NMAX,	RA)
    call random_z(NMAX,	RB)

    MA = RA(1:N, 1:N)
    MB = RB(1:N, 1:1)

    time_init = sys_time()
    call cpu_time(T1) 

    call mkl_le_solver(MA,MB)

    call cpu_time(T2)
    time_mkl = sys_time() - time_init

    print *, 'mkl_le_solver exec time',  time_mkl, T2-T1
    print *, "---------"


    A = RA(1:N, 1:N)
    B = RB(1:N, 1:1)
    time_init = sys_time()
    call gauset (A,B)
    time_gauset = sys_time() - time_init
  
    print *, "---------"

    print *,'Err sum: ', SUM(CDABS(B-MB))
    print *,'Time ratio: ',time_gauset/time_mkl

end program  solver