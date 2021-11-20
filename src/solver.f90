program solver
    use Utils
    use Types
    use le_solver
    use gausets
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
    integer nn

    real all_time1, all_time2
    real time_init, time_mkl
    real time21, time22
    real    ::  T1,T2 
    real    ::  dt1, dt2, dt3 

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

    all_time1 = sys_time()
    print *, 'Init matrix'
    ISEED = (/ 1, 2, 3, 4 /)
    call cpu_time(T1) 
    call zlarnv	(1, ISEED, NMAX*NMAX,	RA )	
	call zlarnv	(1, ISEED, NMAX,	RB )  
    call cpu_time(T2)
    dt1 = T2-T1
    print *, "init matrix time = ", dt1         

    MA = RA(1:N, 1:N)
    MB = RB(1:N, 1:1)

    time_init = sys_time()
    call cpu_time(T1) 

    call mkl_le_solver(MA,MB)

    call cpu_time(T2)
    time_mkl = sys_time() - time_init

    print *, 'mkl_le_solver exec time',  time_mkl
    print *, "---------"


    A = RA(1:N, 1:N)
    B = RB(1:N, 1:1)
    call gauset (A,B)
  
    print *, "---------"

    print *,'Err sum: ', SUM(CDABS(B-MB))

end program  solver