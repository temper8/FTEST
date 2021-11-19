program test_solver
    use Utils
    use Types
    use le_solver
    use gausets
    integer NMAX 
    PARAMETER        ( NMAX = 10000 )
    complex, dimension(NMAX,NMAX) :: RA, RB
    
	complex(DPT), dimension (:,:), allocatable :: A
	complex(DPT), dimension (:,:), allocatable :: B	
    
	integer, dimension( 4 )  ::	ISEED

    CHARACTER(len=32) :: arg
    integer stat
    integer nn

    real all_time1, all_time2
    real time_init
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


    
    !allocate ( A(N, N) )
    !allocate ( B(N, 1) )
    

    all_time1 = sys_time()
    print *, 'Init matrix'
    ISEED = (/ 1, 2, 3, 4 /)
    call cpu_time(T1) 
    call clarnv	(1, ISEED, NMAX*NMAX,	RA )	
	call clarnv	(1, ISEED, NMAX,	RB )  
    
    print *, RB(1:N, 1:1)
    !call clarnv	(1, ISEED, N*N,	A )	
	!call clarnv	(1, ISEED, N,	B )  
    
    
    call cpu_time(T2)
    dt1 = T2-T1
    print *, "init matrix time = ", dt1         
    print *, "         n","   cpu time", "         sys time", "      cpu/sys time"    

    do nn = 500, 5000, 500
        N = nn
        A = RA(1:N, 1:N)
        B = RB(1:N, 1:1)
        !print *, RB(1:N, 1:1)
        time_init = sys_time()
        call cpu_time(T1) 

        !call mkl_le_solver(A,B)
        call gauset (A,B)

        call cpu_time(T2)
        time_matmul = sys_time() - time_init
        dt2 = T2-T1
    
        print *, nn, dt2,  time_matmul, dt2/time_matmul

       ! print *, RB(1:N, 1:1)
    end do
    all_time2 = sys_time()
    print *,"all time =", all_time2 - all_time1

end program test_solver