
program test_matmul
    use utils
    use sub
    integer nn
    real(8), dimension(5000,5000) :: a, b, c
    real all_time1, all_time2
    real time_init
    real time21, time22
    real    ::  T1,T2 
    real    ::  dt1, dt2, dt3 
    all_time1 = sys_time()
    print *, 'Test matmul'
    call cpu_time(T1) 
    CALL RANDOM_NUMBER(a)
    CALL RANDOM_NUMBER(b)    
    call cpu_time(T2)
    dt1 = T2-T1
    print *, "init time = ", dt1         
    print *, "         n","   cpu time", "         sys time", "      cpu/sys time"    
    do nn = 500, 2500, 200
        time_init = sys_time()
        call cpu_time(T1) 
        call matmul(a,b,c,nn)
        call cpu_time(T2)
        time_matmul = sys_time() - time_init
        dt2 = T2-T1
    
        print *, nn, dt2,  time_matmul, dt2/time_matmul
    end do
    all_time2 = sys_time()
    print *,"all time =", all_time2 - all_time1

end program test_matmul