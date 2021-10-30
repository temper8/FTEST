module util
   Contains   
   subroutine matmul2(a,b,c,n)
    implicit none
      Real(8) a(n,n),b(n,n),c(n,n)
      integer i,j,k,n
      c=0.d0
      !DIR$ NOPARALLEL
      do i=1,n         ! Outer loop is parallelized.
         do j=1,n      ! inner loops are interchanged
            do k=1,n   ! new inner loop is vectorized 
               c(j,i)=c(j,i)+a(k,i)*b(j,k)
            enddo
         enddo
      enddo
   end    

   function sys_time()
      implicit none
      real(8) sys_time
      integer count, count_rate, count_max
      call system_clock(count, count_rate, count_max)
      sys_time = count*1.0/count_rate
      return
   end   

end module util

program mul
   ! Vector4 tests
     use util
     use sub
     integer nn
     real(8), dimension(5000,5000) :: a, b, c
     real all_time1, all_time2
     real time11, time12
     real time21, time22
     real    ::  T1,T2 
     real    ::  dt1, dt2, dt3 
     all_time1 = sys_time()
     print *, 'Hello, matmul'
     print *, "         n", "      init","         parallel", "     no-parallel"
     do nn = 500, 2500, 100
     call cpu_time(T1) 
     CALL RANDOM_NUMBER(a)
     CALL RANDOM_NUMBER(b)    
     call cpu_time(T2)
     dt1 = T2-T1
     !print *, sum(c) 

     time11 = sys_time()
     call cpu_time(T1) 
     call matmul2(a,b,c,nn)
     call cpu_time(T2)
     time12 = sys_time()
     dt2 = T2-T1
     !print *, sum(c)     

     time21 = sys_time()
     call cpu_time(T1) 
     call matmul(a,b,c,nn)
     call cpu_time(T2)
     time22 = sys_time()
     dt3 = T2-T1
     !print *, sum(c)

     print *, nn, dt1, dt2, dt3
     print *, 0, 1, time12-time11, time22-time21
     end do
     all_time2 = sys_time()
     print *,"all time =", all_time2 - all_time1

end program mul