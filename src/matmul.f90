module mm
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
end module mm

program mul
   ! Vector4 tests
     use mm
     use sub
     integer nn
     real(8), dimension(5000,5000) :: a, b, c
     real    ::  T1,T2 
     real    ::  dt1, dt2, dt3 
     print *, 'Hello, matmul'
     print *, "         n", "      init","         parallel", "     no-parallel"
     do nn = 500, 2500, 100
     call cpu_time(T1) 
     CALL RANDOM_NUMBER(a)
     CALL RANDOM_NUMBER(b)    
     call cpu_time(T2)
     dt1 = T2-T1
     !print *, sum(c) 

     call cpu_time(T1) 
     call matmul(a,b,c,nn)
     call cpu_time(T2)
     dt2 = T2-T1
     !print *, sum(c)     

     call cpu_time(T1) 
     call matmul2(a,b,c,nn)
     call cpu_time(T2)
     dt3 = T2-T1
     !print *, sum(c)

     print *, nn, dt1, dt2, dt3
     end do
     print *,"end"
end program mul