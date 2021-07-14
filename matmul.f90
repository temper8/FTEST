module mm
   Contains   
   subroutine matmul(a,b,c,n)
      Real(8) a(n,n),b(n,n),c(n,n)
      c=0.d0
      do i=1,n         ! Outer loop is parallelized.
         do j=1,n      ! inner loops are interchanged
            do k=1,n   ! new inner loop is vectorized 
               c(j,i)=c(j,i)+a(k,i)*b(j,k)
            enddo
         enddo
      enddo
   end  subroutine  matmul   
end module mm

program mul
   ! Vector4 tests
     use mm
     PARAMETER n = 3000
     real(8), dimension(n,n) :: a, b, c
     real    ::  TT1,TT2 
     print *, 'Hello, matmul'
     call cpu_time(T1) 
     call matmul(a,b,c,n)
     call cpu_time(T2)
     print *, "time=" , T2-T1
     print *,"end"
end program mul