module sub
contains 
subroutine matmul(a,b,c,n)
	real(8) a(n,n),b(n,n),c(n,n)
	c=0.d0
	!DIR$ PARALLEL  ALWAYS
	do i=1,n         ! Outer loop is parallelized.
	   do j=1,n      ! inner loops are interchanged
		  do k=1,n   ! new inner loop is vectorized 
			 c(j,i)=c(j,i)+a(k,i)*b(j,k)
		  enddo
	   enddo
	enddo
end
end module sub