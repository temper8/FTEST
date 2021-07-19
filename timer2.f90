module test
	Contains   
	PURE subroutine test_f(nn)
	 implicit none
	   integer, intent (in) :: nn
	   Real arr(nn,nn)
	   integer i,j,x,y,n
	   INTEGER , PARAMETER :: runs=10000
	   x = nn
	   y = nn
	   DO n = 1 , runs
		  !DIR$ PARALLEL  ALWAYS	
		  FORALL(i = 1:x,j = 1:y)
			 arr(j,i) = REAL(i)*REAL(j) + 2
		  END FORALL
		  arr(1,1) = arr(1,2)     
	   END DO

	end    
 end module test

PROGRAM timer2
	use test
	IMPLICIT NONE
	REAL :: t1,t2,rate 
	real sys_clock(4), cpu_time_(4)
	INTEGER :: c1,c2,cr,cm,i,j,n,s
	INTEGER :: x, y, count
	INTEGER , PARAMETER :: runs=10000
	REAL, dimension (:,:), allocatable :: array
  	!dir$ attributes align:64 ::array
	! First initialize the system_clock
	CALL system_clock(count_rate=cr)
	CALL system_clock(count_max=cm)
	rate = REAL(cr)
	WRITE(*,*) "system_clock rate ",rate
  
	do count = 100,1500,100
		x = count
		y = count
		
		allocate ( array(x,y) )
		CALL CPU_TIME(t1)
		CALL SYSTEM_CLOCK(c1)

		call test_f(count)

		CALL CPU_TIME(t2)
		CALL SYSTEM_CLOCK(c2)	

		sys_clock(1) = (c2 - c1)/rate
		cpu_time_(1) = (t2-t1)
 	
	CALL CPU_TIME(t1)
	CALL SYSTEM_CLOCK(c1)	
	DO n = 1 , runs

	!DIR$ PARALLEL  ALWAYS
	   do i = 1,x 
		do j = 1,y
		  array(j,i) = REAL(i)*REAL(j) + 2
		end do
		end do	
	   array(1,1) = array(1,2)     
	END DO   
	CALL CPU_TIME(t2)
	CALL SYSTEM_CLOCK(c2)	

	sys_clock(2) = (c2 - c1)/rate
	cpu_time_(2) = (t2-t1)

	CALL CPU_TIME(t1)
	CALL SYSTEM_CLOCK(c1)	
	DO n = 1 , runs
	   !DIR$ 	NOPARALLEL
		do i = 1,x 
			do j = 1,y
			  array(j,i) = REAL(i)*REAL(j) + 2
			end do
		end do
	   array(1,1) = array(1,2)     
	END DO   
	CALL CPU_TIME(t2)
	CALL SYSTEM_CLOCK(c2)	

	sys_clock(3) = (c2 - c1)/rate
	cpu_time_(3) = (t2-t1)

	CALL CPU_TIME(t1)
	CALL SYSTEM_CLOCK(c1)	
	DO n = 1 , runs
	   !DIR$ 	NOPARALLEL
	   FORALL(i = 1:x,j = 1:y)
		  array(j,i) = REAL(i)*REAL(j) + 2
	   END FORALL
	   array(1,1) = array(1,2)     
	END DO   
	CALL CPU_TIME(t2)
	CALL SYSTEM_CLOCK(c2)	

	sys_clock(4) = (c2 - c1)/rate
	cpu_time_(4) = (t2-t1)

	print *, x, x*y, '         PARALLEL', '       NOPARALLEL', '     FORALL'	
	WRITE(*,*) "system_clock : ",sys_clock
	WRITE(*,*) "cpu_time     : ",cpu_time_
	print *,'-----------------'
	deallocate (array)
	end do
  END PROGRAM timer2