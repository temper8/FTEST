PROGRAM timer
	IMPLICIT NONE
	REAL :: t1,t2,rate 
	INTEGER :: c1,c2,cr,cm,i,j,n,s
	INTEGER :: x, y, count
	INTEGER , PARAMETER :: runs=10000
	REAL, dimension (:,:), allocatable :: array
  
	! First initialize the system_clock
	CALL system_clock(count_rate=cr)
	CALL system_clock(count_max=cm)
	rate = REAL(cr)
	WRITE(*,*) "system_clock rate ",rate
  
	do count = 300,1000,100
	x = count
	y = count
	allocate ( array(x,y) ) 	
	CALL CPU_TIME(t1)
	CALL SYSTEM_CLOCK(c1)	
	DO n = 1 , runs
	   !DIR$ PARALLEL  ALWAYS	
	   FORALL(i = 1:x,j = 1:y)
		  array(i,j) = REAL(i)*REAL(j) + 2
	   END FORALL
	   array(1,1) = array(1,2)     
	END DO   
	CALL CPU_TIME(t2)
	CALL SYSTEM_CLOCK(c2)	
	print *,x, x*y
	WRITE(*,*) "system_clock : ",(c2 - c1)/rate
	WRITE(*,*) "cpu_time     : ",(t2-t1)

	CALL CPU_TIME(t1)
	CALL SYSTEM_CLOCK(c1)	
	DO n = 1 , runs
	   !DIR$ NOPARALLEL	
	   FORALL(i = 1:x,j = 1:y)
		  array(i,j) = REAL(i)*REAL(j) + 2
	   END FORALL
	   array(1,1) = array(1,2)     
	END DO   
	CALL CPU_TIME(t2)
	CALL SYSTEM_CLOCK(c2)	
	print *, 'NOPARALLEL'
	WRITE(*,*) "system_clock : ",(c2 - c1)/rate
	WRITE(*,*) "cpu_time     : ",(t2-t1)
	print *,'-----------------'
	deallocate (array)
	end do
  END PROGRAM timer