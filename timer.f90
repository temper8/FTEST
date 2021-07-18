PROGRAM timer
	IMPLICIT NONE
	REAL :: t1,t2,rate 
	INTEGER :: c1,c2,cr,cm,i,j,n,s
	INTEGER , PARAMETER :: x=500,y=500,runs=10000
	REAL :: array(x,y),a_diff,diff
  
	! First initialize the system_clock
	CALL system_clock(count_rate=cr)
	CALL system_clock(count_max=cm)
	rate = REAL(cr)
	WRITE(*,*) "system_clock rate ",rate
  
	CALL CPU_TIME(t1)
	CALL SYSTEM_CLOCK(c1)	
	DO n = 1 , runs
	   FORALL(i = 1:x,j = 1:y)
		  array(i,j) = REAL(i)*REAL(j) + 2
	   END FORALL
	   array(1,1) = array(1,2)     
	END DO   
	CALL CPU_TIME(t2)
	CALL SYSTEM_CLOCK(c2)	

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

  END PROGRAM timer