! **********************************************************************
module Gausets
use Types
use Utils
implicit none
private
public gauset
interface gauset
 module procedure wrapper_c, wrapper_z, wrapper_zv
end interface
contains
! **********************************************************************
subroutine wrapper_c(ak,as)
    real time1, time2
    complex(SPT),intent(inout)::ak(:,:),as(:)
    print *, 'gauset_c', size(ak), size(as)
    time1 = sys_time()
    call gauset_c(ak,as)
    time2 = sys_time() - time1
    print *, 'gauset_c exec time: ', time2
end subroutine wrapper_c

subroutine gauset_c(ak,as)
use LowLevLib, only: assert_eq,swap
implicit none
character(len=*),parameter::prname='gauset_c'
integer(I4B)::l,i,imax,j,jj,k
real(SPT),parameter::ZERO=0.0_SPT,ONE=1.0_SPT
complex(SPT),intent(inout)::ak(:,:),as(:)
complex(SPT),parameter::ZEROC=(ZERO,ZERO)
complex(SPT)::s
! ----------------------------------------------------------------------
l=assert_eq(size(ak,1),size(ak,2),size(as),prname)
call goutput (l,0)
OuterLoop:do j=1,l
 call goutput (l,j)
 s=ZEROC
 do i=j,l
  if (abs(s) < abs(ak(i,j))) then
   s=ak(i,j)
   imax=i
  end if
 end do
 if (abs(s) /= ZERO) then
  call swap (ak(imax,j:l),ak(j,j:l)) ; ak(j,j:l)=ak(j,j:l)/s
  call swap (as(imax),as(j))         ; as(j)=as(j)/s
 end if
 if (j == l) exit OuterLoop
 do i=j+1,l
  do k=j+1,l
   ak(i,k)=ak(i,k)-ak(i,j)*ak(j,k)
  end do
  as(i)=as(i)-ak(i,j)*as(j)
 end do
end do OuterLoop
if (abs(s) == ZERO) as(l)=ZEROC
do jj=1,l-1
 j=l-jj
 do k=j+1,l
  as(j)=as(j)-ak(j,k)*as(k)
 end do
end do
end subroutine gauset_c
! **********************************************************************
subroutine wrapper_z(ak,as)
    real time1, time2
    complex(DPT),intent(inout)::ak(:,:),as(:)
    print *, 'gauset_z', size(ak), size(as)
    time1 = sys_time()
    call gauset_z(ak,as)
    time2 = sys_time() - time1
    print *, 'gauset_z exec time: ', time2
end subroutine wrapper_z

subroutine gauset_z(ak,as)
use Constants, only: ZERO,ONE,ZEROC
use LowLevLib, only: assert_eq,swap
implicit none
character(len=*),parameter::prname='gauset_z'
integer(I4B)::l,i,imax,j,jj,k
complex(DPT),intent(inout)::ak(:,:),as(:)
complex(DPT)::s
! ----------------------------------------------------------------------
l=assert_eq(size(ak,1),size(ak,2),size(as),prname)
call goutput (l,0)
OuterLoop:do j=1,l
 call goutput (l,j)
 s=ZEROC
 do i=j,l
  if (abs(s) < abs(ak(i,j))) then
   s=ak(i,j)
   imax=i
  end if
 end do
 if (abs(s) /= ZERO) then
  call swap (ak(imax,j:l),ak(j,j:l)) ; ak(j,j:l)=ak(j,j:l)/s
  call swap (as(imax),as(j))         ; as(j)=as(j)/s
 end if
 if (j == l) exit OuterLoop
 do i=j+1,l
  do k=j+1,l
   ak(i,k)=ak(i,k)-ak(i,j)*ak(j,k)
  end do
  as(i)=as(i)-ak(i,j)*as(j)
 end do
end do OuterLoop
if (abs(s) == ZERO) as(l)=ZEROC
do jj=1,l-1
 j=l-jj
 do k=j+1,l
  as(j)=as(j)-ak(j,k)*as(k)
 end do
end do
end subroutine gauset_z
! **********************************************************************
subroutine wrapper_zv(ak,as)
    real time1, time2
    complex(DPT),intent(inout)::ak(:,:),as(:,:)
    print *, 'gauset_zv', size(ak), size(as)
    time1 = sys_time()
    call gauset_zv(ak,as)
    time2 = sys_time() - time1
    print *, 'gauset_zv exec time: ', time2
end subroutine wrapper_zv

subroutine gauset_zv(ak,as)
use Constants, only: ZERO,ONE,ZEROC
use LowLevLib, only: assert_eq,swap
implicit none
character(len=*),parameter::prname='gauset_zv'
integer(I4B)::l,i,imax,j,jj,k
complex(DPT),intent(inout)::ak(:,:),as(:,:)
complex(DPT)::s
! ----------------------------------------------------------------------
l=assert_eq(size(ak,1),size(ak,2),size(as,1),prname)
call goutput (l,0)
OuterLoop:do j=1,l
 call goutput (l,j)
 s=ZEROC
 do i=j,l
  if (abs(s) < abs(ak(i,j))) then
   s=ak(i,j)
   imax=i
  end if
 end do
 if (abs(s) /= ZERO) then
  call swap (ak(imax,j:l),ak(j,j:l)) ; ak(j,j:l)=ak(j,j:l)/s
  call swap (as(imax,:),as(j,:))     ; as(j,:)=as(j,:)/s
 end if
 if (j == l) exit OuterLoop
 do i=j+1,l
  do k=j+1,l
   ak(i,k)=ak(i,k)-ak(i,j)*ak(j,k)
  end do
  as(i,:)=as(i,:)-ak(i,j)*as(j,:)
 end do
end do OuterLoop
if (abs(s) == ZERO) as(l,:)=ZEROC
do jj=1,l-1
 j=l-jj
 do k=j+1,l
  as(j,:)=as(j,:)-ak(j,k)*as(k,:)
 end do
end do
end subroutine gauset_zv
! **********************************************************************
subroutine goutput (ltot,jcurr)
use Constants, only: ONE
implicit none
integer(I4B),intent(in)::ltot,jcurr
integer(I4B),save::jprint
! ----------------------------------------------------------------------
if (jcurr == 0) then ! initialize jprint
 jprint=cfunc(ltot,0)
else if (jcurr == jprint) then ! make output and compute next jprint
 write(6,'(f7.2,2a,$)') 100*(ONE-(real(ltot-jcurr,kind=DPT)/ltot)**3),'%',char(13)
 if (jcurr < ltot) jprint=cfunc(ltot,jcurr)
 if (jprint > ltot) jprint=ltot
! write(2,*) jcurr,(real(ltot-jcurr)/ltot)**3-(real(ltot-jprint)/ltot)**3
end if
end subroutine goutput
! **********************************************************************
function cfunc (ltot,jcurr)
implicit none
integer(I4B),intent(in)::ltot,jcurr
integer(I4B)::cfunc,m
real(DPT),parameter::cprint=500.0_DPT
! ltot - total number of lines (unknowns); jcurr - current line number;
! This function computes the number of next line with output trying to
! keep the CPU time between the successive outputs more or less constant.
! m - number of lines left to process. For m > cprint output is performed
! after each line, i.e. cfunc = jcurr + 1.
! ----------------------------------------------------------------------
m=ltot-jcurr
cfunc=jcurr+(cprint/m)**2+1
end function cfunc
! **********************************************************************
end module Gausets
