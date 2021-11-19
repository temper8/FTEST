! **********************************************************************
module LowLevLib
use Constants
use ErrMan
use Types
implicit none

private
public arth,assert,assert_eq,outerand,outerprod,swap,kron
public UniformGridVerification,realloc,locate
integer(I4B), parameter :: NPAR_ARTH=16,NPAR2_ARTH=8
interface arth
 module procedure arth_i,arth_d
end interface
interface assert
 module procedure assert1,assert2,assert3,assert_v
end interface
interface assert_eq
 module procedure assert_eq2,assert_eq3,assert_eq4,assert_eqn
end interface
interface outerprod
 module procedure outerprod_d,outerprod_z
end interface
interface swap
 module procedure swap_d,swap_c,swap_z,swap_dv,swap_cv,swap_zv
end interface
interface realloc
 module procedure realloc_dv,realloc_dm,realloc_iv,realloc_im,realloc_str
end interface
contains
! **********************************************************************
function arth_i (first,increment,n)
implicit none
integer(I4B),intent(in)::first,increment,n
integer(I4B),dimension(n)::arth_i
integer(I4B)::k,k2,temp
! ----------------------------------------------------------------------
if (n > 0) arth_i(1)=first
if (n <= NPAR_ARTH) then
 do k=2,n
  arth_i(k)=arth_i(k-1)+increment
 end do
else
 do k=2,NPAR2_ARTH
  arth_i(k)=arth_i(k-1)+increment
 end do
 temp=increment*NPAR2_ARTH
 k=NPAR2_ARTH
 do
  if (k >= n) exit
  k2=k+k
  arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
  temp=temp+temp
  k=k2
 end do
end if
end function arth_i
! **********************************************************************
function arth_d (first,increment,n)
implicit none
real(DPT),intent(in)::first,increment
integer(I4B),intent(in)::n
real(DPT), dimension(n)::arth_d
integer(I4B)::k,k2
real(DPT)::temp
! ----------------------------------------------------------------------
if (n > 0) arth_d(1)=first
if (n <= NPAR_ARTH) then
 do k=2,n
  arth_d(k)=arth_d(k-1)+increment
 end do
else
 do k=2,NPAR2_ARTH
  arth_d(k)=arth_d(k-1)+increment
 end do
 temp=increment*NPAR2_ARTH
 k=NPAR2_ARTH
 do
  if (k >= n) exit
  k2=k+k
  arth_d(k+1:min(k2,n))=temp+arth_d(1:min(k,n-k))
  temp=temp+temp
  k=k2
 end do
end if
end function arth_d
! **********************************************************************
subroutine assert1 (n1,string)
implicit none
character(len=*),intent(in)::string
logical,intent(in)::n1
character(len=*),parameter::prname='assert1'
character(len=255)::errmessage
! ----------------------------------------------------------------------
if (.not. n1) then
 write(errmessage,'(a)') 'assertion failed in: '//string
 call ferror (prname,trim(errmessage))
end if
end subroutine assert1
! **********************************************************************
subroutine assert2 (n1,n2,string)
implicit none
character(len=*),intent(in)::string
logical,intent(in)::n1,n2
character(len=*),parameter::prname='assert2'
character(len=255)::errmessage
! ----------------------------------------------------------------------
if (.not. (n1 .and. n2)) then
 write(errmessage,'(a)') 'assertion failed in: '//string
 call ferror (prname,trim(errmessage))
end if
end subroutine assert2
! **********************************************************************
subroutine assert3 (n1,n2,n3,string)
implicit none
character(len=*),intent(in)::string
logical,intent(in)::n1,n2,n3
character(len=*),parameter::prname='assert3'
character(len=255)::errmessage
! ----------------------------------------------------------------------
if (.not. (n1 .and. n2 .and. n3)) then
 write(errmessage,'(a)') 'assertion failed in: '//string
 call ferror (prname,trim(errmessage))
end if
end subroutine assert3
! **********************************************************************
subroutine assert_v (n,string)
implicit none
character(len=*),intent(in)::string
logical,dimension(:),intent(in)::n
character(len=*),parameter::prname='assert_v'
character(len=255)::errmessage
! ----------------------------------------------------------------------
if (.not. all(n)) then
 write(errmessage,'(a)') 'assertion failed in: '//string
 call ferror (prname,trim(errmessage))
end if
end subroutine assert_v
! **********************************************************************
function assert_eq2(n1,n2,string)
implicit none
character(len=*),intent(in) :: string
integer,intent(in) :: n1,n2
integer :: assert_eq2
character(len=*),parameter::prname='assert_eq2'
character(len=255)::errmessage
! ----------------------------------------------------------------------
if (n1 == n2) then
 assert_eq2=n1
else
 write(errmessage,'(a)') 'assertion failed in: '//string
 call ferror (prname,trim(errmessage))
end if
end function assert_eq2
! **********************************************************************
function assert_eq3(n1,n2,n3,string)
implicit none
character(len=*),intent(in) :: string
integer,intent(in) :: n1,n2,n3
integer :: assert_eq3
character(len=*),parameter::prname='assert_eq3'
character(len=255)::errmessage
! ----------------------------------------------------------------------
if (n1 == n2 .and. n2 == n3) then
 assert_eq3=n1
else
 write(errmessage,'(a)') 'assertion failed in: '//string
 call ferror (prname,trim(errmessage))
end if
end function assert_eq3
! **********************************************************************
function assert_eq4(n1,n2,n3,n4,string)
implicit none
character(len=*),intent(in) :: string
integer,intent(in) :: n1,n2,n3,n4
integer :: assert_eq4
character(len=*),parameter::prname='assert_eq4'
character(len=255)::errmessage
! ----------------------------------------------------------------------
if (n1 == n2 .and. n2 == n3 .and. n3 == n4) then
 assert_eq4=n1
else
 write(errmessage,'(a)') 'assertion failed in: '//string
 call ferror (prname,trim(errmessage))
end if
end function assert_eq4
! **********************************************************************
function assert_eqn(nn,string)
implicit none
character(len=*),intent(in) :: string
integer,dimension(:),intent(in) :: nn
integer::assert_eqn
character(len=*),parameter::prname='assert_eqn'
character(len=255)::errmessage
! ----------------------------------------------------------------------
if (all(nn(2:) == nn(1))) then
 assert_eqn=nn(1)
else
 write(errmessage,'(a)') 'assertion failed in: '//string
 call ferror (prname,trim(errmessage))
end if
end function assert_eqn
! **********************************************************************
function outerand(a,b)
implicit none
logical(LGT),dimension(:),intent(in) :: a,b
logical(LGT),dimension(size(a),size(b)) :: outerand
! ----------------------------------------------------------------------
outerand = spread(a,dim=2,ncopies=size(b)) .and. &
           spread(b,dim=1,ncopies=size(a))
end function outerand
! **********************************************************************
function outerprod_d(a,b)
implicit none
real(DPT),dimension(:),intent(in) :: a,b
real(DPT),dimension(size(a),size(b)) :: outerprod_d
! ----------------------------------------------------------------------
outerprod_d = spread(a,dim=2,ncopies=size(b)) * &
              spread(b,dim=1,ncopies=size(a))
end function outerprod_d
! **********************************************************************
function outerprod_z(a,b)
implicit none
complex(DPT),dimension(:),intent(in) :: a,b
complex(DPT),dimension(size(a),size(b)) :: outerprod_z
! ----------------------------------------------------------------------
outerprod_z = spread(a,dim=2,ncopies=size(b)) * &
              spread(b,dim=1,ncopies=size(a))
end function outerprod_z
! **********************************************************************
subroutine swap_d(a,b)
implicit none
real(DPT),intent(inout) :: a,b
real(DPT) :: dum
! ----------------------------------------------------------------------
dum=a
a=b
b=dum
end subroutine swap_d
! **********************************************************************
subroutine swap_c(a,b)
implicit none
complex(SPT),intent(inout) :: a,b
complex(SPT) :: dum
! ----------------------------------------------------------------------
dum=a
a=b
b=dum
end subroutine swap_c
! **********************************************************************
subroutine swap_z(a,b)
implicit none
complex(DPT),intent(inout) :: a,b
complex(DPT) :: dum
! ----------------------------------------------------------------------
dum=a
a=b
b=dum
end subroutine swap_z
! **********************************************************************
subroutine swap_dv(a,b)
implicit none
real(DPT),dimension(:),intent(inout) :: a,b
real(DPT),dimension(size(a)) :: dum
! ----------------------------------------------------------------------
dum=a
a=b
b=dum
end subroutine swap_dv
! **********************************************************************
subroutine swap_cv(a,b)
implicit none
complex(SPT),dimension(:),intent(inout) :: a,b
complex(SPT),dimension(size(a)) :: dum
! ----------------------------------------------------------------------
dum=a
a=b
b=dum
end subroutine swap_cv
! **********************************************************************
subroutine swap_zv(a,b)
implicit none
complex(DPT),dimension(:),intent(inout) :: a,b
complex(DPT),dimension(size(a)) :: dum
! ----------------------------------------------------------------------
dum=a
a=b
b=dum
end subroutine swap_zv
! **********************************************************************
pure function kron(i,j)
implicit none
integer(I4B),intent(in)::i,j
integer(I4B)::kron
! ----------------------------------------------------------------------
if (i == j) then ; kron=1
            else ; kron=0
end if
end function kron
! **********************************************************************
subroutine UniformGridVerification (grid,int,descr,prec)
implicit none
real(DPT),intent(in)::grid(:)
logical(LGT),intent(in),optional::int
character(len=*),intent(in),optional::descr
real(DPT),intent(in),optional::prec
character(len=*),parameter::prname='UniformGridVerification'
character(len=:),allocatable::description
integer(I4B)::i
real(DPT),parameter::prec_default=1.0e-12_DPT
real(DPT)::precision
real(DPT)::spacing
! ----------------------------------------------------------------------
if (present(descr)) then ; description=descr//': '
                    else ; description=''
end if
if (present(prec)) then ; precision=prec
                   else ; precision=prec_default
end if
if (size(grid) < 2) call ferror(prname,description//'grid size must be at least 2, or larger')
spacing=grid(2)-grid(1)
if ( any (abs([(grid(i+1)-grid(i),i=2,size(grid)-1)]/spacing-ONE) > precision) ) &
  call ferror(prname,description//'grid is not uniform')
if (present(int)) then
  if (int) then
    if (abs(grid(1)/spacing - nint(grid(1)/spacing))/nint(grid(1)/spacing) > precision) &
      call ferror(prname,description//'first grid point coordinate value is not multiple of grid spacing')
  end if
end if
end subroutine UniformGridVerification
! **********************************************************************
function realloc_dv(p,n)
implicit none
real(DPT),dimension(:),pointer::p,realloc_dv
character(len=*),parameter::prname='realloc_dv'
integer(I4B),intent(in)::n
integer(I4B)::nold,ierr
! ----------------------------------------------------------------------
allocate(realloc_dv(n),stat=ierr)
if (ierr /= 0) call ferror(prname,'Failed to allocate memory')
if (.not.associated(p)) return
nold=size(p)
realloc_dv(1:min(nold,n))=p(1:min(nold,n))
deallocate(p)
end function realloc_dv
! **********************************************************************
function realloc_dm(p,n,m)
implicit none
real(DPT),dimension(:,:),pointer::p,realloc_dm
character(len=*),parameter::prname='realloc_dm'
integer(I4B),intent(in)::n,m
integer(I4B)::nold,mold,ierr
! ----------------------------------------------------------------------
allocate(realloc_dm(n,m),stat=ierr)
if (ierr /= 0) call ferror(prname,'Failed to allocate memory')
if (.not.associated(p)) return
nold=size(p,1)
mold=size(p,2)
realloc_dm(1:min(nold,n),1:min(mold,m))=p(1:min(nold,n),1:min(mold,m))
deallocate(p)
end function realloc_dm
! **********************************************************************
function realloc_iv(p,n)
implicit none
integer(I4B),dimension(:),pointer::p,realloc_iv
character(len=*),parameter::prname='realloc_iv'
integer(I4B),intent(in)::n
integer(I4B)::nold,ierr
! ----------------------------------------------------------------------
allocate(realloc_iv(n),stat=ierr)
if (ierr /= 0) call ferror(prname,'Failed to allocate memory')
if (.not.associated(p)) return
nold=size(p)
realloc_iv(1:min(nold,n))=p(1:min(nold,n))
deallocate(p)
end function realloc_iv
! **********************************************************************
function realloc_im(p,n,m)
implicit none
integer(I4B),dimension(:,:),pointer::p,realloc_im
character(len=*),parameter::prname='realloc_im'
integer(I4B),intent(in)::n,m
integer(I4B)::nold,mold,ierr
! ----------------------------------------------------------------------
allocate(realloc_im(n,m),stat=ierr)
if (ierr /= 0) call ferror(prname,'Failed to allocate memory')
if (.not.associated(p)) return
nold=size(p,1)
mold=size(p,2)
realloc_im(1:min(nold,n),1:min(mold,m))=p(1:min(nold,n),1:min(mold,m))
deallocate(p)
end function realloc_im
! **********************************************************************
!function realloc_str(p,n)
!implicit none
!character(len=:),pointer::p,realloc_str
!character(len=*),parameter::prname='realloc_str'
!integer(I4B),intent(in)::n
!integer(I4B)::nold,ierr
! This function call causes a catastrophic compile-time error with
! IVF 11.1. But it is built smoothly by Intel Fortran Compiler XE 13.0
! ----------------------------------------------------------------------
!allocate(character(len=n)::realloc_str,stat=ierr)
!if (ierr /= 0) call ferror(prname,'Failed to allocate memory')
!if (.not.associated(p)) return
!nold=len(p)
!realloc_str(1:min(nold,n))=p(1:min(nold,n))
!deallocate(p)
!end function realloc_str
! **********************************************************************
function realloc_str(s,n)
implicit none
character(len=:),allocatable::realloc_str
character(len=:),allocatable::s
integer,intent(in)::n
integer::nold
! ----------------------------------------------------------------------
allocate(character(len=n)::realloc_str)
if (.not.allocated(s)) return
nold=len(s)
realloc_str(1:min(nold,n))=s(1:min(nold,n))
deallocate(s)
end function realloc_str
! **********************************************************************
function locate (xx,x)
implicit none
real(DPT),dimension(:),intent(in)::xx
real(DPT),intent(in)::x
integer(I4B)::locate
integer(I4B)::n,jl,jm,ju
logical::ascnd
! Given a real array xx(1:N) and a real value x this routine
! returns a value j such that x is between xx(j) and xx(j + 1).
! xx must be monotonic - either increasing or decreasing.
! j = 0 or j = N is returned to indicate that x is out of range.
! ----------------------------------------------------------------------
n=size(xx)
ascnd = (xx(n) >= xx(1)) ! True if ascending order of table, false otherwise
jl=0   ! Initialize lower...
ju=n+1 ! ...and upper limits
do
 if (ju-jl <= 1) exit ! Repeat until this condition is satisfied
 jm=(ju+jl)/2 ! Compute a midpoint...
 if (ascnd .eqv. (x >= xx(jm))) then
  jl=jm       ! ...and replace either the lower limit...
 else
  ju=jm       ! ...or the upper limit, as appropriate
 end if
end do
! Then set the output, being careful with the endpoints
if      (x == xx(1)) then ; locate=1
else if (x == xx(n)) then ; locate=n-1
                     else ; locate=jl
end if
end function locate
! **********************************************************************
end module LowLevLib
