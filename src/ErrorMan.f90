! **********************************************************************
module ErrMan
use Types
implicit none
private
public ferror,nerror
contains
! **********************************************************************
subroutine nerror (locname,usrmsg,rterrno)
use ifcore
implicit none
character(len=*),intent(in)::locname ! name of the location the error was encountered at
character(len=*),intent(in)::usrmsg ! user-provided error message
integer(I4B),optional,intent(in)::rterrno ! run-time error number
character(len=LongStrLen)::rtlmsg ! standard RTL error message
character(len=LongStrLen)::serrno
! ----------------------------------------------------------------------
if (present(rterrno)) then
 write(serrno,'(i0)') rterrno
 call GERROR(rtlmsg)
 write(6,'(/,a,/)') 'Noncritical Error: '//trim(locname)//' ('//trim(usrmsg)//')'// &
                            ' RTL error #'//trim(serrno)//' ('//trim(rtlmsg)//')'
else
 write(6,'(/,a,/)') 'Noncritical Error: '//trim(locname)//' ('//trim(usrmsg)//')'
end if
end subroutine nerror
! **********************************************************************
subroutine ferror (locname,usrmsg,rterrno)
use ifcore
implicit none
character(len=*),intent(in)::locname ! name of the location the error was encountered at
character(len=*),intent(in)::usrmsg ! user-provided error message
integer(I4B),optional,intent(in)::rterrno ! run-time error number
character(len=LongStrLen)::rtlmsg ! standard RTL error message
character(len=LongStrLen)::serrno
! ----------------------------------------------------------------------
if (present(rterrno)) then
 write(serrno,'(i0)') rterrno
 call GERROR(rtlmsg)
 write(6,'(/,a,/)') 'FATAL ERROR: '//trim(locname)//' ('//trim(usrmsg)//')'// &
                      ' RTL error #'//trim(serrno)//' ('//trim(rtlmsg)//')'
else
 write(6,'(/,a,/)') 'FATAL ERROR: '//trim(locname)//' ('//trim(usrmsg)//')'
end if
!CALL SLEEPQQ (5000)
write(6,*) 'Press <ENTER> to close the program...'
read(5,*)
stop 1
end subroutine ferror
! **********************************************************************
end module ErrMan
