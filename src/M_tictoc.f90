module M_tictoc
use,intrinsic :: iso_fortran_env, only : int8,int16,int32,int64,real32,real64,real128
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
implicit none
private

type timer
   real(kind=real64)   :: cpu_start
   real(kind=real64)   :: cpu_end
   integer(kind=int64) :: clock_start
   integer(kind=int64) :: clock_end
   integer             :: dat_start(8)
   integer             :: dat_end(8)
   contains
       procedure  ::  tic        =>  clock_tic
       procedure  ::  toc        =>  clock_toc
       procedure  ::  print      =>  clock_print
       procedure  ::  wallclock  =>  clock_wallclock
       procedure  ::  cputime    =>  clock_cputime
       procedure  ::  dattime    =>  clock_dattime
end type

interface timer
     procedure :: clock_new
end interface timer

!integer,parameter,public   :: realtime=kind(0.0d0)  ! type for unix epoch time and julian days
integer,parameter,public   :: realtime=real64        ! type for unix epoch time and julian days

public :: timer
public :: irand
public :: catstat

character(len=*),parameter :: gen='(*(g0))'

contains

! initialization constructor
type(timer) function clock_new(this)
type(timer),intent(in),optional :: this

   call cpu_time(clock_new%cpu_start)
   call system_clock(clock_new%clock_start)
   call date_and_time(values=clock_new%dat_start)

   clock_new%cpu_end= clock_new%cpu_start
   clock_new%clock_end= clock_new%clock_start
   clock_new%dat_end= clock_new%dat_start

end function clock_new

subroutine clock_tic(this)
class(timer) :: this

   call cpu_time(this%cpu_start)
   call system_clock(this%clock_start)
   call date_and_time(values=this%dat_start)

   this%cpu_end= this%cpu_start
   this%clock_end= this%clock_start
   this%dat_end= this%dat_start

end subroutine clock_tic

subroutine clock_toc(this)
class(timer) :: this

   call cpu_time(this%cpu_end)
   call system_clock(this%clock_end)
   call date_and_time(values=this%dat_end)

end subroutine clock_toc

subroutine clock_print(this,string,lun)
class(timer),intent(in)                  :: this
character(len=*),intent(in),optional     :: string
integer(kind=int32),intent(in),optional  :: lun
integer(kind=int32)                      :: lun_
real(kind=real64)                        :: elapsed_time
real(kind=realtime)                      :: elapsed_date_and_time
real(kind=real64)                        :: cpu_time
character(len=105)                       :: biggest
integer(kind=int64)                      :: count_rate


   if(present(lun))then
      lun_=lun
   else
      lun_=stdout
   endif
   elapsed_time = this%wallclock()
   elapsed_date_and_time = this%dattime()
   cpu_time = this%cputime()

   if(present(string)) write( lun_,gen ) string

   write( lun_,'(a,f0.3)')       'Elapsed dat  (sec) ::',elapsed_date_and_time
   ! find how many characters to use for integers
   call system_clock(count_rate=count_rate) ! Find the time rate
   ! try to make a reasonable format for the number of digits of precision
   write(biggest,'("(a,f0.",i0,")")')ceiling(log10(real(count_rate,kind=real64)))
   write( lun_,biggest)          'Elapsed time (sec) ::',elapsed_time
   write( lun_,gen)              'CPU time     (sec) ::',cpu_time
   write( lun_,'(a,1x,f0.2)')    'Percentage         ::',(cpu_time/elapsed_time)*100

end subroutine clock_print

function clock_wallclock(this) result(elapsed_time)
class(timer)        :: this
integer(kind=int64) :: count_rate
real(kind=real64)   :: elapsed_time
real(kind=real64)   :: cpu_time
   call system_clock(count_rate=count_rate) ! Find the time rate
   elapsed_time = real(this%clock_end-this%clock_start,kind=real64)/real(count_rate,kind=real64)
end function clock_wallclock

function  clock_cputime(this)  result(cpu_time)
class(timer)        :: this
real(kind=real64)   :: cpu_time
   cpu_time = real(this%cpu_end-this%cpu_start,kind=real64)
end function clock_cputime

function  clock_dattime(this)  result(cpu_time)
class(timer)        :: this
real(kind=real64)   :: cpu_time
real(kind=realtime) :: e,s
integer             :: ierr
   call date_to_julian(this%dat_end,e,ierr)
   call date_to_julian(this%dat_start,s,ierr)
   cpu_time = real((e-s)*86400,kind=real64)
end function clock_dattime

subroutine date_to_julian(dat,julian,ierr)
! @(#)M_time::date_to_julian(3f): Converts proleptic Gregorian DAT date-time array to Julian Date
! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19
! correction for time zone should or should not be included?
integer,intent(in)               ::  dat(8)! array like returned by DATE_AND_TIME(3f)
real(kind=realtime),intent(out)  ::  julian
integer,intent(out) :: ierr ! 0 =successful, -1=bad year, -2=bad month, -3=ibad day, -4=bad date 29 Feb, non leap-year
integer                          ::  a , y , m , jdn
integer                          ::  utc
utc=dat(4)*60
associate&
&(year=>dat(1),month=>dat(2),day=>dat(3),utc=>utc,hour=>dat(5),minute=>dat(6),second=>dat(7)-utc+dat(8)/1000.0d0)
   julian = -huge(99999)               ! this is the date if an error occurs and IERR is < 0
   if ( year==0 .or. year<-4713 ) then
      ierr = -1
   else
      ierr=0
   !  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
      a = (14-month)/12    ! A will be 1 for January or February, and 0 for other months, with integer truncation
      y = year + 4800 - a
      m = month + 12*a - 3 ! M will be 0 for March and 11 for February
   !  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
   !  Convert to a negative number, then increment towards zero
   !  Staring from a Gregorian calendar date
      jdn = day + (153*m+2)/5 + 365*y + y/4 - y/100 + y/400 - 32045 !  with integer truncation
   !  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
      julian = jdn + (hour-12)/24.0_realtime + (minute)/1440.0_realtime + second/86400.0_realtime
      ierr=merge(-6,ierr, julian<0.0_realtime ) ! Julian Day must be non-negative
   endif
end associate
end subroutine date_to_julian

impure elemental function irand(first,last)
!@(#) a random whole number from FIRST to LAST inclusive
use, intrinsic :: iso_fortran_env, only : dp=>real64
integer,intent(in)   :: first,last
real(kind=dp)        :: rand_val
integer              :: irand
   call random_number(rand_val)
   irand=first+floor((last+1-first)*rand_val)
end function irand

function scramble( number_of_values ) result(array)
!$@(#) M_random::scramble(3f): return integer array of random values 1 to N.
integer,intent(in)    :: number_of_values
integer,allocatable   :: array(:)
integer               :: i, j, k
integer               :: temp
real                  :: u
   array=[(i,i=1,number_of_values)]
   do k=1,2
      do i=1,number_of_values
         call random_number(u)
         j = 1 + FLOOR(number_of_values*u)
         ! switch values
         temp=array(j)
         array(j)=array(i)
         array(i)=temp
      enddo
   enddo
end function scramble

subroutine catstat()
!@(#) on linux systems show process statistics. System-dependent
integer             ::  exitstat
integer             ::  cmdstat
character(len=255)  ::  cmdmsg
   call execute_command_line("cat /proc/$PPID/status", exitstat=exitstat,cmdstat=cmdstat,cmdmsg=cmdmsg)
end subroutine catstat

end module M_tictoc
