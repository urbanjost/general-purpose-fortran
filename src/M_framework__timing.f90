module M_framework__timing
use,intrinsic :: iso_fortran_env, only : int8,int16,int32,int64,real32,real64,real128
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
implicit none
private

type timer
   real(real64)   :: cpu_start
   real(real64)   :: cpu_end
   integer(int64) :: clock_start
   integer(int64) :: clock_end
   contains
      procedure :: tic => clock_tic
      procedure :: toc => clock_toc
      procedure :: print => clock_print
      procedure :: wallclock => clock_wallclock
      procedure :: cputime => clock_cputime
end type

interface timer
     procedure :: clock_new
end interface timer

public :: timer

character(len=*),parameter :: gen='(*(g0,1x))'

contains

! initialization constructor
type(timer) function clock_new(this)
type(timer),intent(in),optional :: this
   call cpu_time(clock_new%cpu_start)
   call system_clock(clock_new%clock_start)
   clock_new%cpu_end= clock_new%cpu_start
   clock_new%clock_end= clock_new%clock_start
end function clock_new

subroutine clock_tic(this)
class(timer) :: this
   call cpu_time(this%cpu_start)
   call system_clock(this%clock_start)
   this%cpu_end= this%cpu_start
   this%clock_end= this%clock_start
end subroutine clock_tic

subroutine clock_toc(this)
class(timer) :: this
   call cpu_time(this%cpu_end)
   call system_clock(this%clock_end)
end subroutine clock_toc

subroutine clock_print(this,string,lun)
class(timer),intent(in)                  :: this
character(len=*),intent(in),optional     :: string
integer(kind=int64),intent(in),optional  :: lun
integer(kind=int64)                      :: count_rate
integer(kind=int64)                      :: lun_
real                                     :: elapsed_time
real                                     :: cpu_time
   if(present(lun))then
      lun_=lun
   else
      lun_=stdout
   endif
   elapsed_time = this%wallclock()
   cpu_time = this%cputime()
   if(present(string))then
      write( lun_,gen,advance='no' ) string
      write( lun_,gen,advance='no' )           'Elapsed time (sec) ::', elapsed_time,','
      write( lun_,gen,advance='no' )           'CPU time (sec) ::', cpu_time,','
      write( lun_,'(a,1x,f0.2)',advance='yes') 'Percentage ::', (cpu_time/elapsed_time)*100.0
   else
      write( lun_,gen)           'Elapsed time (sec) ::',elapsed_time
      write( lun_,gen)           'CPU time     (sec) ::',cpu_time
      write( lun_,'(a,1x,f0.2)') 'Percentage         ::',(cpu_time/elapsed_time)*100
   endif
end subroutine clock_print

function clock_wallclock(this) result(elapsed_time)
class(timer)        :: this
integer(kind=int64) :: count_rate
real                :: elapsed_time
real                :: cpu_time
   call system_clock(count_rate=count_rate) ! Find the time rate
   elapsed_time = real(this%clock_end-this%clock_start)/real(count_rate)
end function clock_wallclock

function clock_cputime(this) result(cpu_time)
class(timer)        :: this
real                :: cpu_time
   cpu_time = real(this%cpu_end-this%cpu_start)
end function clock_cputime

end module M_framework__timing
