










!>
!!
!!                     M_StopWatch Version 1.1
!!
!!   M_StopWatch is a Fortran 90 module for portable, easy-to-use
!!   measurement of execution time.  It supports four clocks
!!
!!      o wall clock
!!      o CPU clock
!!      o user CPU clock
!!      o system CPU clock
!!
!!   and returns all times in seconds.
!!
!!   It provides a simple means of determining which clocks are available,
!!   and the precision of those clocks.
!!
!!   M_StopWatch is used by instrumenting your code with subroutine calls
!!   that mimic the operation of a stop watch.  M_StopWatch supports multiple
!!   watches, and provides the concept of watch groups to allow functions
!!   to operate on multiple watches simultaneously.
!!
!!   For further information on using M_StopWatch, see the User Guide or
!!   man pages.
!!
!!   The M_StopWatch software and documentation have been produced as part
!!   of work done by the U.S. Government, and are not subject to copyright
!!   in the United States.
!!
!!   William F. Mitchell
!!   mitchell@cam.nist.gov
!!   National Institute of Standards and Technology
!!   December 2, 1996
!>
!!
!! The research software provided on this web site (“software”) is provided by
!! NIST as a public service. You may use, copy and distribute copies of the
!! software in any medium, provided that you keep intact this entire notice. You
!! may improve, modify and create derivative works of the software or any portion
!! of the software, and you may copy and distribute such modifications or works.
!! Modified works should carry a notice stating that you changed the software and
!! should note the date and nature of any such change. Please explicitly
!! acknowledge the National Institute of Standards and Technology as the source of
!! the software.
!!
!! The software is expressly provided “AS IS.” NIST MAKES NO WARRANTY OF ANY KIND,
!!##EXPRESS, IMPLIED, IN FACT OR ARISING BY OPERATION OF LAW, INCLUDING, WITHOUT
!!##LIMITATION, THE IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
!!##PURPOSE, NON-INFRINGEMENT AND DATA ACCURACY. NIST NEITHER REPRESENTS NOR
!!##WARRANTS THAT THE OPERATION OF THE SOFTWARE WILL BE UNINTERRUPTED OR ERROR-FREE,
!!##OR THAT ANY DEFECTS WILL BE CORRECTED. NIST DOES NOT WARRANT OR MAKE ANY
!!##REPRESENTATIONS REGARDING THE USE OF THE SOFTWARE OR THE RESULTS THEREOF,
!!##INCLUDING BUT NOT LIMITED TO THE CORRECTNESS, ACCURACY, RELIABILITY, OR
!!##USEFULNESS OF THE SOFTWARE.
!!
!! You are solely responsible for determining the appropriateness of using and
!! distributing the software and you assume all risks associated with its use,
!! including but not limited to the risks and costs of program errors, compliance
!! with applicable laws, damage to or loss of data, programs or equipment, and the
!! unavailability or interruption of operation. This software is not intended to be
!! used in any situation where a failure could cause risk of injury or damage to
!! property. The software was developed by NIST employees. NIST employee
!! contributions are not subject to copyright protection within the United States.
module M_stopwatch
use M_system, only : system_cpu_time
implicit none
private

! ident_1="@(#)M_stopwatch::M_stopwatch(3f): package for measuring cpu and wall clock"

public :: create_watch, destroy_watch, start_watch, stop_watch, reset_watch, &
       read_watch, print_watch, pause_watch, end_pause_watch, &
       option_stopwatch, inquiry_stopwatch, create_watchgroup, destroy_watchgroup, &
       join_watchgroup, leave_watchgroup

public test_suite_M_stopwatch

private :: create_watch_aa, create_watch_as, create_watch_sa, create_watch_ss, &
     destroy_watch_aa, destroy_watch_as, destroy_watch_sa, destroy_watch_ss, &
     start_watch_aa, start_watch_as, start_watch_sa, start_watch_ss, start_watch_ga, start_watch_gs, &
     stop_watch_aa, stop_watch_as, stop_watch_sa, stop_watch_ss, stop_watch_ga, stop_watch_gs, &
     reset_watch_aa, reset_watch_as, reset_watch_sa, reset_watch_ss, reset_watch_ga, reset_watch_gs, &
     pause_watch_aa, pause_watch_as, pause_watch_sa, pause_watch_ss, pause_watch_ga, pause_watch_gs, &
     end_pause_watch_aa, end_pause_watch_as, end_pause_watch_sa, end_pause_watch_ss, &
     end_pause_watch_ga, end_pause_watch_gs, &
     read_watch_aa, read_watch_as, read_watch_sa, read_watch_ss, &
     read_watch_ax, read_watch_sx, &
     print_watch_aa, print_watch_as, print_watch_sa, print_watch_ss, print_watch_ga, print_watch_gs, &
     option_stopwatch_a, option_stopwatch_s, &
     create_watchgroup_a, create_watchgroup_s, &
     join_watchgroup_a, join_watchgroup_s, &
     leave_watchgroup_a, leave_watchgroup_s, &
     which_clocks, which_clocks_a, which_clocks_s, &
     create_watch_actual, destroy_watch_actual, start_watch_actual, stop_watch_actual, &
     reset_watch_actual, pause_watch_actual, end_pause_watch_actual, &
     read_watch_actual, print_watch_actual, create_watchgroup_actual, &
     join_watchgroup_actual, leave_watchgroup_actual, &
     err_handler_watch, print_time, free_watch_list
!----------------------------------------------------
! The following parameters are defined:

! M_StopWatch version number
character(len=16), private, parameter :: sw_version = "1.1"

! status of clocks
integer, private, parameter :: STOPPED = 1, &
                      RUNNING = 2, &
                      PAUSED  = 3, &
                      OMITTED = 4
! error codes
integer, private, parameter :: ERR_CREATE    = 1, &
                      ERR_BAD_STATE = 2, &
                      ERR_UNK_STATE = 4, &
                      ERR_CLOCK     = 8, &
                      ERR_TMC       = 16, &
                      ERR_NAMES     = 32, &
                      ERR_C2LONG    = 64, &
                      ERR_GROUP     = 128, &
                      ERR_IO        = 256, &
                      ERR_ALLOC     = 512, &
                      ERR_DEALLOC   = 1024, &
                      ERR_FORM      = 2048

! length of character strings
integer, private, parameter :: CLOCK_LEN = 4, NAME_LEN = 132, FORM_LEN = 12
!----------------------------------------------------

!----------------------------------------------------
! The following types are defined:

type, private :: clocks
   real :: cpu, user, sys
   integer :: wall
end type clocks

type, private :: status_type
   integer :: cpu, user, sys, wall
end type status_type

type, private :: watch_actual
   character(len=NAME_LEN) :: name
   type (status_type) :: status
   type (clocks) :: last_read
   type (clocks) :: elapsed
end type watch_actual

type, public :: watchtype
   private
   type (watch_actual), pointer :: ptr
end type watchtype

type, private :: watch_pointer
   type (watch_actual), pointer :: ptr
end type watch_pointer

type, private :: watch_list
   type (watch_actual), pointer :: this_watch
   type (watch_list), pointer :: next
end type watch_list

type, public :: watchgroup
   private
   type (watch_list), pointer:: head
   integer :: wgsize
end type watchgroup

!----------------------------------------------------
!----------------------------------------------------
! The following variables are defined:

logical, private :: do_cpu, do_user, do_sys, do_wall
character(len=CLOCK_LEN), private, allocatable, dimension(:) :: default_clocks
integer, private, save :: iounit = 6, errunit = 6
logical, private, save :: errprint = .true., errabort = .false.
character(len=FORM_LEN), private, save :: default_form = "sec"

!----------------------------------------------------
!----------------------------------------------------
! Non-module procedures used are:

! this will be reinserted when F allows interface blocks
!interface

!   subroutine system_cpu_time(cpu,user,sys)
!   implicit none
!   real, intent(out) :: cpu, user, sys
!   end subroutine system_cpu_time

!end interface
!----------------------------------------------------
!----------------------------------------------------
! Generic procedure names are:

interface create_watch
   module procedure create_watch_aa, create_watch_as, create_watch_sa, create_watch_ss
end interface

interface destroy_watch
   module procedure destroy_watch_aa, destroy_watch_as, destroy_watch_sa, destroy_watch_ss
end interface

interface start_watch
   module procedure start_watch_aa, start_watch_as, start_watch_sa, start_watch_ss, start_watch_ga, start_watch_gs
end interface

interface stop_watch
   module procedure stop_watch_aa, stop_watch_as, stop_watch_sa, stop_watch_ss, stop_watch_ga, stop_watch_gs
end interface

interface reset_watch
   module procedure reset_watch_aa, reset_watch_as, reset_watch_sa, reset_watch_ss, reset_watch_ga, reset_watch_gs
end interface

interface pause_watch
   module procedure pause_watch_aa, pause_watch_as, pause_watch_sa, pause_watch_ss, pause_watch_ga, pause_watch_gs
end interface

interface end_pause_watch
   module procedure end_pause_watch_aa, end_pause_watch_as, end_pause_watch_sa, end_pause_watch_ss, &
                    end_pause_watch_ga, end_pause_watch_gs
end interface

interface read_watch
   module procedure read_watch_aa, read_watch_as, read_watch_ax, read_watch_sa, read_watch_ss, read_watch_sx
end interface

interface print_watch
   module procedure print_watch_aa, print_watch_as, print_watch_sa, print_watch_ss, print_watch_ga, print_watch_gs
end interface

interface option_stopwatch
   module procedure option_stopwatch_a, option_stopwatch_s
end interface

interface create_watchgroup
   module procedure create_watchgroup_a, create_watchgroup_s
end interface

interface join_watchgroup
   module procedure join_watchgroup_a, join_watchgroup_s
end interface

interface leave_watchgroup
   module procedure leave_watchgroup_a, leave_watchgroup_s
end interface

interface which_clocks
   module procedure which_clocks_a, which_clocks_s
end interface
contains
!-------------------------------------------------------------------
!                 CREATE_WATCH
!-------------------------------------------------------------------

!          -------------------
subroutine create_watch_actual(watch,clock,name,err)
!          -------------------

!----------------------------------------------------
! This routine creates the specified watches with the specified clocks.
! You can NOT use it to add a clock to an already created watch.  This is
! because I cannot use "allocated" to see if the watch was already created,
! and then know whether to allocate or just add a clock.
!----------------------------------------------------

!----------------------------------------------------
! Dummy arguments

type (watch_pointer), intent(out), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
character(len=*), intent(in), dimension(:) :: name
integer, optional, intent(out) :: err

!----------------------------------------------------

!----------------------------------------------------
! Local variables:

type (watch_actual), pointer :: the_watch
integer :: i, erralloc
character(len=NAME_LEN) :: tname

!----------------------------------------------------

!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! set default clocks to be all available clocks.  This only needs to be done
! once, which can be checked by seeing if default_clocks has been allocated.

if (.not. allocated(default_clocks)) then
   call option_stopwatch(default_clock=(/"cpu ","user","sys ","wall"/),err=err)
end if

! set the flags for which clocks to create

call which_clocks(clock,"create_watch",err)

! loop through the watches

do i=1,ubound(watch,dim=1)

   allocate(watch(i)%ptr,stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_ALLOC,"create_watch","", "Watch not created.",err)
   else
      the_watch => watch(i)%ptr

      if (len_trim(name(i)) > 132) then
         call err_handler_watch(ERR_C2LONG,"create_watch",name(i), "Name shortened to 132 characters.",err)
         tname = name(i)(1:132)
         the_watch = watch_actual(tname,status_type(OMITTED,OMITTED,OMITTED, &
                                  OMITTED),clocks(0.0,0.0,0.0,0),clocks(0.0,0.0,0.0,0))
      else

         the_watch = watch_actual(name(i),status_type(OMITTED,OMITTED,OMITTED, &
                                  OMITTED),clocks(0.0,0.0,0.0,0),clocks(0.0,0.0,0.0,0))
      end if

      if (do_cpu) then
         the_watch%status%cpu = STOPPED
      end if
      if (do_user) then
         the_watch%status%user = STOPPED
      end if
      if (do_sys ) then
         the_watch%status%sys  = STOPPED
      end if
      if (do_wall) then
         the_watch%status%wall = STOPPED
      end if

   end if
end do

end subroutine create_watch_actual
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
! Alternate forms for create_watch
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine create_watch_aa(watch,clock,name,err)
!          ---------------
type (watchtype), intent(out), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
character(len=*), optional, intent(in), dimension(:) :: name
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
character(len=NAME_LEN), allocatable, dimension(:) :: no_name
integer :: erralloc,i
if (present(name)) then
   if (size(watch) /= size(name)) then
      call err_handler_watch(ERR_NAMES,"create_watch","", "Watches not created.",err)
      return
   end if
end if
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"create_watch","", "Watches not created.",err)
else
   if (present(name)) then
      call create_watch_actual(watches,clock,name,err)
   else
      allocate(no_name(size(watch)),stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_ALLOC,"create_watch","", "Watches not created.",err)
      else
         no_name = "unnamed watch"
         call create_watch_actual(watches,clock,no_name,err)
         deallocate(no_name,stat=erralloc)
         if (erralloc > 0) then
            call err_handler_watch(ERR_DEALLOC,"create_watch","", "Watches created, but further problems may develop.",err)
         end if
      end if
   end if
end if
do i=1,size(watch)
   watch(i)%ptr => watches(i)%ptr
end do
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"create_watch","", "Watches created, but further problems may develop.",err)
end if

end subroutine create_watch_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine create_watch_as(watch,clock,name,err)
!          --------------
type (watchtype), intent(out), dimension(:) :: watch
character(len=*), optional, intent(in) :: clock
character(len=*), optional, intent(in), dimension(:) :: name
integer, optional, intent(out) :: err

if (present(clock)) then
   call create_watch_aa(watch, (/clock/),name,err)
else
   if (.not. allocated(default_clocks)) then
      call option_stopwatch(default_clock=(/"cpu ","user","sys ","wall"/),err=err)
   end if
   call create_watch_aa(watch,default_clocks,name,err)
end if

end subroutine create_watch_as
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine create_watch_sa(watch,clock,name,err)
!          --------------
type (watchtype), intent(out) :: watch
character(len=*), intent(in), dimension(:) :: clock
character(len=*), optional, intent(in) :: name
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches

if (present(name)) then
   call create_watch_actual(watches,clock,(/name/),err)
else
   call create_watch_actual(watches,clock,(/"unnamed watch"/),err)
end if

watch%ptr => watches(1)%ptr

end subroutine create_watch_sa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine create_watch_ss(watch,clock,name,err)
!          --------------
type (watchtype), intent(out) :: watch
character(len=*), optional, intent(in) :: clock
character(len=*), optional, intent(in) :: name
integer, optional,intent(out) :: err
type (watch_pointer), dimension(1) :: watches
if (present(name)) then
   if (present(clock)) then
      call create_watch_actual(watches, (/clock/),(/name/),err)
   else
      if (.not. allocated(default_clocks)) then
         call option_stopwatch(default_clock=(/"cpu ","user","sys ","wall"/),err=err)
      end if
      call create_watch_actual(watches,default_clocks,(/name/),err)
   end if
else
   if (present(clock)) then
      call create_watch_actual(watches, (/clock/),(/"unnamed watch"/),err)
   else
      if (.not. allocated(default_clocks)) then
         call option_stopwatch(default_clock=(/"cpu ","user","sys ","wall"/),err=err)
      end if
      call create_watch_actual(watches,default_clocks,(/"unnamed watch"/),err)
   end if
end if
watch%ptr => watches(1)%ptr

end subroutine create_watch_ss
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!                 DESTROY_WATCH
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!          -----------------------
subroutine destroy_watch_actual(watch,clock,err)
character(len=*),parameter :: ident_destroy_watch='M_stopwatch::destroy_watch(3f): destroys a M_StopWatch watch'
!          -----------------------

!----------------------------------------------------
! This routine destroys the specified clocks of the specified watches
! and destroys the watch if there are no remaining clocks.
!----------------------------------------------------

!----------------------------------------------------
! Dummy arguments

type (watch_pointer), intent(in out), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err

!----------------------------------------------------
!----------------------------------------------------
! Local variables:

type (watch_actual), pointer :: the_watch
integer :: i, erralloc

!----------------------------------------------------
!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! set the flags for which clocks to destroy

call which_clocks(clock,"destroy_watch",err)

! loop through the watches

do i=1,ubound(watch,dim=1)

   the_watch => watch(i)%ptr
   if (.not. associated(the_watch)) then
      call err_handler_watch(ERR_CREATE,"destroy_watch","", "Watch not destroyed.",err)
   else

      if (do_cpu) then
         the_watch%status%cpu = OMITTED
      end if
      if (do_user) then
         the_watch%status%user = OMITTED
      end if
      if (do_sys ) then
         the_watch%status%sys  = OMITTED
      end if
      if (do_wall) then
         the_watch%status%wall = OMITTED
      end if

      if( the_watch%status%cpu == OMITTED .and. the_watch%status%user == OMITTED &
      .and. the_watch%status%sys == OMITTED .and. the_watch%status%wall == OMITTED) then
         deallocate(watch(i)%ptr,stat=erralloc)
         if (erralloc > 0) then
            call err_handler_watch(ERR_DEALLOC,"destroy_watch","", "Watch destroyed, but further problems may develop.",err)
         end if
      end if
   end if
end do

end subroutine destroy_watch_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
! Alternate forms for destroy_watch
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine destroy_watch_aa(watch,clock,err)
!          ---------------
type (watchtype), intent(in out), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"destroy_watch","", "Watches not destroyed.",err)
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call destroy_watch_actual(watches,clock,err)
end if
do i=1,size(watch)
   watch(i)%ptr => watches(i)%ptr
end do
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"destroy_watch","", "Watches destroyed, but further problems may develop.",err)
end if

end subroutine destroy_watch_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine destroy_watch_as(watch,clock,err)
!          --------------
type (watchtype), intent(in out), dimension(:) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call destroy_watch_aa(watch, (/clock/),err)
else
   call destroy_watch_aa(watch,default_clocks,err)
end if

end subroutine destroy_watch_as
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine destroy_watch_sa(watch,clock,err)
!          --------------
type (watchtype), intent(in out) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
call destroy_watch_actual(watches,clock,err)
watch%ptr => watches(1)%ptr

end subroutine destroy_watch_sa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine destroy_watch_ss(watch,clock,err)
!          --------------
type (watchtype), intent(in out) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional,intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
if (present(clock)) then
   call destroy_watch_actual(watches, (/clock/),err)
else
   call destroy_watch_actual(watches,default_clocks,err)
end if
watch%ptr => watches(1)%ptr

end subroutine destroy_watch_ss
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!                 START_WATCH
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine start_watch_actual(watch,clock,err)

character(len=*),parameter :: ident_start_watch='M_stopwatch::start_watch(3f): starts the specified clocks of the specified watches'

!----------------------------------------------------
! Dummy arguments

type (watch_pointer), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err

!----------------------------------------------------

!----------------------------------------------------
! Local variables:

type (watch_actual), pointer :: the_watch
real :: readcpu,readusr,readsys
integer :: i

!----------------------------------------------------

!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! set the flags for which clocks to start

call which_clocks(clock,"start_watch",err)

! loop through the watches

do i=1,ubound(watch,dim=1)

   the_watch => watch(i)%ptr
   if (.not. associated(the_watch)) then
      call err_handler_watch(ERR_CREATE,"start_watch","", "Watch not started.",err)
   else

! start each flagged clock for this watch

      call system_cpu_time(readcpu,readusr,readsys)
      if (do_cpu) then
         select case (the_watch%status%cpu)
            case (STOPPED)
               the_watch%last_read%cpu = readcpu
               the_watch%status%cpu = RUNNING
            case (RUNNING, PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"start_watch",the_watch%name, "Watch's cpu clock not started.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"start_watch",the_watch%name, "Watch's cpu clock not started.",err)
         end select
      end if
      if (do_user) then
         select case (the_watch%status%user)
            case (STOPPED)
               the_watch%last_read%user = readusr
               the_watch%status%user = RUNNING
            case (RUNNING, PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"start_watch",the_watch%name, "Watch's user clock not started.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"start_watch",the_watch%name, "Watch's user clock not started.",err)
         end select
      end if
      if (do_sys) then
         select case (the_watch%status%sys)
            case (STOPPED)
               the_watch%last_read%sys = readsys
               the_watch%status%sys = RUNNING
            case (RUNNING, PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"start_watch",the_watch%name, "Watch's sys clock not started.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"start_watch",the_watch%name, "Watch's sys clock not started.",err)
         end select
      end if
      if (do_wall) then
         select case (the_watch%status%wall)
            case (STOPPED)
               call system_clock(count=the_watch%last_read%wall)
               the_watch%status%wall = RUNNING
            case (RUNNING, PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"start_watch",the_watch%name, "Watch's wall clock not started.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"start_watch",the_watch%name, "Watch's wall clock not started.",err)
         end select
      end if
   end if
end do

end subroutine start_watch_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
! Alternate forms for start_watch
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine start_watch_ga(watch,clock,err)
!          --------------
type (watchgroup), intent(in out) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
type (watch_list), pointer :: list_entry
integer :: erralloc, i
if (associated(watch%head)) then
   allocate(watches(watch%wgsize),stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_ALLOC,"start_watch","", "Watches not started.",err)
   else
      list_entry => watch%head
      i = 0
      do
         if (.not. associated(list_entry)) then
            exit
         end if
         i=i+1
         watches(i)%ptr => list_entry%this_watch
         list_entry => list_entry%next
      end do
      call start_watch_actual(watches,clock,err)
      deallocate(watches,stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_DEALLOC,"start_watch","", "Watches started, but further problems may develop.",err)
      end if
   end if
else
   if (present(err)) then
      err = 0
   end if
end if

end subroutine start_watch_ga
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine start_watch_gs(watch,clock,err)
!          --------------
type (watchgroup), intent(in out) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call start_watch_ga(watch, (/clock/),err)
else
   call start_watch_ga(watch,default_clocks,err)
end if

end subroutine start_watch_gs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine start_watch_aa(watch,clock,err)
!          --------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"start_watch","", "Watches not started.",err)
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call start_watch_actual(watches,clock,err)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"start_watch","", "Watches started, but further problems may develop.",err)
end if

end subroutine start_watch_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine start_watch_as(watch,clock,err)
!          --------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call start_watch_aa(watch, (/clock/),err)
else
   call start_watch_aa(watch,default_clocks,err)
end if

end subroutine start_watch_as
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine start_watch_sa(watch,clock,err)
!          --------------
type (watchtype), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
call start_watch_actual(watches,clock,err)

end subroutine start_watch_sa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine start_watch_ss(watch,clock,err)
!          --------------
type (watchtype), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
if (present(clock)) then
   call start_watch_actual(watches, (/clock/),err)
else
   call start_watch_actual(watches,default_clocks,err)
end if

end subroutine start_watch_ss
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!                 STOP_WATCH
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_watch_actual(watch,clock,err)

character(len=*),parameter :: ident_stop_watch='M_stopwatch::stop_watch(3f): stops the specified clocks of the specified watches'

!----------------------------------------------------
! Dummy arguments

type (watch_pointer), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err

!----------------------------------------------------

!----------------------------------------------------
! Local variables:

type (watch_actual), pointer :: the_watch
real :: diff,readcpu,readusr,readsys
integer :: i,new_read,r,m,idiff

!----------------------------------------------------

!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! set the flags for which clocks to stop

call which_clocks(clock,"stop_watch",err)

! loop through the watches

do i=1,ubound(watch,dim=1)

   the_watch => watch(i)%ptr
   if (.not. associated(the_watch)) then
      call err_handler_watch(ERR_CREATE,"stop_watch","", "Watch not stopped.",err)
   else

! stop each flagged clock for this watch

      call system_cpu_time(readcpu,readusr,readsys)
      if (do_cpu) then
         select case (the_watch%status%cpu)
            case (STOPPED, PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"stop_watch",the_watch%name, "Watch's cpu clock not stopped.",err)
            case (RUNNING)
               diff = readcpu - the_watch%last_read%cpu
               the_watch%elapsed%cpu = the_watch%elapsed%cpu + diff
               the_watch%status%cpu = STOPPED
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"stop_watch",the_watch%name, "Watch's cpu clock not stopped.",err)
         end select
      end if
      if (do_user) then
         select case (the_watch%status%user)
            case (STOPPED, PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"stop_watch",the_watch%name, "Watch's user clock not stopped.",err)
            case (RUNNING)
               diff = readusr - the_watch%last_read%user
               the_watch%elapsed%user = the_watch%elapsed%user + diff
               the_watch%status%user = STOPPED
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"stop_watch",the_watch%name, "Watch's user clock not stopped.",err)
         end select
      end if
      if (do_sys) then
         select case (the_watch%status%sys)
            case (STOPPED, PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"stop_watch",the_watch%name, "Watch's sys clock not stopped.",err)
            case (RUNNING)
               diff = readsys - the_watch%last_read%sys
               the_watch%elapsed%sys = the_watch%elapsed%sys + diff
               the_watch%status%sys = STOPPED
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"stop_watch",the_watch%name, "Watch's sys clock not stopped.",err)
         end select
      end if
      if (do_wall) then
         select case (the_watch%status%wall)
            case (STOPPED, PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"stop_watch",the_watch%name, "Watch's wall clock not stopped.",err)
            case (RUNNING)
               call system_clock(count=new_read,count_rate=r,count_max=m)
               if (r==0) then
                  idiff = 0
               else
                  idiff = new_read-the_watch%last_read%wall
                  if (idiff < 0) then
                     idiff = idiff + m ! clock cycled
                  end if
               end if
               the_watch%elapsed%wall = the_watch%elapsed%wall + idiff
               the_watch%status%wall = STOPPED
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"stop_watch",the_watch%name, "Watch's wall clock not stopped.",err)
         end select
      end if
   end if
end do

end subroutine stop_watch_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
! Alternate forms for stop_watch
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_watch_ga(watch,clock,err)
!          -------------
type (watchgroup), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
type (watch_list), pointer :: list_entry
integer :: erralloc, i
if (associated(watch%head)) then
   allocate(watches(watch%wgsize),stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_ALLOC,"stop_watch","", "Watches not stopped.",err)
   else
      list_entry => watch%head
      i = 0
      do
         if (.not. associated(list_entry)) then
            exit
         end if
         i=i+1
         watches(i)%ptr => list_entry%this_watch
         list_entry => list_entry%next
      end do
      call stop_watch_actual(watches,clock,err)
      deallocate(watches,stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_DEALLOC,"stop_watch","", "Watches stopped, but further problems may develop.",err)
      end if
   end if
else
   if (present(err)) then
      err = 0
   end if
end if
end subroutine stop_watch_ga
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_watch_gs(watch,clock,err)
!          -------------
type (watchgroup), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call stop_watch_ga(watch, (/clock/),err)
else
   call stop_watch_ga(watch,default_clocks,err)
end if
end subroutine stop_watch_gs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_watch_aa(watch,clock,err)
!          -------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"stop_watch","", "Watches not stopped.",err)
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call stop_watch_actual(watches,clock,err)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"stop_watch","", "Watches stopped, but further problems may develop.",err)
end if
end subroutine stop_watch_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_watch_as(watch,clock,err)
!          -------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call stop_watch_aa(watch, (/clock/),err)
else
   call stop_watch_aa(watch,default_clocks,err)
end if
end subroutine stop_watch_as
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_watch_sa(watch,clock,err)
!          -------------
type (watchtype), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
call stop_watch_actual(watches,clock,err)
end subroutine stop_watch_sa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stop_watch_ss(watch,clock,err)
!          -------------
type (watchtype), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
if (present(clock)) then
   call stop_watch_actual(watches, (/clock/),err)
else
   call stop_watch_actual(watches,default_clocks,err)
end if
end subroutine stop_watch_ss
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!-------------------------------------------------------------------
!                 RESET_WATCH
!-------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine reset_watch_actual(watch,clock,err)
!          ------------------
character(len=*),parameter :: ident_reset_watch='M_stopwatch::reset_watch(3f): resets a M_StopWatch watch to 0.0'

!----------------------------------------------------
! This routine resets the specified clocks of the specified watches to 0.0
!----------------------------------------------------

!----------------------------------------------------
! Dummy arguments

type (watch_pointer), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err

!----------------------------------------------------
!----------------------------------------------------
! Local variables:
type (watch_actual), pointer :: the_watch
real :: readcpu,readusr,readsys
integer :: i
!----------------------------------------------------
!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! set the flags for which clocks to reset

call which_clocks(clock,"reset_watch",err)

! loop through the watches

do i=1,ubound(watch,dim=1)

   the_watch => watch(i)%ptr
   if (.not. associated(the_watch)) then
      call err_handler_watch(ERR_CREATE,"reset_watch","", "Watch not reset.",err)
   else

! reset each flagged clock for this watch

      call system_cpu_time(readcpu,readusr,readsys)
      if (do_cpu) then
         select case (the_watch%status%cpu)
            case (STOPPED)
               the_watch%elapsed%cpu = 0.0
            case (RUNNING)
               the_watch%elapsed%cpu = 0.0
               the_watch%last_read%cpu = readcpu
            case (PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"reset_watch",the_watch%name, "Watch's cpu clock not reset.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"reset_watch",the_watch%name, "Watch's cpu clock not reset.",err)
         end select
      end if
      if (do_user) then
         select case (the_watch%status%user)
            case (STOPPED)
               the_watch%elapsed%user = 0.0
            case (RUNNING)
               the_watch%elapsed%user = 0.0
               the_watch%last_read%user = readusr
            case (PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"reset_watch",the_watch%name, "Watch's user clock not reset.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"reset_watch",the_watch%name, "Watch's user clock not reset.",err)
         end select
      end if
      if (do_sys) then
         select case (the_watch%status%sys)
            case (STOPPED)
               the_watch%elapsed%sys = 0.0
            case (RUNNING)
               the_watch%elapsed%sys = 0.0
               the_watch%last_read%sys = readsys
            case (PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"reset_watch",the_watch%name, "Watch's sys clock not reset.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"reset_watch",the_watch%name, "Watch's sys clock not reset.",err)
         end select
      end if
      if (do_wall) then
         select case (the_watch%status%wall)
            case (STOPPED)
               the_watch%elapsed%wall = 0
            case (RUNNING)
               the_watch%elapsed%wall = 0
               call system_clock(count=the_watch%last_read%wall)
            case (PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"reset_watch",the_watch%name, "Watch's wall clock not reset.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"reset_watch",the_watch%name, "Watch's wall clock not reset.",err)
         end select
      end if
   end if
end do

end subroutine reset_watch_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
! Alternate forms for reset_watch
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine reset_watch_ga(watch,clock,err)

type (watchgroup), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
type (watch_list), pointer :: list_entry
integer :: erralloc, i

if (associated(watch%head)) then
   allocate(watches(watch%wgsize),stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_ALLOC,"reset_watch","", "Watches not reset.",err)
   else
      list_entry => watch%head
      i = 0
      do
         if (.not. associated(list_entry)) then
            exit
         end if
         i=i+1
         watches(i)%ptr => list_entry%this_watch
         list_entry => list_entry%next
      end do
      call reset_watch_actual(watches,clock,err)
      deallocate(watches,stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_DEALLOC,"reset_watch","", "Watches reset, but further problems may develop.",err)
      end if
   end if
else
   if (present(err)) then
      err = 0
   end if
end if
end subroutine reset_watch_ga
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine reset_watch_gs(watch,clock,err)

type (watchgroup), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call reset_watch_ga(watch, (/clock/),err)
else
   call reset_watch_ga(watch,default_clocks,err)
end if
end subroutine reset_watch_gs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine reset_watch_aa(watch,clock,err)
!          --------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"reset_watch","", "Watches not reset.",err)
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call reset_watch_actual(watches,clock,err)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"reset_watch","", "Watches reset, but further problems may develop.",err)
end if
end subroutine reset_watch_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine reset_watch_as(watch,clock,err)
!          --------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call reset_watch_aa(watch, (/clock/),err)
else
   call reset_watch_aa(watch,default_clocks,err)
end if
end subroutine reset_watch_as
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine reset_watch_sa(watch,clock,err)
!          --------------
type (watchtype), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
call reset_watch_actual(watches,clock,err)
end subroutine reset_watch_sa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine reset_watch_ss(watch,clock,err)
!          --------------
type (watchtype), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
if (present(clock)) then
   call reset_watch_actual(watches, (/clock/),err)
else
   call reset_watch_actual(watches,default_clocks,err)
end if
end subroutine reset_watch_ss
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!-------------------------------------------------------------------
!                 PAUSE_WATCH
!-------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!          ------------------
subroutine pause_watch_actual(watch,clock,err)
!          ------------------
character(len=*),parameter :: ident_pause_watch='M_stopwatch::pause_watch(3f): pauses a M_StopWatch watch'

!----------------------------------------------------
! This routine pauses the specified clocks of the specified watches.
!----------------------------------------------------

!----------------------------------------------------
! Dummy arguments

type (watch_pointer), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err

!----------------------------------------------------

!----------------------------------------------------
! Local variables:

type (watch_actual), pointer :: the_watch
real :: diff,readcpu,readusr,readsys
integer :: i,new_read,r,m,idiff

!----------------------------------------------------

!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! set the flags for which clocks to pause

call which_clocks(clock,"pause_watch",err)

! loop through the watches

do i=1,ubound(watch,dim=1)

   the_watch => watch(i)%ptr
   if (.not. associated(the_watch)) then
      call err_handler_watch(ERR_CREATE,"pause_watch","", "Watch not paused.",err)
   else

! pause each flagged clock for this watch

      call system_cpu_time(readcpu,readusr,readsys)
      if (do_cpu) then
         select case (the_watch%status%cpu)
            case (STOPPED)
            case (RUNNING)
               diff = readcpu - the_watch%last_read%cpu
               the_watch%elapsed%cpu = the_watch%elapsed%cpu + diff
               the_watch%status%cpu = PAUSED
            case (PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"pause_watch",the_watch%name, "Watch's cpu clock not paused.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"pause_watch",the_watch%name, "Watch's cpu clock not paused.",err)
         end select
      end if
      if (do_user) then
         select case (the_watch%status%user)
            case (STOPPED)
            case (RUNNING)
               diff = readusr - the_watch%last_read%user
               the_watch%elapsed%user = the_watch%elapsed%user + diff
               the_watch%status%user = PAUSED
            case (PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"pause_watch",the_watch%name, "Watch's user clock not paused.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"pause_watch",the_watch%name, "Watch's user clock not paused.",err)
         end select
      end if
      if (do_sys) then
         select case (the_watch%status%sys)
            case (STOPPED)
            case (RUNNING)
               diff = readsys - the_watch%last_read%sys
               the_watch%elapsed%sys = the_watch%elapsed%sys + diff
               the_watch%status%sys = PAUSED
            case (PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"pause_watch",the_watch%name, "Watch's sys clock not paused.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"pause_watch",the_watch%name, "Watch's sys clock not paused.",err)
         end select
      end if
      if (do_wall) then
         select case (the_watch%status%wall)
            case (STOPPED)
            case (RUNNING)
               call system_clock(count=new_read,count_rate=r,count_max=m)
               if (r==0) then
                  idiff = 0
               else
                  idiff = new_read-the_watch%last_read%wall
                  if (idiff < 0) then
                     idiff = idiff + m ! clock cycled
                  end if
               end if
               the_watch%elapsed%wall = the_watch%elapsed%wall + idiff
               the_watch%status%wall = PAUSED
            case (PAUSED)
               call err_handler_watch(ERR_BAD_STATE,"pause_watch",the_watch%name, "Watch's wall clock not paused.",err)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"pause_watch",the_watch%name, "Watch's wall clock not paused.",err)
         end select
      end if
   end if
end do

end subroutine pause_watch_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!----------------------------------------------------
! Alternate forms for pause_watch
!----------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine pause_watch_ga(watch,clock,err)
!          --------------
type (watchgroup), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
type (watch_list), pointer :: list_entry
integer :: erralloc, i
if (associated(watch%head)) then
   allocate(watches(watch%wgsize),stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_ALLOC,"pause_watch","", "Watches not paused.",err)
   else
      list_entry => watch%head
      i = 0
      do
         if (.not. associated(list_entry)) then
            exit
         end if
         i=i+1
         watches(i)%ptr => list_entry%this_watch
         list_entry => list_entry%next
      end do
      call pause_watch_actual(watches,clock,err)
      deallocate(watches,stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_DEALLOC,"pause_watch","", "Watches paused, but further problems may develop.",err)
      end if
   end if
else
   if (present(err)) then
      err = 0
   end if
end if
end subroutine pause_watch_ga
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine pause_watch_gs(watch,clock,err)
!          --------------
type (watchgroup), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call pause_watch_ga(watch, (/clock/),err)
else
   call pause_watch_ga(watch,default_clocks,err)
end if
end subroutine pause_watch_gs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine pause_watch_aa(watch,clock,err)
!          --------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"pause_watch","", "Watches not paused.",err)
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call pause_watch_actual(watches,clock,err)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"pause_watch","", "Watches paused, but further problems may develop.",err)
end if
end subroutine pause_watch_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine pause_watch_as(watch,clock,err)
!          --------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call pause_watch_aa(watch, (/clock/),err)
else
   call pause_watch_aa(watch,default_clocks,err)
end if
end subroutine pause_watch_as
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine pause_watch_sa(watch,clock,err)
!          --------------
type (watchtype), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
call pause_watch_actual(watches,clock,err)
end subroutine pause_watch_sa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine pause_watch_ss(watch,clock,err)
!          --------------
type (watchtype), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
if (present(clock)) then
   call pause_watch_actual(watches, (/clock/),err)
else
   call pause_watch_actual(watches,default_clocks,err)
end if
end subroutine pause_watch_ss
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!-------------------------------------------------------------------
!                 END_PAUSE_WATCH
!-------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!          ----------------------
subroutine end_pause_watch_actual(watch,clock,err)
!          ----------------------
character(len=*),parameter :: ident_end_pause_watch='M_stopwatch::end_pause_watch(3f): resumes a paused M_StopWatch watch'

!----------------------------------------------------
! This routine ends the pause of the specified clocks of the specified watches.
!----------------------------------------------------

!----------------------------------------------------
! Dummy arguments

type (watch_pointer), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err

!----------------------------------------------------

!----------------------------------------------------
! Local variables:

type (watch_actual), pointer :: the_watch
real :: readcpu,readusr,readsys
integer :: i

!----------------------------------------------------

!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! set the flags for which clocks to end_pause

call which_clocks(clock,"end_pause_watch",err)

! loop through the watches

do i=1,ubound(watch,dim=1)

   the_watch => watch(i)%ptr
   if (.not. associated(the_watch)) then
      call err_handler_watch(ERR_CREATE,"end_pause_watch","", "Watch not end_paused.",err)
   else

! end_pause each flagged clock for this watch

      call system_cpu_time(readcpu,readusr,readsys)
      if (do_cpu) then
         select case (the_watch%status%cpu)
            case (STOPPED)
            case (RUNNING)
               call err_handler_watch(ERR_BAD_STATE,"end_pause_watch",the_watch%name, "Watch's cpu clock remains paused.",err)
            case (PAUSED)
               the_watch%last_read%cpu = readcpu
               the_watch%status%cpu = RUNNING
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"end_pause_watch",the_watch%name, "Watch's cpu clock remains paused.",err)
         end select
      end if
      if (do_user) then
         select case (the_watch%status%user)
            case (STOPPED)
            case (RUNNING)
               call err_handler_watch(ERR_BAD_STATE,"end_pause_watch",the_watch%name, "Watch's user clock remains paused.",err)
            case (PAUSED)
               the_watch%last_read%user = readusr
               the_watch%status%user = RUNNING
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"end_pause_watch",the_watch%name, "Watch's user clock remains paused.",err)
         end select
      end if
      if (do_sys) then
         select case (the_watch%status%sys)
            case (STOPPED)
            case (RUNNING)
               call err_handler_watch(ERR_BAD_STATE,"end_pause_watch",the_watch%name, "Watch's sys clock remains paused.",err)
            case (PAUSED)
               the_watch%last_read%sys = readsys
               the_watch%status%sys = RUNNING
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"end_pause_watch",the_watch%name, "Watch's sys clock remains paused.",err)
         end select
      end if
      if (do_wall) then
         select case (the_watch%status%wall)
            case (STOPPED)
            case (RUNNING)
               call err_handler_watch(ERR_BAD_STATE,"end_pause_watch",the_watch%name, "Watch's wall clock remains paused.",err)
            case (PAUSED)
               call system_clock(count=the_watch%last_read%wall)
               the_watch%status%wall = RUNNING
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"end_pause_watch",the_watch%name, "Watch's wall clock remains paused.",err)
         end select
      end if
   end if
end do

end subroutine end_pause_watch_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!----------------------------------------------------
! Alternate forms for end_pause_watch
!----------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine end_pause_watch_ga(watch,clock,err)
!          ------------------
type (watchgroup), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
type (watch_list), pointer :: list_entry
integer :: erralloc, i
if (associated(watch%head)) then
   allocate(watches(watch%wgsize),stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_ALLOC,"end_pause_watch","", "Watches remain paused.",err)
   else
      list_entry => watch%head
      i = 0
      do
         if (.not. associated(list_entry)) then
            exit
         end if
         i=i+1
         watches(i)%ptr => list_entry%this_watch
         list_entry => list_entry%next
      end do
      call end_pause_watch_actual(watches,clock,err)
      deallocate(watches,stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_DEALLOC,"end_pause_watch","", "Watches resumed, but further problems may develop.",err)
      end if
   end if
else
   if (present(err)) then
      err = 0
   end if
end if
end subroutine end_pause_watch_ga
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine end_pause_watch_gs(watch,clock,err)
!          ------------------
type (watchgroup), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call end_pause_watch_ga(watch, (/clock/),err)
else
   call end_pause_watch_ga(watch,default_clocks,err)
end if
end subroutine end_pause_watch_gs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

subroutine end_pause_watch_aa(watch,clock,err)
!          ------------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"end_pause_watch","", "Watches remain paused.",err)
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call end_pause_watch_actual(watches,clock,err)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"end_pause_watch","", "Watches resumed, but further problems may develop.",err)
end if
end subroutine end_pause_watch_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

subroutine end_pause_watch_as(watch,clock,err)
!          ------------------
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
if (present(clock)) then
   call end_pause_watch_aa(watch, (/clock/),err)
else
   call end_pause_watch_aa(watch,default_clocks,err)
end if
end subroutine end_pause_watch_as
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

subroutine end_pause_watch_sa(watch,clock,err)
!          ------------------
type (watchtype), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
call end_pause_watch_actual(watches,clock,err)
end subroutine end_pause_watch_sa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

subroutine end_pause_watch_ss(watch,clock,err)
!          ------------------
type (watchtype), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
if (present(clock)) then
   call end_pause_watch_actual(watches, (/clock/),err)
else
   call end_pause_watch_actual(watches,default_clocks,err)
end if
end subroutine end_pause_watch_ss
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!-------------------------------------------------------------------
!                 READ_WATCH
!-------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!          -----------------
subroutine read_watch_actual(read_result,watch,clock,err)
!          -----------------
character(len=*),parameter :: ident_read_watch='M_stopwatch::read_watch(3f): reads the values from a M_StopWatch watch'

!----------------------------------------------------
! This routine reads the specified clocks from the specified watches.
! Returns 0.0 on error conditions.
!----------------------------------------------------

!----------------------------------------------------
! Dummy arguments

real, pointer, dimension(:,:) :: read_result
type (watch_pointer), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err

!----------------------------------------------------

!----------------------------------------------------
! Local variables:

type (watch_actual), pointer :: the_watch
integer :: new_read,r,m,idiff,i,j,erralloc,clock_rate
real :: readcpu,readusr,readsys
real, target, save, dimension(1,1) :: zero_result

!----------------------------------------------------

!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! initialize the result to 0.0

allocate(read_result(size(watch),size(clock)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"read_watch","", "Fatal error may follow.",err)
   read_result => zero_result
end if
read_result = 0.0

! set the flags for which clocks to read

call which_clocks(clock,"read_watch",err)
call system_clock(count_rate=clock_rate)

! loop through the watches

do i=1,ubound(watch,dim=1)

   the_watch => watch(i)%ptr
   if (.not. associated(the_watch)) then
      call err_handler_watch(ERR_CREATE,"read_watch","", "Returning 0.0 for all clocks on this watch.",err)
   else

! read each flagged clock for this watch

      j=0
      call system_cpu_time(readcpu,readusr,readsys)
      if (do_cpu) then
         j=j+1
         select case (the_watch%status%cpu)
            case (STOPPED, PAUSED)
               read_result(i,j) = the_watch%elapsed%cpu
            case (RUNNING)
               read_result(i,j) = the_watch%elapsed%cpu + readcpu - the_watch%last_read%cpu
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"read_watch",the_watch%name, "Returning 0.0 for value of cpu clock.",err)
         end select
      end if
      if (do_user) then
         j=j+1
         select case (the_watch%status%user)
            case (STOPPED, PAUSED)
               read_result(i,j) = the_watch%elapsed%user
            case (RUNNING)
               read_result(i,j) = the_watch%elapsed%user + readusr - the_watch%last_read%user
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"read_watch",the_watch%name, "Returning 0.0 for value of user clock.",err)
         end select
      end if
      if (do_sys) then
         j=j+1
         select case (the_watch%status%sys)
            case (STOPPED, PAUSED)
               read_result(i,j) = the_watch%elapsed%sys
            case (RUNNING)
               read_result(i,j) = the_watch%elapsed%sys + readsys - the_watch%last_read%sys
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"read_watch",the_watch%name, "Returning 0.0 for value of sys clock.",err)
         end select
      end if
      if (do_wall) then
         j=j+1
         select case (the_watch%status%wall)
            case (STOPPED, PAUSED)
               read_result(i,j) = real(the_watch%elapsed%wall)/real(clock_rate)
            case (RUNNING)
               call system_clock(count=new_read,count_rate=r,count_max=m)
               if (r==0) then
                  idiff = 0
               else
                  idiff = new_read-the_watch%last_read%wall
                  if (idiff < 0) then
                     idiff = idiff + m ! clock cycled
                  end if
               end if
               read_result(i,j) = real(the_watch%elapsed%wall+idiff)/real(clock_rate)
            case (OMITTED)
            case default
               call err_handler_watch(ERR_UNK_STATE,"read_watch",the_watch%name,&
                    "Returning 0.0 for value of wall clock.",err)
         end select
      end if
   end if
end do

end subroutine read_watch_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!----------------------------------------------------
! Alternate forms for read_watch
!----------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine read_watch_aa(read_result,watch,clock,err)
!          -------------
real, pointer, dimension(:,:) :: read_result
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
real, target, dimension(1,1) :: zero_result
integer :: idiff,isize,erralloc,i
idiff = lbound(watch,dim=1)-1
isize = ubound(watch,dim=1)-lbound(watch,dim=1)+1
allocate(watches(isize),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"read_watch","", &
        "Returning 0.0 for watch values.",err)
   read_result => zero_result
   read_result = 0.0
else
   do i=1,isize
      watches(i)%ptr => watch(i+idiff)%ptr
   end do
   call read_watch_actual(read_result,watches,clock,err)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"read_watch","", &
       "Watches read, but further problems may develop.",err)
end if
end subroutine read_watch_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine read_watch_ax(read_result,watch,err)
!          -------------
real, pointer, dimension(:,:) :: read_result
type (watchtype), intent(in), dimension(:) :: watch
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
real, target, dimension(1,1) :: zero_result
real, pointer, dimension(:,:) :: double_pointer
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"read_watch","", &
        "Returning 0.0 for watch values.",err)
   read_result => zero_result
   read_result = 0.0
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call read_watch_actual(double_pointer,watches,default_clocks,err)
   read_result => double_pointer
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"read_watch","", &
       "Watches read, but further problems may develop.",err)
end if
end subroutine read_watch_ax
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine read_watch_as(read_result,watch,clock,err)
!        -------------
real, pointer, dimension(:) :: read_result
type (watchtype), intent(in), dimension(:) :: watch
character(len=*), intent(in) :: clock
integer, optional, intent(out) :: err
integer :: idiff,isize,erralloc,i
type (watch_pointer), allocatable, dimension(:) :: watches
real, pointer, dimension(:,:) :: double_pointer
real, target, save, dimension(1,1) :: zero_result
idiff = lbound(watch,dim=1)-1
isize = ubound(watch,dim=1)-lbound(watch,dim=1)+1
allocate(watches(isize),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"read_watch","", "Returning 0.0 for watch values.",err)
   read_result => zero_result(:,1)
   read_result = 0.0
else
   do i=1,isize
      watches(i)%ptr => watch(i+idiff)%ptr
   end do
   call read_watch_actual(double_pointer,watches, (/clock/) ,err)
   read_result => double_pointer(:,1)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"read_watch","", "Watches read, but further problems may develop.",err)
end if
end subroutine read_watch_as
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! read_watch_sa was split into two routine (the other being _sx) with and
! without clock instead of having clock be optional because when this was
! a function the dimension of the result was size(clock) or size(default_clocks)
! depending on the presence of clock, so the split was necessary to use
! real, dimension(size(clock)) :: read_watch_sa
! It was not changed back to a single routine because I realized it could be
! after I sent the code out to beta testers.

subroutine read_watch_sa(read_result,watch,clock,err)
!          -------------
real, pointer, dimension(:) :: read_result
type (watchtype), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
real, pointer, dimension(:,:) :: double_pointer
watches(1)%ptr => watch%ptr
call read_watch_actual(double_pointer,watches,clock,err)
read_result => double_pointer(1,:)
end subroutine read_watch_sa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine read_watch_sx(read_result,watch,err)
!          -------------
real, pointer, dimension(:) :: read_result
type (watchtype), intent(in) :: watch
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
real, pointer, dimension(:,:) :: double_pointer
watches(1)%ptr => watch%ptr
call read_watch_actual(double_pointer,watches,default_clocks,err)
read_result => double_pointer(1,:)
end subroutine read_watch_sx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine read_watch_ss(read_result,watch,clock,err)
!          -------------
real, intent(out) :: read_result
type (watchtype), intent(in) :: watch
character(len=*), intent(in) :: clock
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
real, pointer, dimension(:,:) :: double_pointer
watches(1)%ptr => watch%ptr
call read_watch_actual(double_pointer, watches, (/clock/),err)
read_result = double_pointer(1,1)
end subroutine read_watch_ss
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!-------------------------------------------------------------------
!                 PRINT_WATCH
!-------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!          ------------------
subroutine print_watch_actual(watch,clock,title,form,err)
!          ------------------
character(len=*),parameter :: ident_print_watch='M_stopwatch::print_watch(3f): prints the current value of a M_StopWatch'

!----------------------------------------------------
! This routine prints the specified clocks of the specified watches.
! A title for the output may be provided, or a default title will be
! printed if title is not present.
!----------------------------------------------------

!----------------------------------------------------
! Dummy arguments

type (watch_pointer), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
character(len=*), optional, intent(in) :: title, form
integer, optional, intent(out) :: err

!----------------------------------------------------

!----------------------------------------------------
! Local variables:

type (watch_actual), pointer :: the_watch
type (watchtype) :: toread
integer :: i, badunit, myerr
character(len=4) :: intfile
character(len=FORM_LEN) :: loc_form
real :: readval

!----------------------------------------------------

!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if
myerr=0

! determine the form for printed times

if (present(form)) then
   if (form=="sec" .or. form=="hh:mm:ss" .or. form=="[[hh:]mm:]ss") then
      loc_form = form
   else
      call err_handler_watch(ERR_FORM,"print_watch",form,"Using form 'sec'",myerr)
      loc_form = "sec"
   end if
else
   loc_form = default_form
end if

! print the title

if (present(title)) then
   if (len(title) /= 0) then
      write(unit=iounit,fmt="(a)",iostat=badunit) title
   else
      badunit = 0
   end if
else
   write(unit=iounit,fmt="("" Times printed by M_StopWatch:"")",iostat=badunit)
end if

if (badunit > 0) then
   write(unit=intfile,fmt="(i4)") iounit
   call err_handler_watch(ERR_IO,"print_watch",intfile, "Watch values not printed.",myerr)
   return
end if

! set the flags for which clocks to read

call which_clocks(clock,"print_watch",err)

! loop through the watches

do i=1,ubound(watch,dim=1)

   the_watch => watch(i)%ptr
   if (.not. associated(the_watch)) then
      call err_handler_watch(ERR_CREATE,"print_watch","", "Watch not printed.",myerr)
   else
      toread%ptr => watch(i)%ptr

      if (len_trim(the_watch%name) /= 0) then
         write(unit=iounit,fmt="(a2,a,a1)") "  ",trim(the_watch%name),":"
      end if
      write(unit=iounit,fmt="(a6)",advance="no") "      "
      if (do_cpu .and. the_watch%status%cpu /= OMITTED) then
!         write(unit=iounit,fmt="(a6)",advance="no") " cpu="
         call read_watch(readval,toread,"cpu",err)
         call print_time("  cpu=",readval,loc_form)
         call which_clocks(clock,"print_watch",err)
      end if
      if (do_user .and. the_watch%status%user /= OMITTED) then
!         write(unit=iounit,fmt="(a6)",advance="no") " user="
         call read_watch(readval,toread,"user",err)
         call print_time(" user=",readval,loc_form)
         call which_clocks(clock,"print_watch",err)
      end if
      if (do_sys .and. the_watch%status%sys /= OMITTED) then
!         write(unit=iounit,fmt="(a6)",advance="no") " sys="
         call read_watch(readval,toread,"sys",err)
         call print_time("  sys=",readval,loc_form)
         call which_clocks(clock,"print_watch",err)
      end if
      if (do_wall .and. the_watch%status%wall /= OMITTED) then
!         write(unit=iounit,fmt="(a6)",advance="no") " wall="
         call read_watch(readval,toread,"wall",err)
         call print_time(" wall=",readval,loc_form)
         call which_clocks(clock,"print_watch",err)
      end if
      write(unit=iounit,fmt=*)

   end if
end do

if (present(err)) then
   err=ior(err,myerr)
end if

end subroutine print_watch_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_time(str,time,form)
!          ----------

! prints the time under the specified format

character(len=*), intent(in) :: str
real, intent(in) :: time
character(len=*), intent(in) :: form

integer :: h,m,si
real :: sf

if (form=="sec") then
   write(unit=iounit,fmt="(a6,f8.2)",advance="no") str,time
else if (form=="hh:mm:ss") then
   h = time/3600.0
   m = (time-h*3600.0)/60.0
   si = time-h*3600.0-m*60.0
   sf = time-int(time)
   write(unit=iounit,fmt="(a6,i4,"":"",i2.2,"":"",i2.2,f3.2)",advance="no") str,h,m,si,sf
else if (form=="[[hh:]mm:]ss") then
   h = time/3600.0
   m = (time-h*3600.0)/60.0
   si = time-h*3600.0-m*60.0
   sf = time-int(time)
   if (h>0) then
      write(unit=iounit,fmt="(a6,i4,"":"",i2.2,"":"",i2.2,f3.2)",advance="no") str,h,m,si,sf
   else if (m>0) then
      write(unit=iounit,fmt="(a6,a5,i2,"":"",i2.2,f3.2)",advance="no") str,"     ",m,si,sf
   else
      write(unit=iounit,fmt="(a6,f13.2)",advance="no") str,time
   end if
end if

end subroutine print_time
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
! Alternate forms for print_watch
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine print_watch_ga(watch,clock,title,form,err)

type (watchgroup), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
character(len=*), optional, intent(in) :: title, form
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
type (watch_list), pointer :: list_entry
integer :: erralloc, i

if (associated(watch%head)) then
   allocate(watches(watch%wgsize),stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_ALLOC,"print_watch","", "Watch values not printed.",err)
   else
      list_entry => watch%head
      i = 0
      do
         if (.not. associated(list_entry)) then
            exit
         end if
         i=i+1
         watches(i)%ptr => list_entry%this_watch
         list_entry => list_entry%next
      end do
      call print_watch_actual(watches,clock,title,form,err)
      deallocate(watches,stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_DEALLOC,"print_watch","", "Watches printed, but further problems may develop.",err)
      end if
   end if
else
   if (present(err)) then
      err = 0
   end if
end if

end subroutine print_watch_ga
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_watch_gs(watch,clock,title,form,err)

type (watchgroup), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
character(len=*), optional, intent(in) :: title, form
integer, optional, intent(out) :: err

if (present(clock)) then
   call print_watch_ga(watch, (/clock/),title,form,err)
else
   call print_watch_ga(watch,default_clocks,title,form,err)
end if

end subroutine print_watch_gs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_watch_aa(watch,clock,title,form,err)

type (watchtype), intent(in), dimension(:) :: watch
character(len=*), intent(in), dimension(:) :: clock
character(len=*), optional, intent(in) :: title, form
integer, optional, intent(out) :: err

   type (watch_pointer), allocatable, dimension(:) :: watches
   integer :: erralloc,i

   allocate(watches(size(watch)),stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_ALLOC,"print_watch","", "Watch values not printed.",err)
   else
      do i=1,size(watch)
         watches(i)%ptr => watch(i)%ptr
      end do
      call print_watch_actual(watches,clock,title,form,err)
   end if

   deallocate(watches,stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_DEALLOC,"print_watch","", "Watches printed, but further problems may develop.",err)
   end if

end subroutine print_watch_aa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_watch_as(watch,clock,title,form,err)

type (watchtype), intent(in), dimension(:) :: watch
character(len=*), optional, intent(in) :: clock
character(len=*), optional, intent(in) :: title, form
integer, optional, intent(out) :: err

   if (present(clock)) then
      call print_watch_aa(watch, [clock],title,form,err)
   else
      call print_watch_aa(watch,default_clocks,title,form,err)
   end if

end subroutine print_watch_as
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_watch_sa(watch,clock,title,form,err)

type (watchtype), intent(in) :: watch
character(len=*), intent(in), dimension(:) :: clock
character(len=*), optional, intent(in) :: title, form
integer, optional, intent(out) :: err

   type (watch_pointer), dimension(1) :: watches

   watches(1)%ptr => watch%ptr
   call print_watch_actual(watches,clock,title,form,err)

end subroutine print_watch_sa
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_watch_ss(watch,clock,title,form,err)
!
type (watchtype), intent(in) :: watch
character(len=*), optional, intent(in) :: clock
character(len=*), optional, intent(in) :: title, form
integer, optional, intent(out) :: err

   type (watch_pointer), dimension(1) :: watches

   watches(1)%ptr => watch%ptr
   if (present(clock)) then
      call print_watch_actual(watches, (/clock/),title,form,err)
   else
      call print_watch_actual(watches,default_clocks,title,form,err)
   end if

end subroutine print_watch_ss
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!-------------------------------------------------------------------
!                 OPTION_STOPWATCH
!-------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!          ------------------
subroutine option_stopwatch_a(default_clock,io_unit_print,io_unit_error, print_errors,abort_errors,print_form,err)
!          ------------------
character(len=*),parameter :: ident_option_stopwatch='M_stopwatch::option_stopwatch(3f): sets M_StopWatch options'

!----------------------------------------------------
! This routine allows the user to set certain options.
!----------------------------------------------------

!----------------------------------------------------
! Dummy arguments

! DEC bug 1.  Can't have optional array argument here.
!character(len=*), optional, intent(in), dimension(:) :: default_clock
character(len=*), intent(in), dimension(:) :: default_clock
integer, optional, intent(in) :: io_unit_print, io_unit_error
logical, optional, intent(in) :: print_errors, abort_errors
character(len=*), optional, intent(in) :: print_form
integer, optional, intent(out) :: err

!----------------------------------------------------

!----------------------------------------------------
! Local variables:

logical :: isopen
character(len=8) :: iswrite
character(len=4) :: intfile
character(len=CLOCK_LEN), dimension(4) :: def_clocks
integer :: i,j,erralloc,r
real :: cpu,user,sys

!----------------------------------------------------

!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! default_clocks

! DEC bug 1. default_clock must be present
!if (present(default_clock)) then ! note dummy argument is default_clock and
                                 ! module variable is default_clocks (with an s)
   call system_cpu_time(cpu,user,sys)
   call system_clock(count_rate=r)
   j=0
   do i=1,size(default_clock)
      if (default_clock(i)=="none") then
         ! do nothing, this is for the DEC bug 1
      else if (default_clock(i)/="cpu" .and. default_clock(i)/="user" .and. &
         default_clock(i)/="sys" .and. default_clock(i)/="wall") then
         ! not a legal clock name
         call err_handler_watch(ERR_CLOCK,"option_stopwatch",default_clock(i), &
              "That clock not included in the default clocks.",err)
      else if ((user<0.0 .and. default_clock(i)=="user") .or. &
              (sys<0.0 .and. default_clock(i)=="sys") .or. &
              (cpu<0.0 .and. default_clock(i)=="cpu") .or. &
              (r==0 .and. default_clock(i)=="wall")) then
         ! clock not available on this system
         call err_handler_watch(ERR_CLOCK,"option_stopwatch",default_clock(i), &
              "That clock not included in the default clocks.",err)
      else
         j=j+1
         if (j<=4) then
            def_clocks(j)=default_clock(i)
         else
            j=4
            call err_handler_watch(ERR_TMC,"option_stopwatch",default_clock(i), &
              "That clock not included in the default clocks.",err)
         end if
      end if
   end do
   if (j>0) then
      if (allocated(default_clocks)) then
         deallocate(default_clocks,stat=erralloc)
         if (erralloc > 0) then
            call err_handler_watch(ERR_DEALLOC,"option_stopwatch","", "Further problems may develop.",err)
         end if
      end if
      allocate(default_clocks(j),stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_ALLOC,"option_stopwatch","", "There are no default clocks.",err)
      else
         default_clocks = def_clocks(1:j)
      end if
   end if
! DEC bug 1.  Got rid of this if statement
!end if

! error messages unit number

if (present(io_unit_error)) then
   inquire(unit=io_unit_error,opened=isopen,write=iswrite)
   if(.not. isopen .or. iswrite /= "YES") then
      write(unit=intfile,fmt="(i4)") io_unit_error
      call err_handler_watch(ERR_IO,"option_stopwatch",intfile, "I/O unit number for errors not reset as requested.",err)
   else
      errunit = io_unit_error
   end if
end if

! printed output unit number

if (present(io_unit_print)) then
   inquire(unit=io_unit_print,opened=isopen,write=iswrite)
   if(.not. isopen .or. iswrite /= "YES") then
      write(unit=intfile,fmt="(i4)") io_unit_print
      call err_handler_watch(ERR_IO,"option_stopwatch",intfile, "I/O unit number for printing not reset as requested.",err)
   else
      iounit = io_unit_print
   end if
end if

! behavior on errors

if (present(print_errors)) then
   errprint = print_errors
end if
if (present(abort_errors)) then
   errabort = abort_errors
end if

! form for printing time

if (present(print_form)) then
   if (print_form=="sec" .or. print_form=="hh:mm:ss" .or.  print_form=="[[hh:]mm:]ss") then
      default_form = print_form
   else
      call err_handler_watch(ERR_FORM,"option_stopwatch",print_form, "Default not changed.",err)
   end if
end if

end subroutine option_stopwatch_a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!----------------------------------------------------
! Alternate forms for option_stopwatch
!----------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine option_stopwatch_s(default_clock,io_unit_print,io_unit_error, print_errors,abort_errors,print_form,err)
! DEC bug 1.  Make default_clock optional here instead of as an array
!character(len=*), intent(in) :: default_clock
character(len=*), optional, intent(in) :: default_clock
integer, optional, intent(in) :: io_unit_print, io_unit_error
logical, optional, intent(in) :: print_errors, abort_errors
character(len=*), optional, intent(in) :: print_form
integer, optional, intent(out) :: err
! DEC bug 1.  Should only have the first case here.
if (present(default_clock)) then
   call option_stopwatch_a((/default_clock/),io_unit_print,io_unit_error, print_errors,abort_errors,print_form,err)
else
   call option_stopwatch_a((/"none"/),io_unit_print,io_unit_error, print_errors,abort_errors,print_form,err)
end if
end subroutine option_stopwatch_s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!                 INQUIRY_STOPWATCH
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine inquiry_stopwatch(default_clock,io_unit_print,io_unit_error, &
                   print_errors,abort_errors,print_form,cpu_avail,user_avail, &
                   sys_avail,wall_avail,cpu_prec,wall_prec,version,err)
!          -----------------
character(len=*),parameter :: ident_inquiry_stopwatch='M_stopwatch::inquiry_stopwatch(3f): returns M_StopWatch options and system'

!----------------------------------------------------
! This routine allows the user to determine the value of options set by
! option_stopwatch and certain implementation/system dependent values.
!----------------------------------------------------
!----------------------------------------------------
! Dummy arguments

character(len=*), optional, intent(out), dimension(:) :: default_clock
integer, optional, intent(out) :: io_unit_print, io_unit_error
logical, optional, intent(out) :: print_errors, abort_errors
character(len=*), optional, intent(out) :: print_form
logical, optional, intent(out) :: cpu_avail, user_avail, sys_avail, wall_avail
real, optional, intent(out) :: cpu_prec, wall_prec
character(len=*), optional, intent(out) :: version
integer, optional, intent(out) :: err

!----------------------------------------------------
!----------------------------------------------------
! Local variables:

integer :: r
real :: cpu,cpu2,user,sys

!----------------------------------------------------
!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=0
end if

! default_clocks

if (present(default_clock)) then
   if (.not. allocated(default_clocks)) then
      call option_stopwatch(default_clock=(/"cpu ","user","sys ","wall"/),err=err)
   end if

   default_clock = "    "
   default_clock(1:ubound(default_clocks,dim=1)) = default_clocks
end if

! i/o unit numbers

if (present(io_unit_error)) then
   io_unit_error = errunit
end if
if (present(io_unit_print)) then
   io_unit_print = iounit
end if

! behavior on errors

if (present(print_errors)) then
   print_errors = errprint
end if
if (present(abort_errors)) then
   abort_errors = errabort
end if

! format for printing time

if (present(print_form)) then
   print_form = default_form
end if

! available clocks

call system_cpu_time(cpu,user,sys)

if (present(cpu_avail)) then
   if (cpu < 0.0) then
      cpu_avail = .false.
   else
      cpu_avail = .true.
   end if
end if

if (present(user_avail)) then
   if (user < 0.0) then
      user_avail = .false.
   else
      user_avail = .true.
   end if
end if

if (present(sys_avail)) then
   if (sys < 0.0) then
      sys_avail = .false.
   else
      sys_avail = .true.
   end if
end if

if (present(wall_avail)) then
   call system_clock(count_rate=r)
   if (r == 0) then
      wall_avail = .false.
   else
      wall_avail = .true.
   end if
end if

! cpu precision, by calling system_cpu_time until the cpu clock changes

if (present(cpu_prec)) then
   if (cpu >= 0.0) then
      call system_cpu_time(cpu,user,sys)
      call system_cpu_time(cpu2,user,sys)
      do
         if (cpu2 /= cpu) then
            exit
         end if
         call system_cpu_time(cpu2,user,sys)
      end do
      cpu_prec = cpu2 - cpu
   else
      cpu_prec = 0.0
   end if
end if

! wall clock precision

if (present(wall_prec)) then
   call system_clock(count_rate=r)
   if (r == 0) then
      wall_prec = 0.0
   else
      wall_prec = 1.0/r
   end if
end if

! M_StopWatch version number

if (present(version)) then
   version = sw_version
end if

end subroutine inquiry_stopwatch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine create_watchgroup_actual(watch,handle,err)

character(len=*),parameter :: ident_create_watchgroup='M_stopwatch::create_watchgroup(3f): creates a M_StopWatch watch group'

! handle should not be an optional argument.  It must be present.  It was made
! optional to deal with F's requirement that nonoptional arguments do not
! follow optional arguments, and to keep upward compatibility with 1) a watch
! need not be provided and 2) argument order.  This should not cause confusion
! on the user's part because 1) the documentation will not indicate handle is
! optional, and 2) calling this routine without handle is meaningless
type (watch_pointer), optional, intent(in), dimension(:) :: watch
type (watchgroup), optional, intent(out) :: handle
integer, optional, intent(out) :: err

type (watch_list), pointer :: list_entry
integer :: i,erralloc
integer :: itemp ! change for 0.8.1; see explanation below

! Creates a new watch group and returns a handle for it.  If watch is
! present then the group will initially contain the given watches; otherwise
! the group will initially be empty.

if (present(err)) then
   err=0
end if

if (.not. present(handle)) then ! just in case it gets called without handle
   return
end if

! desired:  if the group already exists (associated(head)==.true.)
! then either destroy_watchgroup or set an error and don"t create.
! Unfortunately, I cannot test associated(head) because head will (correctly)
! have an undefined association status the first time a watch is passed to
! create_watchgroup

if (present(watch)) then
   handle%wgsize = ubound(watch,dim=1)
   allocate(handle%head,stat=erralloc)
   if (erralloc > 0) then
      call err_handler_watch(ERR_ALLOC,"create_watchgroup","", "Group not created.",err)
      return
   end if
   list_entry => handle%head
   do i=1,ubound(watch,dim=1)-1
      if (.not. associated(watch(i)%ptr)) then
         call err_handler_watch(ERR_CREATE,"create_watchgroup","", "Group not created.",err)
         handle%wgsize = 0
         nullify(handle%head)
         return
      else
         list_entry%this_watch => watch(i)%ptr
         allocate(list_entry%next,stat=erralloc)
         if (erralloc > 0) then
            call err_handler_watch(ERR_ALLOC,"create_watchgroup","", "Group not created.",err)
            handle%wgsize = 0
            nullify(handle%head)
            return
         else
            list_entry => list_entry%next
         end if
      end if
   end do
! this is the only change for version 0.8.1
! put ubound in temporary because of PSR VAST bug
   itemp = ubound(watch,dim=1)
   list_entry%this_watch => watch(itemp)%ptr
!   list_entry%this_watch => watch(ubound(watch,dim=1))%ptr
   nullify(list_entry%next)
else
   handle%wgsize = 0
   nullify(handle%head)
end if

end subroutine create_watchgroup_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!----------------------------------------------------
! Alternate forms for create_watchgroup
!----------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine create_watchgroup_a(watch,handle,err)
!          -------------------
type (watchtype), intent(in), dimension(:) :: watch
type (watchgroup), intent(out) :: handle
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"create_watchgroup","", "Group not created.",err)
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call create_watchgroup_actual(watches,handle,err)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"create_watchgroup","", "Group created, but further problems may develop.",err)
end if

end subroutine create_watchgroup_a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine create_watchgroup_s(watch,handle,err)
!          -------------------
! handle should not be optional.  see create_watchgroup_actual
type (watchtype), optional, intent(in) :: watch
type (watchgroup), optional, intent(out) :: handle
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
if (present(watch)) then
   watches(1)%ptr => watch%ptr
   call create_watchgroup_actual(watches,handle,err)
else
   call create_watchgroup_actual(handle=handle,err=err)
end if
end subroutine create_watchgroup_s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine destroy_watchgroup(handle,err)

character(len=*),parameter :: ident_destroy_watchgroup='M_stopwatch::destroy_watchgroup(3f): destroys a M_StopWatch watch group'

type (watchgroup), intent(in out) :: handle
integer, optional, intent(out) :: err

integer :: erralloc

if (present(err)) then
   err=0
end if

if (associated(handle%head)) then
   if (handle%wgsize > 1) then
      call free_watch_list(handle%head,err)
   end if
   if (handle%wgsize > 0) then
      deallocate(handle%head,stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_DEALLOC,"destroy_watchgroup","", "Group destroyed, but there may be a memory leak.",err)
      end if
      nullify(handle%head)
   end if
   handle%wgsize = 0
end if

end subroutine destroy_watchgroup
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
recursive subroutine free_watch_list(list_entry,err)
!                    ---------------
type (watch_list), intent(in out) :: list_entry
integer, optional, intent(out) :: err

integer :: erralloc

if (present(err)) then
   err=0
end if

if (associated(list_entry%next%next)) then
   call free_watch_list(list_entry%next,err)
end if
deallocate(list_entry%next,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"destroy_watchgroup","", "Group being destroyed, but there may be a memory leak.",err)
end if
nullify(list_entry%next)

end subroutine free_watch_list
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
!                 JOIN_GROUP
!-----------------------------------------------------------------------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine join_watchgroup_actual(watch,handle,err)
!          ----------------------
character(len=*),parameter :: ident_join_watchgroup='M_stopwatch::join_watchgroup(3f): adds a M_StopWatch watch to a watch'
type (watch_pointer), intent(in), dimension(:) :: watch
type (watchgroup), intent(in out) :: handle
integer, optional, intent(out) :: err

type (watch_list), pointer :: list_entry
integer :: i,lolim,erralloc

! Adds watches to a watch group

if (present(err)) then
   err=0
end if

if (.not. associated(handle%head)) then  ! create the first entry for an empty group
   if (.not. associated(watch(1)%ptr)) then
      call err_handler_watch(ERR_CREATE,"join_watchgroup","", "No watches added to group.",err)
      return
   else
      allocate(handle%head,stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_ALLOC,"join_watchgroup","", "Watch(es) not added to group.",err)
         return
      end if
      handle%wgsize = 1
      handle%head%this_watch => watch(1)%ptr
      nullify(handle%head%next)
      lolim = 2
   end if
else
   lolim = 1
end if

do i=lolim,ubound(watch,dim=1) ! add watches to the front of the linked list
   if (.not. associated(watch(i)%ptr)) then
      call err_handler_watch(ERR_CREATE,"join_watchgroup","", "Watch not added to group.",err)
   else
      list_entry => handle%head
      nullify(handle%head)
      allocate(handle%head,stat=erralloc)
      if (erralloc > 0) then
         call err_handler_watch(ERR_ALLOC,"join_watchgroup","", "Watch not added to group.",err)
         return
      end if
      handle%wgsize = handle%wgsize + 1
      handle%head%this_watch => watch(i)%ptr
      handle%head%next => list_entry
   end if
end do

end subroutine join_watchgroup_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!----------------------------------------------------
! Alternate forms for join_watchgroup
!----------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine join_watchgroup_a(watch,handle,err)
!          -----------------
type (watchtype), intent(in), dimension(:) :: watch
type (watchgroup), intent(in out) :: handle
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"join_watchgroup","", "Watches not added to group.",err)
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call join_watchgroup_actual(watches,handle,err)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"join_watchgroup","", "Watches added to group, but further problems may develop.",err)
end if
end subroutine join_watchgroup_a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine join_watchgroup_s(watch,handle,err)
!          -----------------
type (watchtype), intent(in) :: watch
type (watchgroup), intent(in out) :: handle
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
call join_watchgroup_actual(watches,handle,err)

end subroutine join_watchgroup_s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!                 LEAVE_GROUP
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine leave_watchgroup_actual(watch,handle,err)

character(len=*),parameter :: ident_leave_watchgroup='M_stopwatch::leave_watchgroup(3f): removes a watch from a watchgroup'

type (watch_pointer), intent(in), dimension(:) :: watch
type (watchgroup), intent(in out) :: handle
integer, optional, intent(out) :: err

type (watch_list), pointer :: list_entry, parent
integer :: i, erralloc

! Removes watches from a watch group

if (present(err)) then
   err=0
end if

! loop through the watches

do i=1,ubound(watch,dim=1)

! find the watch

   nullify(parent)
   list_entry => handle%head
   do
      if (.not. associated(list_entry)) then
         exit
      end if
      if (associated(list_entry%this_watch,watch(i)%ptr)) then
         exit
      end if
      parent => list_entry
      list_entry => list_entry%next
   end do
   if (.not. associated(list_entry)) then
      call err_handler_watch(ERR_GROUP,"leave_watchgroup",watch(i)%ptr%name, "Watch not removed from group.",err)
   else
      if (.not. associated(parent)) then ! remove head of list
         handle%head => list_entry%next
         deallocate(list_entry,stat=erralloc)
      else ! remove one from the interior of the list or tail
         parent%next => list_entry%next
         deallocate(list_entry,stat=erralloc)
      end if
      if (erralloc > 0) then
         call err_handler_watch(ERR_DEALLOC,"leave_watchgroup","", "Watch removed from group, but there may be a memory leak.",err)
      end if
      handle%wgsize = handle%wgsize - 1
   end if

end do

end subroutine leave_watchgroup_actual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
! Alternate forms for leave_watchgroup
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine leave_watchgroup_a(watch,handle,err)
!          ------------------
type (watchtype), intent(in), dimension(:) :: watch
type (watchgroup), intent(in out) :: handle
integer, optional, intent(out) :: err
type (watch_pointer), allocatable, dimension(:) :: watches
integer :: erralloc,i
allocate(watches(size(watch)),stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_ALLOC,"leave_watchgroup","", "Watches not removed from group.",err)
else
   do i=1,size(watch)
      watches(i)%ptr => watch(i)%ptr
   end do
   call leave_watchgroup_actual(watches,handle,err)
end if
deallocate(watches,stat=erralloc)
if (erralloc > 0) then
   call err_handler_watch(ERR_DEALLOC,"leave_watchgroup","", "Watches removed from group, but further problems may develop.",err)
end if
end subroutine leave_watchgroup_a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine leave_watchgroup_s(watch,handle,err)
!          ------------------
type (watchtype), intent(in) :: watch
type (watchgroup), intent(in out) :: handle
integer, optional, intent(out) :: err
type (watch_pointer), dimension(1) :: watches
watches(1)%ptr => watch%ptr
call leave_watchgroup_actual(watches,handle,err)
end subroutine leave_watchgroup_s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!       ROUTINES THAT ARE NOT DIRECTLY CALLABLE BY THE USER
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine which_clocks_a(clock,from,err)
!----------------------------------------------------
! This routine sets the flags do_cpu, do_user, do_sys and do_wall to indicate
! which clocks should be operated on.  clock is an array of clock names
! ("cpu", "user", "sys", and "wall") for which to set the flag true
!----------------------------------------------------
!----------------------------------------------------
! Dummy arguments
character(len=*), intent(in), dimension(:) :: clock
character(len=*), intent(in) :: from
integer, intent(in out), optional :: err
!----------------------------------------------------
!----------------------------------------------------
! Local variables:
integer :: i, r
real :: cpu, user, sys
!----------------------------------------------------
!----------------------------------------------------
! Begin executable code

call system_cpu_time(cpu,user,sys)
call system_clock(count_rate=r)
do_cpu = .false.
do_user = .false.
do_sys = .false.
do_wall = .false.
do i=1,size(clock)
   select case (clock(i))
      case("cpu")
         if (cpu >= 0.0) then
            do_cpu = .true.
         else
            call err_handler_watch(ERR_CLOCK,from,"cpu", "Requested action not performed on cpu clock.",err)
         end if
      case("user")
         if (user >= 0.0) then
            do_user = .true.
         else
            call err_handler_watch(ERR_CLOCK,from,"user", "Requested action not performed on user clock.",err)
         end if
      case("sys")
         if (sys >= 0.0) then
            do_sys = .true.
         else
            call err_handler_watch(ERR_CLOCK,from,"sys", "Requested action not performed on sys clock.",err)
         end if
      case("wall")
         if (r /= 0) then
            do_wall = .true.
         else
            call err_handler_watch(ERR_CLOCK,from,"wall", "Requested action not performed on wall clock.",err)
         end if
      case default
         call err_handler_watch(ERR_CLOCK,from,clock(i), "Requested action not performed on this clock.",err)
   end select
end do

end subroutine which_clocks_a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
! Alternate forms for which_clocks
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine which_clocks_s(clock,from,err)
character(len=*), intent(in) :: clock
character(len=*), intent(in) :: from
integer, intent(in out), optional :: err
   call which_clocks_a( (/ clock /) ,from,err)
end subroutine which_clocks_s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!          -----------------
subroutine err_handler_watch(code,routine,string1,string2,err)
!          -----------------

!----------------------------------------------------
! This routine handles errors by a user specified (through option_stopwatch)
! combination of printing messages, setting the error code, and aborting.
!----------------------------------------------------

!----------------------------------------------------
! Dummy arguments

integer, intent(in) :: code
character(len=*), intent(in) :: routine,string1, string2
integer, intent(in out), optional :: err

integer :: errio
!----------------------------------------------------
! Begin executable code

if (present(err)) then
   err=ior(err,code)
end if

if (errprint) then
  write(unit=errunit,fmt=*,iostat=errio) " "
  if (errio > 0) then  ! failed to write to error file; try printed output file
     write(unit=iounit,fmt=*,iostat=errio) " "
     if (errio == 0) then
       write(unit=iounit,fmt=*) "     ***** WARNING from M_StopWatch error handler *****"
       write(unit=iounit,fmt=*) "     Unable to write to error I/O unit ",errunit
       write(unit=iounit,fmt=*) "     Switching error output to printed output unit ",iounit
       write(unit=iounit,fmt=*) "     ************************************************"
       write(unit=iounit,fmt=*)
     end if
     errunit = iounit
  end if

  if (errio == 0) then ! if failed on iounit, too, then give up

     write(unit=errunit,fmt=*) "     ***** WARNING from M_StopWatch routine ",routine," *****"
     select case (code)
      case(ERR_CLOCK)
          write(unit=errunit,fmt=*) "     Invalid clock type ",trim(string1),"."
      case(ERR_BAD_STATE)
          write(unit=errunit,fmt=*) "     Watch named ",trim(string1)," is in the wrong state for this operation."
      case(ERR_UNK_STATE)
          write(unit=errunit,fmt=*) "     Watch named ",trim(string1)," is in an unknown state."
      case(ERR_IO)
          write(unit=errunit,fmt=*) "     I/O unit number ",trim(string1)," is not open for writing."
      case(ERR_TMC)
          write(unit=errunit,fmt=*) "     Too many clocks specified at clock type ",trim(string1)
      case(ERR_C2LONG)
          write(unit=errunit,fmt=*) "     Character string '",trim(string1),"' too long."
      case(ERR_ALLOC)
          write(unit=errunit,fmt=*) "     Failed to allocate required memory."
      case(ERR_NAMES)
          write(unit=errunit,fmt=*) "     Number of names is not equal to number of watches."
      case(ERR_GROUP)
          write(unit=errunit,fmt=*) "     Watch named ",trim(string1)," not found in given group."
      case(ERR_DEALLOC)
          write(unit=errunit,fmt=*) "     Error occurred while deallocating memory."
      case(ERR_CREATE)
          write(unit=errunit,fmt=*) "     Watch needs to be created."
      case(ERR_FORM)
          write(unit=errunit,fmt=*) "     Illegal output form "",trim(string1),""."
      case default
          write(unit=errunit,fmt=*) "     Error handler called with invalid error code."
     end select
     write(unit=errunit,fmt=*) "     ",string2
     write(unit=errunit,fmt=*) "     ******************************************************"
     write(unit=errunit,fmt=*)
  end if
end if

if (errabort) then
   if (errprint) then
      write(unit=errunit,fmt=*) "Program aborting: user request to abort on errors in M_StopWatch."
   end if
   stop
end if

end subroutine err_handler_watch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_stopwatch()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_create_watch_aa()
   call test_create_watch_as()
   call test_create_watch_sa()
   call test_create_watch_ss()
   call test_create_watchgroup_a()
   call test_create_watchgroup_s()
   call test_destroy_watch_aa()
   call test_destroy_watch_as()
   call test_destroy_watch_sa()
   call test_destroy_watch_ss()
   call test_destroy_watchgroup()
   call test_end_pause_watch_aa()
   call test_end_pause_watch_as()
   call test_end_pause_watch_ga()
   call test_end_pause_watch_gs()
   call test_end_pause_watch_sa()
   call test_end_pause_watch_ss()
   call test_inquiry_stopwatch()
   call test_join_watchgroup_a()
   call test_join_watchgroup_s()
   call test_leave_watchgroup_a()
   call test_leave_watchgroup_s()
   call test_option_stopwatch_a()
   call test_option_stopwatch_s()
   call test_pause_watch_aa()
   call test_pause_watch_as()
   call test_pause_watch_ga()
   call test_pause_watch_gs()
   call test_pause_watch_sa()
   call test_pause_watch_ss()
   call test_print_watch_aa()
   call test_print_watch_as()
   call test_print_watch_ga()
   call test_print_watch_gs()
   call test_print_watch_sa()
   call test_print_watch_ss()
   call test_read_watch_aa()
   call test_read_watch_as()
   call test_read_watch_ax()
   call test_read_watch_sa()
   call test_read_watch_ss()
   call test_read_watch_sx()
   call test_reset_watch_aa()
   call test_reset_watch_as()
   call test_reset_watch_ga()
   call test_reset_watch_gs()
   call test_reset_watch_sa()
   call test_reset_watch_ss()
   call test_start_watch_aa()
   call test_start_watch_as()
   call test_start_watch_ga()
   call test_start_watch_gs()
   call test_start_watch_sa()
   call test_start_watch_ss()
   call test_stop_watch_aa()
   call test_stop_watch_as()
   call test_stop_watch_ga()
   call test_stop_watch_gs()
   call test_stop_watch_sa()
   call test_stop_watch_ss()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_create_watch_aa()

   call unit_check_start('create_watch_aa',msg='')
   !!call unit_check('create_watch_aa', 0.eq.0, 'checking', 100)
   call unit_check_done('create_watch_aa',msg='')
end subroutine test_create_watch_aa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_create_watch_as()

   call unit_check_start('create_watch_as',msg='')
   !!call unit_check('create_watch_as', 0.eq.0, 'checking', 100)
   call unit_check_done('create_watch_as',msg='')
end subroutine test_create_watch_as
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_create_watch_sa()

   call unit_check_start('create_watch_sa',msg='')
   !!call unit_check('create_watch_sa', 0.eq.0, 'checking', 100)
   call unit_check_done('create_watch_sa',msg='')
end subroutine test_create_watch_sa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_create_watch_ss()

   call unit_check_start('create_watch_ss',msg='')
   !!call unit_check('create_watch_ss', 0.eq.0, 'checking', 100)
   call unit_check_done('create_watch_ss',msg='')
end subroutine test_create_watch_ss
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_create_watchgroup_a()

   call unit_check_start('create_watchgroup_a',msg='')
   !!call unit_check('create_watchgroup_a', 0.eq.0, 'checking', 100)
   call unit_check_done('create_watchgroup_a',msg='')
end subroutine test_create_watchgroup_a
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_create_watchgroup_s()

   call unit_check_start('create_watchgroup_s',msg='')
   !!call unit_check('create_watchgroup_s', 0.eq.0, 'checking', 100)
   call unit_check_done('create_watchgroup_s',msg='')
end subroutine test_create_watchgroup_s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_destroy_watch_aa()

   call unit_check_start('destroy_watch_aa',msg='')
   !!call unit_check('destroy_watch_aa', 0.eq.0, 'checking', 100)
   call unit_check_done('destroy_watch_aa',msg='')
end subroutine test_destroy_watch_aa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_destroy_watch_as()

   call unit_check_start('destroy_watch_as',msg='')
   !!call unit_check('destroy_watch_as', 0.eq.0, 'checking', 100)
   call unit_check_done('destroy_watch_as',msg='')
end subroutine test_destroy_watch_as
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_destroy_watch_sa()

   call unit_check_start('destroy_watch_sa',msg='')
   !!call unit_check('destroy_watch_sa', 0.eq.0, 'checking', 100)
   call unit_check_done('destroy_watch_sa',msg='')
end subroutine test_destroy_watch_sa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_destroy_watch_ss()

   call unit_check_start('destroy_watch_ss',msg='')
   !!call unit_check('destroy_watch_ss', 0.eq.0, 'checking', 100)
   call unit_check_done('destroy_watch_ss',msg='')
end subroutine test_destroy_watch_ss
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_destroy_watchgroup()

   call unit_check_start('destroy_watchgroup',msg='')
   !!call unit_check('destroy_watchgroup', 0.eq.0, 'checking', 100)
   call unit_check_done('destroy_watchgroup',msg='')
end subroutine test_destroy_watchgroup
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_end_pause_watch_aa()

   call unit_check_start('end_pause_watch_aa',msg='')
   !!call unit_check('end_pause_watch_aa', 0.eq.0, 'checking', 100)
   call unit_check_done('end_pause_watch_aa',msg='')
end subroutine test_end_pause_watch_aa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_end_pause_watch_as()

   call unit_check_start('end_pause_watch_as',msg='')
   !!call unit_check('end_pause_watch_as', 0.eq.0, 'checking', 100)
   call unit_check_done('end_pause_watch_as',msg='')
end subroutine test_end_pause_watch_as
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_end_pause_watch_ga()

   call unit_check_start('end_pause_watch_ga',msg='')
   !!call unit_check('end_pause_watch_ga', 0.eq.0, 'checking', 100)
   call unit_check_done('end_pause_watch_ga',msg='')
end subroutine test_end_pause_watch_ga
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_end_pause_watch_gs()

   call unit_check_start('end_pause_watch_gs',msg='')
   !!call unit_check('end_pause_watch_gs', 0.eq.0, 'checking', 100)
   call unit_check_done('end_pause_watch_gs',msg='')
end subroutine test_end_pause_watch_gs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_end_pause_watch_sa()

   call unit_check_start('end_pause_watch_sa',msg='')
   !!call unit_check('end_pause_watch_sa', 0.eq.0, 'checking', 100)
   call unit_check_done('end_pause_watch_sa',msg='')
end subroutine test_end_pause_watch_sa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_end_pause_watch_ss()

   call unit_check_start('end_pause_watch_ss',msg='')
   !!call unit_check('end_pause_watch_ss', 0.eq.0, 'checking', 100)
   call unit_check_done('end_pause_watch_ss',msg='')
end subroutine test_end_pause_watch_ss
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inquiry_stopwatch()

   call unit_check_start('inquiry_stopwatch',msg='')
   !!call unit_check('inquiry_stopwatch', 0.eq.0, 'checking', 100)
   call unit_check_done('inquiry_stopwatch',msg='')
end subroutine test_inquiry_stopwatch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_join_watchgroup_a()

   call unit_check_start('join_watchgroup_a',msg='')
   !!call unit_check('join_watchgroup_a', 0.eq.0, 'checking', 100)
   call unit_check_done('join_watchgroup_a',msg='')
end subroutine test_join_watchgroup_a
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_join_watchgroup_s()

   call unit_check_start('join_watchgroup_s',msg='')
   !!call unit_check('join_watchgroup_s', 0.eq.0, 'checking', 100)
   call unit_check_done('join_watchgroup_s',msg='')
end subroutine test_join_watchgroup_s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_leave_watchgroup_a()

   call unit_check_start('leave_watchgroup_a',msg='')
   !!call unit_check('leave_watchgroup_a', 0.eq.0, 'checking', 100)
   call unit_check_done('leave_watchgroup_a',msg='')
end subroutine test_leave_watchgroup_a
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_leave_watchgroup_s()

   call unit_check_start('leave_watchgroup_s',msg='')
   !!call unit_check('leave_watchgroup_s', 0.eq.0, 'checking', 100)
   call unit_check_done('leave_watchgroup_s',msg='')
end subroutine test_leave_watchgroup_s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_option_stopwatch_a()

   call unit_check_start('option_stopwatch_a',msg='')
   !!call unit_check('option_stopwatch_a', 0.eq.0, 'checking', 100)
   call unit_check_done('option_stopwatch_a',msg='')
end subroutine test_option_stopwatch_a
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_option_stopwatch_s()

   call unit_check_start('option_stopwatch_s',msg='')
   !!call unit_check('option_stopwatch_s', 0.eq.0, 'checking', 100)
   call unit_check_done('option_stopwatch_s',msg='')
end subroutine test_option_stopwatch_s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pause_watch_aa()

   call unit_check_start('pause_watch_aa',msg='')
   !!call unit_check('pause_watch_aa', 0.eq.0, 'checking', 100)
   call unit_check_done('pause_watch_aa',msg='')
end subroutine test_pause_watch_aa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pause_watch_as()

   call unit_check_start('pause_watch_as',msg='')
   !!call unit_check('pause_watch_as', 0.eq.0, 'checking', 100)
   call unit_check_done('pause_watch_as',msg='')
end subroutine test_pause_watch_as
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pause_watch_ga()

   call unit_check_start('pause_watch_ga',msg='')
   !!call unit_check('pause_watch_ga', 0.eq.0, 'checking', 100)
   call unit_check_done('pause_watch_ga',msg='')
end subroutine test_pause_watch_ga
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pause_watch_gs()

   call unit_check_start('pause_watch_gs',msg='')
   !!call unit_check('pause_watch_gs', 0.eq.0, 'checking', 100)
   call unit_check_done('pause_watch_gs',msg='')
end subroutine test_pause_watch_gs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pause_watch_sa()

   call unit_check_start('pause_watch_sa',msg='')
   !!call unit_check('pause_watch_sa', 0.eq.0, 'checking', 100)
   call unit_check_done('pause_watch_sa',msg='')
end subroutine test_pause_watch_sa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pause_watch_ss()

   call unit_check_start('pause_watch_ss',msg='')
   !!call unit_check('pause_watch_ss', 0.eq.0, 'checking', 100)
   call unit_check_done('pause_watch_ss',msg='')
end subroutine test_pause_watch_ss
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_watch_aa()

   call unit_check_start('print_watch_aa',msg='')
   !!call unit_check('print_watch_aa', 0.eq.0, 'checking', 100)
   call unit_check_done('print_watch_aa',msg='')
end subroutine test_print_watch_aa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_watch_as()

   call unit_check_start('print_watch_as',msg='')
   !!call unit_check('print_watch_as', 0.eq.0, 'checking', 100)
   call unit_check_done('print_watch_as',msg='')
end subroutine test_print_watch_as
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_watch_ga()

   call unit_check_start('print_watch_ga',msg='')
   !!call unit_check('print_watch_ga', 0.eq.0, 'checking', 100)
   call unit_check_done('print_watch_ga',msg='')
end subroutine test_print_watch_ga
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_watch_gs()

   call unit_check_start('print_watch_gs',msg='')
   !!call unit_check('print_watch_gs', 0.eq.0, 'checking', 100)
   call unit_check_done('print_watch_gs',msg='')
end subroutine test_print_watch_gs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_watch_sa()

   call unit_check_start('print_watch_sa',msg='')
   !!call unit_check('print_watch_sa', 0.eq.0, 'checking', 100)
   call unit_check_done('print_watch_sa',msg='')
end subroutine test_print_watch_sa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_watch_ss()

   call unit_check_start('print_watch_ss',msg='')
   !!call unit_check('print_watch_ss', 0.eq.0, 'checking', 100)
   call unit_check_done('print_watch_ss',msg='')
end subroutine test_print_watch_ss
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_watch_aa()

   call unit_check_start('read_watch_aa',msg='')
   !!call unit_check('read_watch_aa', 0.eq.0, 'checking', 100)
   call unit_check_done('read_watch_aa',msg='')
end subroutine test_read_watch_aa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_watch_as()

   call unit_check_start('read_watch_as',msg='')
   !!call unit_check('read_watch_as', 0.eq.0, 'checking', 100)
   call unit_check_done('read_watch_as',msg='')
end subroutine test_read_watch_as
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_watch_ax()

   call unit_check_start('read_watch_ax',msg='')
   !!call unit_check('read_watch_ax', 0.eq.0, 'checking', 100)
   call unit_check_done('read_watch_ax',msg='')
end subroutine test_read_watch_ax
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_watch_sa()

   call unit_check_start('read_watch_sa',msg='')
   !!call unit_check('read_watch_sa', 0.eq.0, 'checking', 100)
   call unit_check_done('read_watch_sa',msg='')
end subroutine test_read_watch_sa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_watch_ss()

   call unit_check_start('read_watch_ss',msg='')
   !!call unit_check('read_watch_ss', 0.eq.0, 'checking', 100)
   call unit_check_done('read_watch_ss',msg='')
end subroutine test_read_watch_ss
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_read_watch_sx()

   call unit_check_start('read_watch_sx',msg='')
   !!call unit_check('read_watch_sx', 0.eq.0, 'checking', 100)
   call unit_check_done('read_watch_sx',msg='')
end subroutine test_read_watch_sx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reset_watch_aa()

   call unit_check_start('reset_watch_aa',msg='')
   !!call unit_check('reset_watch_aa', 0.eq.0, 'checking', 100)
   call unit_check_done('reset_watch_aa',msg='')
end subroutine test_reset_watch_aa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reset_watch_as()

   call unit_check_start('reset_watch_as',msg='')
   !!call unit_check('reset_watch_as', 0.eq.0, 'checking', 100)
   call unit_check_done('reset_watch_as',msg='')
end subroutine test_reset_watch_as
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reset_watch_ga()

   call unit_check_start('reset_watch_ga',msg='')
   !!call unit_check('reset_watch_ga', 0.eq.0, 'checking', 100)
   call unit_check_done('reset_watch_ga',msg='')
end subroutine test_reset_watch_ga
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reset_watch_gs()

   call unit_check_start('reset_watch_gs',msg='')
   !!call unit_check('reset_watch_gs', 0.eq.0, 'checking', 100)
   call unit_check_done('reset_watch_gs',msg='')
end subroutine test_reset_watch_gs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reset_watch_sa()

   call unit_check_start('reset_watch_sa',msg='')
   !!call unit_check('reset_watch_sa', 0.eq.0, 'checking', 100)
   call unit_check_done('reset_watch_sa',msg='')
end subroutine test_reset_watch_sa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reset_watch_ss()

   call unit_check_start('reset_watch_ss',msg='')
   !!call unit_check('reset_watch_ss', 0.eq.0, 'checking', 100)
   call unit_check_done('reset_watch_ss',msg='')
end subroutine test_reset_watch_ss
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_start_watch_aa()

   call unit_check_start('start_watch_aa',msg='')
   !!call unit_check('start_watch_aa', 0.eq.0, 'checking', 100)
   call unit_check_done('start_watch_aa',msg='')
end subroutine test_start_watch_aa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_start_watch_as()

   call unit_check_start('start_watch_as',msg='')
   !!call unit_check('start_watch_as', 0.eq.0, 'checking', 100)
   call unit_check_done('start_watch_as',msg='')
end subroutine test_start_watch_as
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_start_watch_ga()

   call unit_check_start('start_watch_ga',msg='')
   !!call unit_check('start_watch_ga', 0.eq.0, 'checking', 100)
   call unit_check_done('start_watch_ga',msg='')
end subroutine test_start_watch_ga
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_start_watch_gs()

   call unit_check_start('start_watch_gs',msg='')
   !!call unit_check('start_watch_gs', 0.eq.0, 'checking', 100)
   call unit_check_done('start_watch_gs',msg='')
end subroutine test_start_watch_gs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_start_watch_sa()

   call unit_check_start('start_watch_sa',msg='')
   !!call unit_check('start_watch_sa', 0.eq.0, 'checking', 100)
   call unit_check_done('start_watch_sa',msg='')
end subroutine test_start_watch_sa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_start_watch_ss()

   call unit_check_start('start_watch_ss',msg='')
   !!call unit_check('start_watch_ss', 0.eq.0, 'checking', 100)
   call unit_check_done('start_watch_ss',msg='')
end subroutine test_start_watch_ss
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stop_watch_aa()

   call unit_check_start('stop_watch_aa',msg='')
   !!call unit_check('stop_watch_aa', 0.eq.0, 'checking', 100)
   call unit_check_done('stop_watch_aa',msg='')
end subroutine test_stop_watch_aa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stop_watch_as()

   call unit_check_start('stop_watch_as',msg='')
   !!call unit_check('stop_watch_as', 0.eq.0, 'checking', 100)
   call unit_check_done('stop_watch_as',msg='')
end subroutine test_stop_watch_as
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stop_watch_ga()

   call unit_check_start('stop_watch_ga',msg='')
   !!call unit_check('stop_watch_ga', 0.eq.0, 'checking', 100)
   call unit_check_done('stop_watch_ga',msg='')
end subroutine test_stop_watch_ga
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stop_watch_gs()

   call unit_check_start('stop_watch_gs',msg='')
   !!call unit_check('stop_watch_gs', 0.eq.0, 'checking', 100)
   call unit_check_done('stop_watch_gs',msg='')
end subroutine test_stop_watch_gs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stop_watch_sa()

   call unit_check_start('stop_watch_sa',msg='')
   !!call unit_check('stop_watch_sa', 0.eq.0, 'checking', 100)
   call unit_check_done('stop_watch_sa',msg='')
end subroutine test_stop_watch_sa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stop_watch_ss()

   call unit_check_start('stop_watch_ss',msg='')
   !!call unit_check('stop_watch_ss', 0.eq.0, 'checking', 100)
   call unit_check_done('stop_watch_ss',msg='')
end subroutine test_stop_watch_ss
!===================================================================================================================================
end subroutine test_suite_M_stopwatch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_stopwatch
