program touch
use M_kracken, only : kracken,lget,sgets, IPvalue, sget
use M_time,    only : d2u
use M_system,  only : system_utime, system_perror
use M_time,    only : guessdate, fmtdate
implicit none
character(len=IPvalue),allocatable :: filenames(:)
integer                            :: dat(8)
integer                            :: i
integer                            :: ios
integer                            :: lun
logical                            :: verbose
character(len=4096)                :: errmsg
logical                            :: ex,od
logical                            :: lstat
integer                            :: times(2)
character(len=:),allocatable       :: date

! define command arguments, default values and crack command line
   call kracken('touch','-date -version .f. -help .f. -verbose .f.')
   call help_usage(lget('touch_help'))                ! if -help option is present, display help text and exit
   call help_version(lget('touch_version'))           ! if -version option is present, display version text and exit
   verbose=lget('touch_verbose')
   filenames=sgets('touch_oo')
   date=sget('touch_date')

   if(date.eq.'')then
      call date_and_time(values=dat)
   else
      call guessdate(date,dat)
      if(verbose)then
         write(*,*)'FOR '//date//' GOT '//trim(fmtdate(dat))
         write(*,*)'DAT ARRAY ',dat
      endif
   endif
   times(1)= d2u(dat)
   times(2)= times(1)

   do i=1,size(filenames)
      ! ex, od always become defined unless an error condition occurs.
      inquire(file=filenames(i), exist=ex, opened=od, iostat=ios)
      open(file=filenames(i),newunit=lun,iostat=ios,iomsg=errmsg)
      if(ios.ne.0)then
         !!write(*,*)'*touch* ERROR on '//trim(filenames(i))//':'//trim(errmsg)
      endif
      close(unit=lun,iostat=ios)
      if(.not.ex.and.verbose)then
         write(*,'(a)')trim(filenames(i))//' created'
      endif
      lstat=system_utime(filenames(i),times)
      if(lstat)then
         if(verbose)then
            write(*,'(a)')trim(filenames(i))//' updated'
         endif
      else
          call system_perror('*touch-*:'//filenames(i))
      endif
   enddo
   if(verbose)then
      write(*,'(a)')"That's all Folks!"
   endif
contains
subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                                                                            ',&
'   touch-(1f) - [FUNIX:FILESYSTEM] change file access and modify timestamps to current time, creating file if necessary         ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   touch- [FILE... [ -date DATE]]|[ --help|--version|--verbose]                                                                 ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'                                                                                                                                ',&
'   Make sure specified filenames exist (by creating them as empty                                                               ',&
'   files) and change file access time to current time or specified                                                              ',&
'   time.                                                                                                                        ',&
'OPTIONS                                                                                                                         ',&
'   -date      Change the file timestamps to the specified date instead of                                                       ',&
'              the current time. Uses guessdate(3f) to read the date.                                                            ',&
'   --verbose  Display messages showing command progress                                                                         ',&
'   --help     Display help text and exit                                                                                        ',&
'   --version  Display version information and exit                                                                              ',&
'EXAMPLES                                                                                                                        ',&
'  Sample commands                                                                                                               ',&
'                                                                                                                                ',&
'   touch- *.f90                                                                                                                 ',&
'   touch- * -date 2000-01-02 10:20:30                                                                                           ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    touch-(1f) - [FUNIX:FILESYSTEM] change file access and modify timestamps to current time, creating file if necessary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    touch- [FILE... [ -date DATE]]|[ --help|--version|--verbose]
!!
!!##DESCRIPTION
!!
!!    Make sure specified filenames exist (by creating them as empty
!!    files) and change file access time to current time or specified
!!    time.
!!##OPTIONS
!!    -date      Change the file timestamps to the specified date instead of
!!               the current time. Uses guessdate(3f) to read the date.
!!    --verbose  Display messages showing command progress
!!    --help     Display help text and exit
!!    --version  Display version information and exit
!!##EXAMPLES
!!
!!   Sample commands
!!
!!    touch- *.f90
!!    touch- * -date 2000-01-02 10:20:30
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        touch-(1)>',&
'@(#)DESCRIPTION:    change file access timestamp to current time, creating file is necessary>',&
'@(#)VERSION:        1.0, 20180217>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2022-11-11 14:31:22 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program touch
