!>
!!##NAME
!!        today(1f) - [TIME] output current time for uses such as file suffixes.
!!##SYNOPSIS
!!
!!        today [format]
!!##DESCRIPTION
!!        Outputs the current date using the specified format. Typically used
!!        to generate a string to be used in building filenames containing
!!        date information.
!!##OPTIONS
!!        format   any allowable format for the fmtdate(3) routine. Enter
!!                 "-" to get a list on stdout. defaults to "YMD".
!!##EXAMPLE
!!
!!        Sample commands:
!!
!!         cp myfile myfile.`today`
!!         find . -ls >MANIFEST.`today epoch`
!!         mkdir `today YMDhms`
!!         today -  # show formatting options
!===================================================================================================================================

program today
use M_time, only : now, fmtdate_usage
implicit none
character(len=:),allocatable :: arguments
integer                      :: arguments_length
integer                      :: i
call get_command(length=arguments_length)              ! get command line length
allocate(character(len=arguments_length) :: arguments) ! allocate string big enough to hold command line
   call get_command(command=arguments)                 ! get command line as a string
   arguments=adjustl(arguments)                        ! JIC:: trim leading spaces just in case
   i=index(arguments,' ')                              ! assuming program name has no space in it
   if(i.ne.0)then                                      ! if there were arguments on the command use them
      if(trim(arguments(i+1:)).eq.'-')then
         call fmtdate_usage()                          ! see all formatting options
      else
         write(*,'(a)')now(arguments(i+1:))            ! display current date using format from command line
      endif
   else
      write(*,'(a)')now('YMD')                         ! display current date using a default format
   endif
   deallocate(arguments)                               ! JIC:: releasing string
end program today
