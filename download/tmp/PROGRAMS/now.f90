!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
program display_date
use M_kracken, only : kracken, lget, retrev, sget, dget     ! command line parameter cracking module
use M_time,    only : now,fmtdate_usage,fmtdate,days2sec,d2u,u2d,realtime,guessdate,j2d
use m_strings, only : string_to_values, isdigit, isspace, switch
implicit none
character(len=*),parameter     :: ident="@(#)now(1f): writes timestamp using specified syntax"
integer                        :: dat(8)=0
real                           :: rdat(8)=0
real(kind=realtime)            :: duration=0
character(len=:),allocatable   :: output
!character(len=1),allocatable   :: chars(:)
integer                        :: ierr, inums
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   call kracken('now',' -help .F. -version .F. -dat -date -jed -uet -test .false. -delta')  ! crack command line
   call help_version(lget('now_version'))                                    ! display version number if --version is present
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   if(lget('now_help'))then                                        ! display help text and exit if --help is present
      call usage()
      stop
   endif
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   if(lget('now_test'))then                                        ! try each format type
      call fmtdate_usage(3)
      stop
   endif
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   if (sget('now_uet').ne.' ')then
      dat=u2d(dget('now_uet'))                                     ! convert command option to UET number, convert to dat
   elseif (sget('now_jed').ne.' ')then
      dat=j2d(dget('now_jed'))                                     ! convert command option to JED number, convert to dat
   elseif (sget('now_dat').ne.' ')then
      dat=u2d()                                                    ! initialize DAT with current date and time to get time zone
      dat=[dat(1),1,1,dat(4),0,0,0,0]                              ! default is Jan 1st in current year and timezone 00:00:00
      call string_to_values(sget('now_dat'),size(dat),rdat,inums,' ,:/',ierr) ! convert string to array and overlay default values
      dat(1:inums)=nint(rdat(1:inums))

      ! if -date is all integer digits and whitespace and +- and only one - (for UTC value) then use it directly to load a DAT
      !chars=switch(trim(sget('now_dat')))
      !if( all(isdigit(chars).or.isspace(chars).or.chars.eq.'-'.or.chars.eq.'+' ) .and. count(chars.eq.'-').le.1 )then
      !else
      !   write(*,*)'*now* error: invalid characters in -dat input '//trim(sget('now_dat'))
      !endif

   elseif (sget('now_date').ne.' ')then                            ! convert command option to date string and try to guess date
      call guessdate(sget('now_date'),dat)
   else                                                            ! create DAT for current time
      dat=u2d()                                                    ! current time
   endif
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   duration=days2sec(sget('now_delta'))                            ! convert string to duration in seconds
   dat=u2d(d2u(dat)+duration)                                      ! convert DAT to UET, add duration, place back in DAT
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   output=fmtdate(dat,sget('now_oo'))                              ! create output string by applying format
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   write(*,'(a)')trim(output)                                      ! write output
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine usage()
!
character(len=*),parameter    :: ident="@(#)usage(3f,private): writes program help to stdout and exits"
character(len=80),allocatable :: text(:)
integer                       :: i
! NOTE: Without the type specification this constructor would have to specify all of the constants with the same character length.
text=[ character(len=len(text(1))) ::                                               &
&'NAME                                                                            ',&
&'   now(1)  - [TIME] print the date and time                                     ',&
&'                                                                                ',&
&'SYNOPSIS                                                                        ',&
&'   now [Format [-date date_str|-ued Unix_time|-jed Julian_Date|-dat date_vector]',&
&'       [-delta dd-hh:mm:ss]]|--help |--version|-test]                           ',&
&'                                                                                ',&
&'DESCRIPTION                                                                     ',&
&'   Report the current time or a Fortran date vector in a variety of formats.    ',&
&'   Julian dates, Unix Epoch time, weekdays, monthnames, ordinal days,           ',&
&'   AM/PM and iso-8601 week-numbering are supported by building a format         ',&
&'   string containing the desired macros.                                        ',&
&'OPTIONS                                                                         ',&
&'   Format  :                                                                    ',&
&'     This string, containing macro names or keywords, creates the format used   ',&
&'     to print the specified date.                                               ',&
&'                                                                                ',&
&'     The FORMAT string is expanded using the following macros:                  ',&
&'                                                                                ',&
&'CALL FMTDATE_USAGE                                                              ',&
&'                                                                                ',&
&'   -dat date_vector  :                                                          ',&
&'      A date vector is eight integers representing a date in the same manner as ',&
&'      the Fortran DATE_AND_TIME(3f) function:                                   ',&
&'          yyyy mm dd zone hh mm ss mss                                          ',&
&'      only numeric time zones are supported.                                    ',&
&'                                                                                ',&
&'      When present, the specified date is used instead of the current time.     ',&
&'                                                                                ',&
&'   -uet Unix_Epoch_Time  :                                                      ',&
&'      When present a value is used as the Unix Epoch Time. This date is         ',&
&'      is then adjusted using any -delta value and then printed using            ',&
&'      the specified format.                                                     ',&
&'                                                                                ',&
&'   -jed Julian_Date  :                                                          ',&
&'      When present a value is used as the Julian Ephemeris Date.                ',&
&'                                                                                ',&
&'   -delta dd-hh:mm:ss  :                                                        ',&
&'      Add the specified duration to the date.                                   ',&
&'                                                                                ',&
&'   -date date_str  :                                                            ',&
&'      The guessdate(3f) routine is used to try to convert a date description    ',&
&'      to a date vector. For the guess to work, dates must either be in the      ',&
&'      form YYYY-MM-DD or the order of numeric values must be ""dd yy yyy".      ',&
&'      Only four-digit years are supported. Month names are preferred over       ',&
&'      numeric values. See the guessdate(3f) documentation for further details.  ',&
&'                                                                                ',&
&'   -test :                                                                      ',&
&'      To list allowed macros use the -test switch.                              ',&
&'                                                                                ',&
&'   When present, the specified date is used instead of the current time.        ',&
&'EXAMPLES                                                                        ',&
&' Sample commands:                                                               ',&
&'                                                                                ',&
&'  now                                                                           ',&
&'    Friday, June 17th, 2016 03:22:53 PM UTC-0240                                ',&
&'                                                                                ',&
&'  now -delta  1-0:0:0  # Tomorrow                                               ',&
&'    Sunday, June 19th, 2016 11:32:26 AM UTC-0240                                ',&
&'                                                                                ',&
&'  now -delta -1-0:0:0  # Yesterday                                              ',&
&'    Friday, June 17th, 2016 11:32:43 AM UTC-0240                                ',&
&'                                                                                ',&
&'  now long -delta  7-0:0:0  # Next week                                         ',&
&'    Saturday, June 25th, 2016 11:32:57 AM UTC-04:00                             ',&
&'                                                                                ',&
&'  now The date is %Y/%M/%D %h:%m:%s  # user-specified formats using macros      ',&
&'    The date is 2009/08/10 00:33:48                                             ',&
&'                                                                                ',&
&'  now Y/M/D h:m:s # user-specified format with no % character                   ',&
&'    2009/08/10 00:33:48                                                         ',&
&'                                                                                ',&
&'  now year-month-day # user-specified format with no % with long keywords       ',&
&'  2016-07-29                                                                    ',&
&'                                                                                ',&
&'  now -dat 2016 07 23 -240 1 01 00 00  # alternate date                         ',&
&'  Saturday, July 23rd, 2016 1:01:00 AM UTC-0240                                 ',&
&'                                                                                ',&
&'  now -uet  1469250060                 # alternate Unix Epoch date              ',&
&'  now -date January 4th, 1999 10:20:30 # try to determine date from description.',&
&'                                                                                ',&
&'  now YEAR=%Y MONTH=%M DAY=%D                                                   ',&
&'    YEAR=2009 MONTH=08 DAY=10                                                   ',&
&'                                                                                ',&
&'  now HOUR=%h MINUTES=%m SECONDS=%s MILLISECONDS=%x                             ',&
&'    HOUR=01 MINUTES=18 SECONDS=44 MILLISECONDS=946                              ',&
&'                                                                                ',&
&'  # double-quotes are trickly (double them) to put in literally in this program:',&
&'  now ''""year-month-day"",""hour-minute-second""''                               ',&
&'     "2017-04-23","14-41-09"                                                    ',&
&'                                                                                ',&
&'  # quotes are easier to control using the single-letter macros(use %q and %Q): ',&
&'  now QY-M-DQ,Qh:m:sQ                                                           ',&
&'     "2017-04-23","14-41-09"                                                    ',&
&'                                                                                ',&
&'  now -test       # Show formatting options, handy way to look up macro names   ',&
&'                                                                                ',&
&'LIMITS                                                                          ',&
&'  See the M_time module description. Basically, A Gregorian Calendar is         ',&
&'  assumed, and Leap Seconds are not specifically accounted for.                 ',&
&'SEE ALSO                                                                        ',&
&'   calen(1), sec2days(1), days2sec(1)                                           ',&
&'                                                                                ']

do i=1,size(text)
   select case (text(i))
   case('CALL FMTDATE_USAGE')
      call fmtdate_usage(6)
   case default
      write(*,'(a)')trim(text(i))
   end select
enddo

stop
end subroutine usage
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
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        now(1f)>',&
'@(#)DESCRIPTION:    Report a date in a variety of formats>',&
'@(#)VERSION:        1.0, 2009>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COPYRIGHT:      Copyright (C) 2009 John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:04:09 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
end program display_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
