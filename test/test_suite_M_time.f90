program runtest
use M_msg
use M_verify
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
use M_verify, only : unit_check_level
use M_time
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_time()
contains
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_M_time
use M_verify, only : unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level
use,intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
use M_time
implicit none
integer :: dat(8)
integer :: ierr
character(len=*),parameter :: SAME='-library libGPF -section 3 -description'

!! no not use M_system version or will create a circular dependency
call put_environment_variable('TZ','America/New_York',ierr) ! some of the test values assume EST
call put_environment_variable('TZ','UTC+04:00',ierr) ! some of the test values assume EST

call unit_check_msg('M_time','This section contains unit tests for procedures in the M_time(3f) module.')

call unit_check_start('box_month      ',SAME//' "print specified month into character array" ')
call test_box_month()
call unit_check_start('d2j            ',SAME//' "Convert date array to Julian Date" ')
call test_d2j()
call unit_check_start('d2o            ',SAME//' "Converts date-time array to Ordinal day" ')
call test_d2o()
call unit_check_start('d2u            ',SAME//' "Convert date array to Unix Time" ')
call test_d2u()
call unit_check_start('d2w            ',SAME//' "Calculate iso-8601 Week-numbering year date yyyy-Www-d" ')
call test_d2w()
call unit_check_start('date_to_julian ',SAME//' "Converts Proleptic Gregorian date array to Julian Date" ')
call test_date_to_julian()
call unit_check_start('date_to_unix   ',SAME//' "Converts date array to Unix Time (UT starts at 0000 on 1 Jan. 1970, UTC)" ')
call test_date_to_unix()
call unit_check_start('days2sec       ',SAME//' "converts string D-HH:MM:SS to seconds from small to large" ')
call test_days2sec()
call unit_check_start('dow            ',SAME//' "Return the day of the week" ')
call test_dow()
call unit_check_start('easter         ',SAME//' "Determine month and day Easter falls on for given year" ')
call test_easter()
!!call unit_check_start('ephemeris      ',SAME//' "ephemeris position of planets for adjusting an equatorial telescope" ')
!!call test_ephemeris()
call unit_check_start('fmtdate        ',SAME//' "given date array return date as string using format" ')
call test_fmtdate()
call unit_check_start('fmtdate_usage  ',SAME//' "display macros recognized by fmtdate(3f)" ')
call test_fmtdate_usage()
call unit_check_start('guessdate      ',SAME//' "Reads in a date, in various formats" ')
call test_guessdate()
call unit_check_start('j2d            ',SAME//' "Convert Julian Date to date array" ')
call test_j2d()
call unit_check_start('julian_to_date ',SAME//' "Converts Julian Date to (year, month, day, hour, minute, second)" ')
call test_julian_to_date()
call unit_check_start('mo2d           ',SAME//' "return date array for beginning of given month name in specified year" ')
call test_mo2d()
call unit_check_start('mo2v           ',SAME//' "given month as name return month number (1-12) of that month" ')
call test_mo2v()
call unit_check_start('moon_fullness  ',SAME//' "return name for phase of moon for given date" ')
call test_moon_fullness()
call unit_check_start('now            ',SAME//' "return string representing current time given format" ')
call test_now()
call unit_check_start('now_ex         ',SAME//' "use of now(3f) outside of a module" ')
call test_now_ex()
call unit_check_start('o2d            ',SAME//' "given ordinal day of year return date array, Jan 1st=1" ')
call test_o2d()
call unit_check_start('ordinal_to_date',SAME//' "given ordinal day of year return date array, Jan 1st=1" ')
call test_ordinal_to_date()
call unit_check_start('phase_of_moon  ',SAME//' "percentage of moon phase from new to full" ')
call test_phase_of_moon()
call unit_check_start('sec2days       ',SAME//' "converts seconds to string D-HH:MM:SS" ')
call test_sec2days()
call unit_check_start('u2d            ',SAME//' "Convert Unix Time to date array" ')
call test_u2d()
call unit_check_start('unix_to_date   ',SAME//' "Converts Unix Time to date array" ')
call test_unix_to_date()
call unit_check_start('v2mo           ',SAME//' "returns the month name of a Common month" ')
call test_v2mo()
call unit_check_start('w2d            ',SAME//' "Given iso-8601 Week-numbering year date yyyy-Www-d calculate date" ')
call test_w2d()

contains
!===================================================================================================================================
subroutine put_environment_variable(name,value,status)

!  This is an private copy of the set_environment_variable routine(3f) routine from
!  M_system.FF that is duplicated in order to prevent a circular dependency.

! ident_33="@(#)M_system::put_environment_variable(3f): call setenv(3c) to set environment variable"

character(len=*)               :: NAME
character(len=*)               :: VALUE
integer, optional, intent(out) :: STATUS
integer                        :: loc_err

interface
   integer(kind=c_int) function c_setenv(c_name,c_VALUE) bind(C,NAME="setenv")
      import c_int, c_char
      character(kind=c_char)   :: c_name(*)
      character(kind=c_char)   :: c_VALUE(*)
   end function
end interface

   loc_err =  c_setenv(str2arr(trim(NAME)),str2arr(VALUE))
   if (present(STATUS)) STATUS = loc_err
end subroutine put_environment_variable
!===================================================================================================================================
pure function str2arr(string) result (array)

! ident_34="@(#)M_system::str2arr(3fp): function copies string to null terminated char array"

character(len=*),intent(in)     :: string
character(len=1,kind=c_char)    :: array(len(string)+1)
integer                         :: i

   do i = 1,len_trim(string)
      array(i) = string(i:i)
   enddo
   array(size(array))=c_null_char

end function str2arr
!===================================================================================================================================
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_date_to_julian()
!!use M_time,    only : date_to_julian, now, fmtdate, date_to_unix, realtime
implicit none
real(kind=realtime) :: julian
integer             :: ierr

   call unit_check_start('date_to_julian',msg='Checking Julian Date') ! assume if got here passed checks

   call date_to_julian( [1970, 1, 1,0, 0,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',abs(julian-2440587.5d0).lt.0.00001 ,msg="Dec 31st, 1969  8:00(2440587.5)")

   call date_to_julian( [1995, 1, 1,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2449719 ,msg="Jan  1st, 1995 12:00(2449719)")

   call date_to_julian( [1995,10,19,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450010, msg="Oct 19th, 1995 12:00(2450010)")

   call date_to_julian( [1995,12,31,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450083, msg="Dec 31st, 1995 12:00(2450083)")

   call date_to_julian( [1996, 1, 1,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450084, msg="Jan  1st, 1996 12:00(2450084)")

   call date_to_julian( [1996,12,31,0,12,0,0,0] ,julian,ierr)
   call unit_check('date_to_julian',int(julian).eq.2450449, msg="Dec 31th, 1996 12:00(2450449)")

   call unit_check_done('date_to_julian')

end subroutine test_date_to_julian
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_julian_to_date()
!!use M_time, only : julian_to_date, fmtdate, realtime
implicit none
real(kind=realtime)          :: juliandate
integer                      :: dat(8)
integer                      :: ierr
character(len=:),allocatable :: expected

   call unit_check_start('julian_to_date')

   juliandate=2457589.129d0                 ! set sample Julian Date
   call julian_to_date(juliandate,dat,ierr) ! create DAT array for this date
   expected='2016-07-19 11:05:45'
   call unit_check('julian_to_date',fmtdate(dat,'year-month-day hour:minute:second').eq.expected,&
          & juliandate,'==> EXPECTED ',expected,' GOT ',fmtdate(dat),'year-month-day hour:minute:second')

   call julian_to_date(juliandate-1.0d0,dat,ierr) ! go back one day
   expected='2016-07-18 11:05:45'
   call unit_check('julian_to_date',fmtdate(dat,'year-month-day hour:minute:second').eq.expected,&
          & juliandate,'==> EXPECTED ',expected,' GOT ',fmtdate(dat),'year-month-day hour:minute:second')

   call julian_to_date(juliandate+1.0d0,dat,ierr) ! go forward one day
   expected='2016-07-20 11:05:45'
   call unit_check('julian_to_date',fmtdate(dat,'year-month-day hour:minute:second').eq.expected,&
          & juliandate,'==> EXPECTED ',expected,' GOT ',fmtdate(dat),'year-month-day hour:minute:second')

   call unit_check_done('julian_to_date')

end subroutine test_julian_to_date
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_date_to_unix
real(kind=realtime)              :: unixtime
integer                          :: ierr

call unit_check_start('date_to_unix')

call date_to_unix([2017,03,29,-240,01,46,47,0],unixtime,ierr)
call unit_check('d2u',abs(unixtime-1490766407).lt.0.001 ,d2u([2017,03,29,-240,01,46,47,0]) )

call unit_check_done('date_to_unix')

end subroutine test_date_to_unix
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unix_to_date
call unit_check_start('unix_to_date')
call unit_check('unix_to_date',all([2017,03,29,-240,01,46,47,0].eq.u2d(1490766407)),msg=d2u([2017,03,29,-240,01,46,47,0]) )
call unit_check_done('unix_to_date')
end subroutine test_unix_to_date
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2o()
!!use M_time, only : d2o
implicit none
integer                      :: iday,iyear,omonth,oday,rday
integer                      :: i,dat(8)
character(len=40),parameter  :: tests(*)=[ &
   'ordinal  year  month  month_day  ',  &
   '100      2004  4      9          ',  &
   '100      2005  4      10         ',  &
   '100      2006  4      10         ',  &
   '100      2007  4      10         ',  &
   '100      2008  4      9          ',  &
   '100      2016  4      9          ']
character(len=40)            :: readme

   call unit_check_start('d2o')

    do i=2,size(tests)
       readme=tests(i)
       read(readme,*)iday,iyear,omonth,oday
       dat=o2d(iday,iyear)
       rday=d2o(dat)
       call unit_check('d2o',iday.eq.rday,msg=tests(i))
    enddo

   call unit_check_done('d2o')

end subroutine test_d2o
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ordinal_seconds()
!!use M_time, only : ordinal_seconds
implicit none
integer  :: rday

   call unit_check_start('ordinal_seconds')
   rday=ordinal_seconds()/(60*60*24)
   call unit_check('ordinal_seconds',rday.eq.d2o(),rday,d2o())
   call unit_check_done('ordinal_seconds')

end subroutine test_ordinal_seconds
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ordinal_to_date()
!!use M_time, only : o2d, ordinal_to_date, d2o
implicit none
integer                      :: iday,iyear,omonth,oday
integer                      :: i,dat(8)
character(len=40),parameter  :: tests(*)=[ &
   'ordinal  year  month  month_day  ',  &
   '100      2004  4      9          ',  &
   '100      2005  4      10         ',  &
   '100      2006  4      10         ',  &
   '100      2007  4      10         ',  &
   '100      2008  4      9          ',  &
   '100      2016  4      9          ']
character(len=40)            :: readme

   call unit_check_start('ordinal_to_date')

   do i=2,size(tests)
      readme=tests(i)
      read(readme,*)iday,iyear,omonth,oday
      call ordinal_to_date(iyear,iday,dat)
      call unit_check('ordinal_to_date',dat(2).eq.omonth.and.dat(3).eq.oday,'year',iyear,'ordinal',iday)
   enddo

   call unit_check_done('ordinal_to_date')

end subroutine test_ordinal_to_date
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_o2d()
!!use M_time, only : o2d, ordinal_to_date, d2o
implicit none
integer                      :: iday,iyear,omonth,oday
integer                      :: i,dat(8)
character(len=40),parameter  :: tests(*)=[ &
   'ordinal  year  month  month_day  ',  &
   '100      2004  4      9          ',  &
   '100      2005  4      10         ',  &
   '100      2006  4      10         ',  &
   '100      2007  4      10         ',  &
   '100      2008  4      9          ',  &
   '100      2016  4      9          ']
character(len=40)            :: readme

  call unit_check_start('o2d')

   do i=2,size(tests)
      readme=tests(i)
      read(readme,*)iday,iyear,omonth,oday
      dat=o2d(iday,iyear)
      call unit_check('o2d',dat(1).eq.iyear.and.dat(2).eq.omonth.and.dat(3).eq.oday,msg=tests(i))
   enddo

   call unit_check_done('o2d')

end subroutine test_o2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_v2mo
call unit_check_start('v2mo')
call unit_check('v2mo',v2mo(1).eq.'January',    msg='January')
call unit_check('v2mo',v2mo(2).eq.'February',   msg='February')
call unit_check('v2mo',v2mo(3).eq.'March',      msg='March')
call unit_check('v2mo',v2mo(4).eq.'April',      msg='April')
call unit_check('v2mo',v2mo(5).eq.'May',        msg='May')
call unit_check('v2mo',v2mo(6).eq.'June',       msg='June')
call unit_check('v2mo',v2mo(7).eq.'July',       msg='July')
call unit_check('v2mo',v2mo(8).eq.'August',     msg='August')
call unit_check('v2mo',v2mo(9).eq.'September',  msg='September')
call unit_check('v2mo',v2mo(10).eq.'October',   msg='October')
call unit_check('v2mo',v2mo(11).eq.'November',  msg='November')
call unit_check('v2mo',v2mo(12).eq.'December',  msg='December')
call unit_check_done('v2mo')
end subroutine test_v2mo
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mo2d
integer :: dat(8)
call unit_check_start('mo2d')
call date_and_time(values=dat)
call  unit_check('mo2d',all(mo2d('january',    2019).eq.[2019,01,01,dat(4),0,0,0,0]),msg='January    2019')
call  unit_check('mo2d',all(mo2d('february',   2019).eq.[2019,02,01,dat(4),0,0,0,0]),msg='February   2019')
call  unit_check('mo2d',all(mo2d('march',      2019).eq.[2019,03,01,dat(4),0,0,0,0]),msg='March      2019')
call  unit_check('mo2d',all(mo2d('april',      2019).eq.[2019,04,01,dat(4),0,0,0,0]),msg='April      2019')
call  unit_check('mo2d',all(mo2d('may',        2019).eq.[2019,05,01,dat(4),0,0,0,0]),msg='May        2019')
call  unit_check('mo2d',all(mo2d('june',       2019).eq.[2019,06,01,dat(4),0,0,0,0]),msg='June       2019')
call  unit_check('mo2d',all(mo2d('july',       2019).eq.[2019,07,01,dat(4),0,0,0,0]),msg='July       2019')
call  unit_check('mo2d',all(mo2d('august',     2019).eq.[2019,08,01,dat(4),0,0,0,0]),msg='August     2019')
call  unit_check('mo2d',all(mo2d('september',  2019).eq.[2019,09,01,dat(4),0,0,0,0]),msg='September  2019')
call  unit_check('mo2d',all(mo2d('october',    2019).eq.[2019,10,01,dat(4),0,0,0,0]),msg='October    2019')
call  unit_check('mo2d',all(mo2d('november',   2019).eq.[2019,11,01,dat(4),0,0,0,0]),msg='November   2019')
call  unit_check('mo2d',all(mo2d('december',   2019).eq.[2019,12,01,dat(4),0,0,0,0]),msg='December   2019')
call unit_check_done('mo2d')
end subroutine test_mo2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mo2v()
!!use M_time,     only: mo2v

call unit_check_start('mo2v')

call unit_check('mo2v', mo2v('jan')       .eq.  1   ,msg='Check January')
call unit_check('mo2v', mo2v('Feb')       .eq.  2   ,msg='Check February')
call unit_check('mo2v', mo2v('March')     .eq.  3   ,msg='Check March')
call unit_check('mo2v', mo2v('APR')       .eq.  4   ,msg='Check April')
call unit_check('mo2v', mo2v('may')       .eq.  5   ,msg='Check May')
call unit_check('mo2v', mo2v('jun')       .eq.  6   ,msg='Check Jun')
call unit_check('mo2v', mo2v('july')      .eq.  7   ,msg='Check July')
call unit_check('mo2v', mo2v('Aug')       .eq.  8   ,msg='Check August')
call unit_check('mo2v', mo2v('Sept')      .eq.  9   ,msg='Check September')
call unit_check('mo2v', mo2v('Oct')       .eq.  10  ,msg='Check October')
call unit_check('mo2v', mo2v('Nov')       .eq.  11  ,msg='Check November')
call unit_check('mo2v', mo2v('December')  .eq.  12  ,msg='Check December')
call unit_check('mo2v', mo2v('jax')       .eq.  1   ,msg='Check "jax"')
call unit_check('mo2v', mo2v('ja')        .eq.  1   ,msg='Check "ja"')
call unit_check('mo2v', mo2v('j')         .eq. -1   ,msg='Check "j"')
call unit_check('mo2v', mo2v('')          .eq. -1   ,msg='Check ""')

call unit_check_done('mo2v') ! assume if got here passed checks

end subroutine test_mo2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_now
call unit_check_start('now')
call unit_check_done('now')
end subroutine test_now
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fmtdate
!!use M_time, only: guessdate, fmtdate
implicit none
character(len=80)              :: date1
character(len=80)              :: date2
character(len=80)              :: iso_week_date
character(len=132)             :: comment
character(len=372),allocatable :: line(:)
integer                        :: dat(8)
integer                        :: i
! the data file with dates to read and expected answers and comments
line=[ character(len=372) :: &
& ' "Sat 1 Jan 2005",  "2005-01-01", "2004-W53-6", " " ', &
& ' "Sun 2 Jan 2005",  "2005-01-02", "2004-W53-7", " " ', &
& ' "Sat 31 Dec 2005", "2005-12-31", "2005-W52-6", " " ', &
& ' "Mon 1 Jan 2007",  "2007-01-01", "2007-W01-1", "Both years 2007 start with the same day." ', &
& ' "Sun 30 Dec 2007", "2007-12-30", "2007-W52-7", " " ', &
& ' "Mon 31 Dec 2007", "2007-12-31", "2008-W01-1", " " ', &
& ' "Tue 1 Jan 2008",  "2008-01-01", "2008-W01-2", &
& "Gregorian year 2008 is a leap year. ISO year 2008 is 2 days shorter: 1 day longer at the start,  3 days shorter at the end." ', &
& ' "Sun 28 Dec 2008", "2008-12-28", "2008-W52-7", " ISO year 2009 begins three days before the end of Gregorian 2008." ', &
& ' "Mon 29 Dec 2008", "2008-12-29", "2009-W01-1", " " ', &
& ' "Tue 30 Dec 2008", "2008-12-30", "2009-W01-2", " " ', &
& ' "Wed 31 Dec 2008", "2008-12-31", "2009-W01-3", " " ', &
& ' "Thu 1 Jan 2009",  "2009-01-01", "2009-W01-4", " " ', &
& ' "Thu 31 Dec 2009", "2009-12-31", "2009-W53-4", "ISO year 2009 has 53 weeks and ends three days into Gregorian year 2010." ', &
& ' "Fri 1 Jan 2010",  "2010-01-01", "2009-W53-5", " " ', &
& ' "Sat 2 Jan 2010",  "2010-01-02", "2009-W53-6", " " ', &
& ' "Sun 3 Jan 2010",  "2010-01-03", "2009-W53-7", " " ', &
&' ' ]

call unit_check_start('fmtdate')

do i=1,size(line)-1
   read(line(i),*)date1,date2,iso_week_date,comment
   if(unit_check_level.gt.0)then
      call unit_check_msg('fmtdate','GIVEN:'//trim(date1)//' '//trim(comment))
   endif

   call guessdate(date1,dat)                                         ! convert date string to DAT
   call unit_check('fmtdate',fmtdate(dat,'year-month-day').eq.trim(date2),fmtdate(dat,'year-month-day'))

   ! convert DAT to ISO week date, all generated dates should match ISO week date
   call unit_check('fmtdate',fmtdate(dat,"%I").eq.iso_week_date, msg=iso_week_date)
enddo

call unit_check_done('fmtdate')

end subroutine test_fmtdate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fmtdate_usage
call unit_check_start('fmtdate_usage')
call unit_check_done('fmtdate_usage')
end subroutine test_fmtdate_usage
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_guessdate
!!use M_time, only: guessdate, w2d, d2w, fmtdate
implicit none
character(len=80)              :: date1
character(len=80)              :: date2
character(len=80)              :: iso_week_date
character(len=132)             :: comment
character(len=372),allocatable :: line(:)
integer                        :: dat(8)
integer                        :: i

call unit_check_start('guessdate')

! the data file with dates to read and expected answers and comments
line=[ character(len=372) :: &

& ' "Sat 1 Jan 2005",  "2005-01-01", "2004-W53-6", " " ', &
& ' "Sun 2 Jan 2005",  "2005-01-02", "2004-W53-7", " " ', &
& ' "Sat 31 Dec 2005", "2005-12-31", "2005-W52-6", " " ', &
& ' "Mon 1 Jan 2007",  "2007-01-01", "2007-W01-1", "Both years 2007 start with the same day." ', &
& ' "Sun 30 Dec 2007", "2007-12-30", "2007-W52-7", " " ', &
& ' "Mon 31 Dec 2007", "2007-12-31", "2008-W01-1", " " ', &
& ' "Tue 1 Jan 2008",  "2008-01-01", "2008-W01-2", &
& "Gregorian year 2008 is a leap year. ISO year 2008 is 2 days shorter: 1 day longer at the start,  3 days shorter at the end." ', &
& ' "Sun 28 Dec 2008", "2008-12-28", "2008-W52-7", " ISO year 2009 begins three days before the end of Gregorian 2008." ', &
& ' "Mon 29 Dec 2008", "2008-12-29", "2009-W01-1", " " ', &
& ' "Tue 30 Dec 2008", "2008-12-30", "2009-W01-2", " " ', &
& ' "Wed 31 Dec 2008", "2008-12-31", "2009-W01-3", " " ', &
& ' "Thu 1 Jan 2009",  "2009-01-01", "2009-W01-4", " " ', &
& ' "Thu 31 Dec 2009", "2009-12-31", "2009-W53-4", "ISO year 2009 has 53 weeks and ends three days into Gregorian year 2010." ', &
& ' "Fri 1 Jan 2010",  "2010-01-01", "2009-W53-5", " " ', &
& ' "Sat 2 Jan 2010",  "2010-01-02", "2009-W53-6", " " ', &
& ' "Sun 3 Jan 2010",  "2010-01-03", "2009-W53-7", " " ', &

&' ' ]
do i=1,size(line)-1
   read(line(i),*)date1,date2,iso_week_date,comment
   call guessdate(date1,dat)                                         ! convert date string to DAT
   call unit_check('guessdate',fmtdate(dat,"%I").eq.iso_week_date,msg=date1)
   call unit_check('guessdate',fmtdate(dat,"year-month-day").eq.date2,msg=date2)
enddo

call unit_check_done('guessdate')

end subroutine test_guessdate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dow
!!use M_time, only : dow
implicit none
integer          :: dat(8)     ! input date array
integer          :: weekday
character(len=9) :: day
integer          :: ierr
call unit_check_start('dow')
call date_and_time(values=dat)
call dow([1957,3,2,dat(4),12,0,0,0], weekday, day, ierr)
call unit_check('dow',day.eq.'Saturday'.and.weekday.eq.6,msg='Saturday')
call unit_check_done('dow')
end subroutine test_dow
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_w2d
!!use M_time, only: w2d
implicit none
character(len=372),allocatable :: line(:)
integer            :: y,m,d
integer            :: iso_year
integer            :: iso_week
integer            :: iso_weekday
integer            :: dat(8)
integer            :: i

call unit_check_start('w2d')

! the data file with dates to read and expected answers and comments
line=[ character(len=372) :: &

& ' 2005 01 01  2004 53 6   ', &
& ' 2005 01 02  2004 53 7   ', &
& ' 2005 12 31  2005 52 6   ', &
& ' 2007 01 01  2007 01 1   ', &
& ' 2007 12 30  2007 52 7   ', &
& ' 2007 12 31  2008 01 1   ', &
& ' 2008 01 01  2008 01 2   ', &
& ' 2008 12 28  2008 52 7   ', &
& ' 2008 12 29  2009 01 1   ', &
& ' 2008 12 30  2009 01 2   ', &
& ' 2008 12 31  2009 01 3   ', &
& ' 2009 01 01  2009 01 4   ', &
& ' 2009 12 31  2009 53 4   ', &
& ' 2010 01 01  2009 53 5   ', &
& ' 2010 01 02  2009 53 6   ', &
& ' 2010 01 03  2009 53 7   ', &
& '                          ' ]
do i=1,size(line)-1
   read(line(i),*)y,m,d,iso_year,iso_week,iso_weekday
   call w2d(iso_year,iso_week,iso_weekday,dat)                                      ! convert ISO week date to DAT
   call unit_check('w2d',dat(1).eq.y.and.dat(2).eq.m.and.dat(3).eq.d,msg=line(i))   ! all should match
enddo

call unit_check_done('w2d')

end subroutine test_w2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_box_month
integer           :: dat(8)
character(len=21) :: calendar(8)
character(len=21) :: mnth(8)
dat=[2016,07,01,-240,12,0,0,0]
mnth=[ &
'      July 2016      ', &
'Mo Tu We Th Fr Sa Su ', &
'             1  2  3 ', &
' 4  5  6  7  8  9 10 ', &
'11 12 13 14 15 16 17 ', &
'18 19 20 21 22 23 24 ', &
'25 26 27 28 29 30 31 ', &
'                     ']
call unit_check_start('box_month')
call box_month(dat,calendar)
if(unit_check_level.gt.0)then
   write(*,'(a)')calendar
   write(*,'(a)')mnth
endif
call unit_check('box_month',all(calendar.eq.mnth),msg='July 2016')
call unit_check_done('box_month')
end subroutine test_box_month
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2j
real(kind=realtime)         :: julian

call unit_check_start('d2j')

   call unit_check_start('d2j',msg='Checking Julian Date') ! assume if got here passed checks

   julian=d2j( [1970, 1, 1,0, 0,0,0,0])
   call unit_check('d2j',abs(julian-2440587.5d0).lt.0.00001 ,msg="Dec 31st, 1969  8:00(2440587.5)")

   julian=d2j( [1995, 1, 1,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2449719 ,msg="Jan  1st, 1995 12:00(2449719)")

   julian=d2j( [1995,10,19,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2450010, msg="Oct 19th, 1995 12:00(2450010)")

   julian=d2j( [1995,12,31,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2450083, msg="Dec 31st, 1995 12:00(2450083)")

   julian=d2j( [1996, 1, 1,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2450084, msg="Jan  1st, 1996 12:00(2450084)")

   julian=d2j( [1996,12,31,0,12,0,0,0])
   call unit_check('d2j',int(julian).eq.2450449, msg="Dec 31th, 1996 12:00(2450449)")

call unit_check_done('d2j')

end subroutine test_d2j
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_j2d
real(kind=realtime)          :: juliandate
character(len=:),allocatable :: expected

call unit_check_start('j2d')


   juliandate=2457589.129d0                 ! set sample Julian Date

   expected='2016-07-19 11:05:45'
   call unit_check('j2d',fmtdate(j2d(juliandate),'year-month-day hour:minute:second').eq.expected, &
   & juliandate,'==> EXPECTED ',expected,' GOT ',fmtdate(j2d(juliandate),'year-month-day hour:minute:second'))

   ! go back one day
   expected='2016-07-18 11:05:45'
   call unit_check('j2d',fmtdate(j2d(juliandate-1.0d0),'year-month-day hour:minute:second').eq.expected, &
   & juliandate,'==> EXPECTED ',expected,' GOT ',fmtdate(j2d(juliandate-1.0d0),'year-month-day hour:minute:second'))

   ! go forward one day
   expected='2016-07-20 11:05:45'
   call unit_check('j2d',fmtdate(j2d(juliandate+1.0d0),'year-month-day hour:minute:second').eq.expected, &
   & juliandate,'==> EXPECTED ',expected,' GOT ',fmtdate(j2d(juliandate+1.0d0),'year-month-day hour:minute:second'))

call unit_check_done('j2d')

end subroutine test_j2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2u()
!!use M_time, only : d2u
implicit none

!  Note that time zones are usually -HHMM or -HH:MM and not MM, which is what the DAT array uses
!  Comparing to Unix date(1) command:
!    date --date "Wed Mar 29 01:46:47 EDT 2017" +%s      ! 1490766407
!    date --date "Wed Mar 29 01:46:47 2017" +%s          ! 1490766407
!    date --date "Wed Mar 29 01:46:47 -400 2017" +%s     ! 1490766407
!    date --date "Wed Mar 29 01:46:47 UTC-400 2017" +%s  ! 1490766407
!    date --date "Wed Mar 29 01:46:47 UTC-4:00 2017" +%s ! 1490766407

   call unit_check_start('d2u')

   call unit_check('d2u',nint(d2u([2017,03,29,-240,01,46,47,0])+0.5).eq.1490766407,d2u([2017,03,29,-240,01,46,47,0]) )

   call unit_check_done('d2u')

end subroutine test_d2u
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_u2d
call unit_check_start('u2d')
call unit_check('u2d',all([2017,03,29,-240,01,46,47,0].eq.u2d(1490766407)),&
        & d2u([2017,03,29,-240,01,46,47,0]) )
call unit_check_done('u2d')
end subroutine test_u2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sec2days()
!!use M_time, only: sec2days
implicit none

   call unit_check_start('sec2days')

   call unit_check('sec2days',sec2days(129860).eq.             '1-12:04:20','129860 is 1-12:04:20')
   call unit_check('sec2days',sec2days(80000.0d0).eq.          '0-22:13:20','80000.0d0 is 0-22:13:20')
   call unit_check('sec2days',sec2days(80000,crop=.true.).eq.    '22:13:20','80000 is 22:13:20')
   call unit_check('sec2days',sec2days('1day 2hr 3 min 4s').eq.'1-02:03:04','1day 2hr 3 min 4s is 1-02:03:04')

   call unit_check_done('sec2days')

end subroutine test_sec2days
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_days2sec()
!!use M_time, only  : days2sec, realtime
implicit none

   call unit_check_start('days2sec')

   call unit_check('days2sec',nint(days2sec('1')).eq.             1, msg='1')
   call unit_check('days2sec',nint(days2sec('1:00')).eq.         60, msg='1:00')
   call unit_check('days2sec',nint(days2sec('1:00:00')).eq.    3600, msg='1:00:00')
   call unit_check('days2sec',nint(days2sec('1-00:00:00')).eq.86400, msg='1-00:00:00')
   call unit_check('days2sec',nint(days2sec('1d2h 3.0 minutes 4sec')).eq.93784,msg='1d2h 3.0 minutes 4sec')

   call unit_check('days2sec',nint(days2sec(' 1-12:04:20              ')) .eq. 129860,msg='1-12:04:20')
   call unit_check('days2sec',nint(days2sec(' 1.5 days                ')) .eq. 129600,msg='1.5 days')
   call unit_check('days2sec',nint(days2sec(' 1.5 days 4hrs 30minutes ')) .eq. 145800,msg='1.5 days 4 hrs 30 minutes')
   call unit_check('days2sec',nint(days2sec(' 1.5d                    ')) .eq. 129600,msg='1.5d')
   call unit_check('days2sec',nint(days2sec(' 1d2h3m4s                ')) .eq. 93784,msg='1d2h3m4s')
   call unit_check('days2sec',nint(days2sec(' 1d1d1d                  ')) .eq. 259200,msg='DUPLICATES: 1d1d1d')
   call unit_check('days2sec',nint(days2sec(' 4d-12h                  ')) .eq. 302400,msg='NEGATIVE VALUES: 4d-12h')
   call unit_check('days2sec',nint(days2sec(' 3  d  1 2   h           ')) .eq. 302400,msg='WHITESPACE: 3 d 1 2 h')

   call unit_check_done('days2sec')

end subroutine test_days2sec
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_phase_of_moon
integer  :: dat(8)=[2018,11,3,-240,20,18,44,245]
call unit_check_start('phase_of_moon')
call unit_check('phase_of_moon',phase_of_moon(dat).eq.'Waning crescent')
call unit_check_done('phase_of_moon')
end subroutine test_phase_of_moon
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_moon_fullness
integer  :: dat(8)=[2018,11,3,-240,20,18,44,245]
call unit_check_start('moon_fullness')
call unit_check('moon_fullness', moon_fullness(dat).eq.-30)
call unit_check_done('moon_fullness')
end subroutine test_moon_fullness
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_easter()
!!use M_time,  only : easter
implicit none
character(len=20),parameter  :: tests(*)=[ &
'1980,4,6  ',  &
'1981,4,19 ',  &
'1982,4,11 ',  &
'1983,4,3  ',  &
'1984,4,22 ',  &
'1985,4,7  ',  &
'1986,3,30 ',  &
'1987,4,19 ',  &
'1988,4,3  ',  &
'1989,3,26 ',  &
'1990,4,15 ',  &
'1991,3,31 ',  &
'1992,4,19 ',  &
'1993,4,11 ',  &
'1994,4,3  ',  &
'1995,4,16 ',  &
'1996,4,7  ',  &
'1997,3,30 ',  &
'1998,4,12 ',  &
'1999,4,4  ',  &
'2000,4,23 ',  &
'2001,4,15 ',  &
'2002,3,31 ',  &
'2003,4,20 ',  &
'2004,4,11 ',  &
'2005,3,27 ',  &
'2006,4,16 ',  &
'2007,4,8  ',  &
'2008,3,23 ',  &
'2009,4,12 ',  &
'2010,4,4  ',  &
'2011,4,24 ',  &
'2012,4,8  ',  &
'2013,3,31 ',  &
'2014,4,20 ',  &
'2015,4,5  ',  &
'2016,3,27 ',  &
'2017,4,16 ',  &
'2018,4,1  ',  &
'2019,4,21 ',  &
'2020,4,12 ',  &
'2021,4,4  ',  &
'2022,4,17 ',  &
'2023,4,9  ',  &
'2024,3,31 '   ]
character(len=20)       :: readme

integer :: tmonth, tday
integer :: inyear, outmonth, outday
integer :: ii
integer :: dat(8)
character (len=5)  :: mon(3:4) = (/ 'march', 'april' /)

call unit_check_start('easter') ! assume if got here passed checks

do ii=1,size(tests)
   readme=tests(ii) ! cannot do internal read on a parameter
   read(readme,*)inyear,tmonth,tday
   call easter(inyear,dat)
   outmonth=dat(2)
   outday=dat(3)
   call unit_check('easter', tmonth.eq.outmonth.and.tday.eq.outday ,tests(ii),'month=',mon(outmonth))
enddo

call unit_check_done('easter') ! assume if got here passed checks

end subroutine test_easter
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_now_ex
call unit_check_start('now_ex')
call unit_check_done('now_ex')
end subroutine test_now_ex
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2w()
use M_verify, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level
implicit none

   call unit_check_start('d2w',msg='Examples of contemporary dates around New Year''s Day')
   call date_and_time(values=dat)

   dat=[2005,01,01,dat(4),0,0,0,0] !  Sat 1 Jan 2005 2005-01-01 2004-W53-6
   call showme("2004-W53-6")
   dat=[2005,01,02,dat(4),0,0,0,0] !  Sun 2 Jan 2005 2005-01-02 2004-W53-7
   call showme("2004-W53-7")
   dat=[2005,12,31,dat(4),0,0,0,0] !  Sat 31 Dec 2005 2005-12-31 2005-W52-6
   call showme("2005-W52-6")
   dat=[2007,01,01,dat(4),0,0,0,0] !  Mon 1 Jan 2007 2007-01-01 2007-W01-1 Both years 2007 start with the same day.
   call showme("2007-W01-1")
   dat=[2007,12,30,dat(4),0,0,0,0] !  Sun 30 Dec 2007 2007-12-30 2007-W52-7
   call showme("2007-W52-7")
   dat=[2007,12,31,dat(4),0,0,0,0] !  Mon 31 Dec 2007 2007-12-31 2008-W01-1
   call showme("2008-W01-1")
   dat=[2008,01,01,dat(4),0,0,0,0] !  Tue 1 Jan 2008 2008-01-01 2008-W01-2
                                   !     Gregorian year 2008 is a leap year. ...
                                   !     ISO year 2008 is 2 days shorter: 1 day longer at the start, 3 days shorter at the end.
   call showme("2008-W01-2")
   dat=[2008,12,28,dat(4),0,0,0,0] !  Sun 28 Dec 2008 2008-12-28 2008-W52-7 ...
                                   !     ISO year 2009 begins three days before the end of Gregorian 2008.
   call showme("2008-W52-7")
   dat=[2008,12,29,dat(4),0,0,0,0] !  Mon 29 Dec 2008 2008-12-29 2009-W01-1
   call showme("2009-W01-1")
   dat=[2008,12,30,dat(4),0,0,0,0] !  Tue 30 Dec 2008 2008-12-30 2009-W01-2
   call showme("2009-W01-2")
   dat=[2008,12,31,dat(4),0,0,0,0] !  Wed 31 Dec 2008 2008-12-31 2009-W01-3
   call showme("2009-W01-3")
   dat=[2009,01,01,dat(4),0,0,0,0] !  Thu 1 Jan 2009 2009-01-01 2009-W01-4
   call showme("2009-W01-4")
   dat=[2009,12,31,dat(4),0,0,0,0] !  Thu 31 Dec 2009 2009-12-31 2009-W53-4  ...
                                   !     ISO year 2009 has 53 weeks and ends three days into Gregorian year 2010.
   call showme("2009-W53-4")
   dat=[2010,01,01,dat(4),0,0,0,0] !  Fri 1 Jan 2010 2010-01-01 2009-W53-5
   call showme("2009-W53-5")
   dat=[2010,01,02,dat(4),0,0,0,0] !  Sat 2 Jan 2010 2010-01-02 2009-W53-6
   call showme("2009-W53-6")
   dat=[2010,01,03,dat(4),0,0,0,0] !  Sun 3 Jan 2010 2010-01-03 2009-W53-7
   call showme("2009-W53-7")

   call unit_check_done('d2w') ! assume if got here passed checks
end subroutine test_d2w

subroutine showme(string)
use M_time, only : d2w
implicit none
character(len=*) :: string
integer          :: iyear,iweek,iweekday
character(len=10):: name
   call d2w(dat,iyear,iweek,iweekday,name)
   call unit_check('d2w', name.eq.string ,iyear,iweek,iweekday,name,string)
end subroutine showme
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end subroutine test_suite_M_time
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
