program runtest
use,intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
use M_framework__verify, only : unit_test, unit_test_end
use M_framework__verify, only : unit_test_start, unit_test_msg, unit_test_level
use M_framework__verify, only : unit_test_stop
use M_time,  only: &
             d2o,             d2u,      d2w,      date_to_julian, date_to_unix,    &
             days2sec,        dow,      easter,   fmtdate,        guessdate,       &
             julian_to_date,  mo2v,     now,      o2d,            ordinal_seconds, &
             ordinal_to_date, realtime, sec2days, w2d,            moon_fullness,   &
             phase_of_moon,   u2d,      j2d,      d2j,            box_month,       &
             mo2d,            v2mo,                               unix_to_date
use M_time, only : locale
implicit none
integer :: dat(8)
integer :: ierr
character(len=*),parameter :: SAME='' ! '-library libGPF -section 3 -description "'
character(len=*),parameter :: SAMEEND='' ! '"'

unit_test_level=0

!! no not use M_system version or will create a circular dependency
call put_environment_variable('TZ','America/New_York',ierr) ! some of the test values assume EST
call put_environment_variable('TZ','UTC+04:00',ierr) ! some of the test values assume EST

call unit_test_msg('M_time','This section contains unit tests for procedures in the M_time(3f) module.')

call unit_test_start('box_month      ',SAME//'print specified month into character array'//SAMEEND)
call test_box_month()
call unit_test_start('d2j            ',SAME//'Convert date array to Julian Date'//SAMEEND)
call test_d2j()
call unit_test_start('d2o            ',SAME//'Converts date-time array to Ordinal day'//SAMEEND)
call test_d2o()
call unit_test_start('ordinal_seconds',SAME//'seconds since begiing of year'//SAMEEND)
call test_ordinal_seconds()
call unit_test_start('d2u            ',SAME//'Convert date array to Unix Time'//SAMEEND)
call test_d2u()
call unit_test_start('d2w            ',SAME//'Calculate iso-8601 Week-numbering year date yyyy-Www-d'//SAMEEND)
call test_d2w()
call unit_test_start('date_to_julian ',SAME//'Converts Proleptic Gregorian date array to Julian Date'//SAMEEND)
call test_date_to_julian()
call unit_test_start('date_to_unix   ',SAME//'Converts date array to Unix Time (UT starts at 0000 on 1 Jan. 1970, UTC)'//SAMEEND)
call test_date_to_unix()
call unit_test_start('days2sec       ',SAME//'converts string D-HH:MM:SS to seconds from small to large'//SAMEEND)
call test_days2sec()
call unit_test_start('dow            ',SAME//'Return the day of the week'//SAMEEND)
call test_dow()
call unit_test_start('easter         ',SAME//'Determine month and day Easter falls on for given year'//SAMEEND)
call test_easter()
!!call unit_test_start('ephemeris      ',SAME//'ephemeris position of planets for adjusting an equatorial telescope'//SAMEEND)
!!call test_ephemeris()
call unit_test_start('locale        ',SAME//'select language for month and weekday names'//SAMEEND)
call test_locale()

call unit_test_start('fmtdate        ',SAME//'given date array return date as string using format'//SAMEEND)
call test_fmtdate()
call unit_test_start('fmtdate_usage  ',SAME//'display macros recognized by fmtdate(3f)'//SAMEEND)
call test_fmtdate_usage()
call unit_test_start('guessdate      ',SAME//'Reads in a date, in various formats'//SAMEEND)
call test_guessdate()
call unit_test_start('j2d            ',SAME//'Convert Julian Date to date array'//SAMEEND)
call test_j2d()
call unit_test_start('julian_to_date ',SAME//'Converts Julian Date to (year, month, day, hour, minute, second)'//SAMEEND)
call test_julian_to_date()
call unit_test_start('mo2d           ',SAME//'return date array for beginning of given month name in specified year'//SAMEEND)
call test_mo2d()
call unit_test_start('mo2v           ',SAME//'given month as name return month number (1-12) of that month'//SAMEEND)
call test_mo2v()
call unit_test_start('moon_fullness  ',SAME//'return name for phase of moon for given date'//SAMEEND)
call test_moon_fullness()
call unit_test_start('now            ',SAME//'return string representing current time given format'//SAMEEND)
call test_now()
call unit_test_start('now_ex         ',SAME//'use of now(3f) outside of a module'//SAMEEND)
call test_now_ex()
call unit_test_start('o2d            ',SAME//'given ordinal day of year return date array, Jan 1st=1'//SAMEEND)
call test_o2d()
call unit_test_start('ordinal_to_date',SAME//'given ordinal day of year return date array, Jan 1st=1'//SAMEEND)
call test_ordinal_to_date()
call unit_test_start('phase_of_moon  ',SAME//'percentage of moon phase from new to full'//SAMEEND)
call test_phase_of_moon()
call unit_test_start('sec2days       ',SAME//'converts seconds to string D-HH:MM:SS'//SAMEEND)
call test_sec2days()
call unit_test_start('u2d            ',SAME//'Convert Unix Time to date array'//SAMEEND)
call test_u2d()
call unit_test_start('unix_to_date   ',SAME//'Converts Unix Time to date array'//SAMEEND)
call test_unix_to_date()
call unit_test_start('v2mo           ',SAME//'returns the month name of a Common month'//SAMEEND)
call test_v2mo()
call unit_test_start('w2d            ',SAME//'Given iso-8601 Week-numbering year date yyyy-Www-d calculate date'//SAMEEND)
call test_w2d()

call unit_test_stop('M_time tests completed')

contains
!===================================================================================================================================
subroutine to_upper_extended_ascii()
character(len=:),allocatable :: month_names(:), weekday_names(:), month_names_abbr(:), weekday_names_abbr(:)

month_names = [ character(len=9) :: &
& 'JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE','JULY','AUGUST','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER' ]

weekday_names = [ character(len=10) :: &
& 'MONDAY','TUESDAY','WEDNESDAY','THURSDAY','FRIDAY', 'SATURDAY','SUNDAY' ]

month_names_abbr= month_names(:)(1:3)
weekday_names_abbr= weekday_names(:)(1:3)

call locale('user', month_names, weekday_names, month_names_abbr, weekday_names_abbr )

end subroutine to_upper_extended_ascii
!===================================================================================================================================
#ifndef _WIN32

subroutine put_environment_variable(name,value,status)

!  This is an private copy of the set_environment_variable routine(3f) routine from
!  M_system.FF that is duplicated in order to prevent a circular dependency.

! ident_33="@(#)M_system::put_environment_variable(3f): call setenv(3c) to set environment variable"

character(len=*)               :: NAME
character(len=*)               :: VALUE
integer, optional, intent(out) :: STATUS
integer                        :: loc_err
character(kind=c_char,len=1),allocatable :: temp_chars1(:)
character(kind=c_char,len=1),allocatable :: temp_chars2(:)

interface
   integer(kind=c_int) function c_setenv(c_name,c_VALUE) bind(C,NAME="setenv")
      import c_int, c_char
      character(kind=c_char)   :: c_name(*)
      character(kind=c_char)   :: c_VALUE(*)
   end function
end interface

   temp_chars1=str2arr(trim(NAME))
   temp_chars2=str2arr(VALUE)
   loc_err =  c_setenv(temp_chars1,temp_chars2)
   if (present(STATUS)) STATUS = loc_err
end subroutine put_environment_variable

#else

subroutine put_environment_variable(name,value,status)
character(len=*)               :: NAME
character(len=*)               :: VALUE
integer, optional, intent(out) :: STATUS
   write(*,*)'<WARNING>put_environment_variable is not working on this platform'
   if (present(STATUS)) STATUS = -1
end subroutine put_environment_variable

#endif
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
real(kind=realtime) :: julian
integer             :: ierr
integer             :: dat(8)

   dat = [1970, 1, 1,0, 0,0,0,0]
   call date_to_julian( dat,julian,ierr)
   call unit_test('date_to_julian',abs(julian-2440587.5d0) < 0.00001 ,msg="Dec 31st, 1969  8:00(2440587.5)")

   dat = [1995, 1, 1,0,12,0,0,0]
   call date_to_julian( dat,julian,ierr)
   call unit_test('date_to_julian',int(julian) == 2449719 ,msg="Jan  1st, 1995 12:00(2449719)")

   dat = [1995,10,19,0,12,0,0,0]
   call date_to_julian( dat,julian,ierr)
   call unit_test('date_to_julian',int(julian) == 2450010, msg="Oct 19th, 1995 12:00(2450010)")

   dat = [1995,12,31,0,12,0,0,0]
   call date_to_julian( dat,julian,ierr)
   call unit_test('date_to_julian',int(julian) == 2450083, msg="Dec 31st, 1995 12:00(2450083)")

   dat = [1996, 1, 1,0,12,0,0,0]
   call date_to_julian( dat,julian,ierr)
   call unit_test('date_to_julian',int(julian) == 2450084, msg="Jan  1st, 1996 12:00(2450084)")

   dat = [1996,12,31,0,12,0,0,0]
   call date_to_julian( dat,julian,ierr)
   call unit_test('date_to_julian',int(julian) == 2450449, msg="Dec 31th, 1996 12:00(2450449)")

   call unit_test_end('date_to_julian')

end subroutine test_date_to_julian
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_julian_to_date()
real(kind=realtime)          :: juliandate
integer                      :: dat(8)
integer                      :: ierr
character(len=:),allocatable :: expected

   juliandate=2457589.129d0                 ! set sample Julian Date
   call julian_to_date(juliandate,dat,ierr) ! create DAT array for this date
   expected='2016-07-19 11:05:45'
   call unit_test('julian_to_date',fmtdate(dat,'year-month-day hour:minute:second') == expected,&
          & juliandate,'==> EXPECTED ',expected,' GOT ',fmtdate(dat),'year-month-day hour:minute:second')

   call julian_to_date(juliandate-1.0d0,dat,ierr) ! go back one day
   expected='2016-07-18 11:05:45'
   call unit_test('julian_to_date',fmtdate(dat,'year-month-day hour:minute:second') == expected,&
          & juliandate,'==> EXPECTED ',expected,' GOT ',fmtdate(dat),'year-month-day hour:minute:second')

   call julian_to_date(juliandate+1.0d0,dat,ierr) ! go forward one day
   expected='2016-07-20 11:05:45'
   call unit_test('julian_to_date',fmtdate(dat,'year-month-day hour:minute:second') == expected,&
          & juliandate,'==> EXPECTED ',expected,' GOT ',fmtdate(dat),'year-month-day hour:minute:second')

   call unit_test_end('julian_to_date')

end subroutine test_julian_to_date
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_date_to_unix
real(kind=realtime)              :: unixtime
integer                          :: ierr
integer                          :: in(8)
integer                          :: expected

in=[2017,03,29,-240,01,46,47,0]
expected=1490766407
call date_to_unix(in,unixtime,ierr)
call unit_test('date_to_unix',abs(unixtime-expected) < 0.001 ,'EXPECTED',expected,'RESULT',unixtime)
call unit_test_end('date_to_unix')

end subroutine test_date_to_unix
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unix_to_date
integer                          :: ierr
integer                          :: dat(8)
integer                          :: result(8)
integer                          :: unixtime
dat=[2017,03,29,-240,01,46,47,0]
unixtime=1490766407

call unix_to_date(unixtime,result,ierr)
call unit_test('unix_to_date',all(dat == result),'IN',unixtime) !JSU
call unit_test('unix_to_date',ierr==0,'IERR',ierr)
call unit_test_end('unix_to_date')
end subroutine test_unix_to_date
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2o()
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


    do i=2,size(tests)
       readme=tests(i)
       read(readme,*)iday,iyear,omonth,oday
       dat=o2d(iday,iyear)
       rday=d2o(dat)
       call unit_test('d2o',iday == rday,msg=tests(i))
    enddo

   call unit_test_end('d2o')

end subroutine test_d2o
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ordinal_seconds()
integer  :: rday

   rday=ordinal_seconds()/(60*60*24)
   call unit_test('ordinal_seconds',rday == d2o(),rday,d2o())
   call unit_test_end('ordinal_seconds')

end subroutine test_ordinal_seconds
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ordinal_to_date()
!!use M_time, only : o2d, ordinal_to_date, d2o
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

   do i=2,size(tests)
      readme=tests(i)
      read(readme,*)iday,iyear,omonth,oday
      call ordinal_to_date(iyear,iday,dat)
      call unit_test('ordinal_to_date',dat(2) == omonth.and.dat(3) == oday,'year',iyear,'ordinal',iday)
   enddo

   call unit_test_end('ordinal_to_date')

end subroutine test_ordinal_to_date
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_o2d()
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


   do i=2,size(tests)
      readme=tests(i)
      read(readme,*)iday,iyear,omonth,oday
      dat=o2d(iday,iyear)
      call unit_test('o2d',dat(1) == iyear.and.dat(2) == omonth.and.dat(3) == oday,msg=tests(i))
   enddo

   call unit_test_end('o2d')

end subroutine test_o2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_v2mo
call  unit_test('v2mo',v2mo(1)   ==  'January',    'January',    v2mo(1)   )
call  unit_test('v2mo',v2mo(2)   ==  'February',   'February',   v2mo(2)   )
call  unit_test('v2mo',v2mo(3)   ==  'March',      'March',      v2mo(3)   )
call  unit_test('v2mo',v2mo(4)   ==  'April',      'April',      v2mo(4)   )
call  unit_test('v2mo',v2mo(5)   ==  'May',        'May',        v2mo(5)   )
call  unit_test('v2mo',v2mo(6)   ==  'June',       'June',       v2mo(6)   )
call  unit_test('v2mo',v2mo(7)   ==  'July',       'July',       v2mo(7)   )
call  unit_test('v2mo',v2mo(8)   ==  'August',     'August',     v2mo(8)   )
call  unit_test('v2mo',v2mo(9)   ==  'September',  'September',  v2mo(9)   )
call  unit_test('v2mo',v2mo(10)  ==  'October',    'October',    v2mo(10)  )
call  unit_test('v2mo',v2mo(11)  ==  'November',   'November',   v2mo(11)  )
call  unit_test('v2mo',v2mo(12)  ==  'December',   'December',   v2mo(12)  )
call unit_test_end('v2mo')
end subroutine test_v2mo
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mo2d
integer :: dat(8)
call date_and_time(values=dat)
call  unit_test('mo2d',all(mo2d('january',    2019) == [2019,01,01,dat(4),0,0,0,0]),msg='January    2019')
call  unit_test('mo2d',all(mo2d('february',   2019) == [2019,02,01,dat(4),0,0,0,0]),msg='February   2019')
call  unit_test('mo2d',all(mo2d('march',      2019) == [2019,03,01,dat(4),0,0,0,0]),msg='March      2019')
call  unit_test('mo2d',all(mo2d('april',      2019) == [2019,04,01,dat(4),0,0,0,0]),msg='April      2019')
call  unit_test('mo2d',all(mo2d('may',        2019) == [2019,05,01,dat(4),0,0,0,0]),msg='May        2019')
call  unit_test('mo2d',all(mo2d('june',       2019) == [2019,06,01,dat(4),0,0,0,0]),msg='June       2019')
call  unit_test('mo2d',all(mo2d('july',       2019) == [2019,07,01,dat(4),0,0,0,0]),msg='July       2019')
call  unit_test('mo2d',all(mo2d('august',     2019) == [2019,08,01,dat(4),0,0,0,0]),msg='August     2019')
call  unit_test('mo2d',all(mo2d('september',  2019) == [2019,09,01,dat(4),0,0,0,0]),msg='September  2019')
call  unit_test('mo2d',all(mo2d('october',    2019) == [2019,10,01,dat(4),0,0,0,0]),msg='October    2019')
call  unit_test('mo2d',all(mo2d('november',   2019) == [2019,11,01,dat(4),0,0,0,0]),msg='November   2019')
call  unit_test('mo2d',all(mo2d('december',   2019) == [2019,12,01,dat(4),0,0,0,0]),msg='December   2019')
call unit_test_end('mo2d')
end subroutine test_mo2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mo2v()

call unit_test('mo2v', mo2v('jan')        ==   1   ,msg='Check January')
call unit_test('mo2v', mo2v('Feb')        ==   2   ,msg='Check February')
call unit_test('mo2v', mo2v('March')      ==   3   ,msg='Check March')
call unit_test('mo2v', mo2v('APR')        ==   4   ,msg='Check April')
call unit_test('mo2v', mo2v('may')        ==   5   ,msg='Check May')
call unit_test('mo2v', mo2v('jun')        ==   6   ,msg='Check Jun')
call unit_test('mo2v', mo2v('july')       ==   7   ,msg='Check July')
call unit_test('mo2v', mo2v('Aug')        ==   8   ,msg='Check August')
call unit_test('mo2v', mo2v('Sept')       ==   9   ,msg='Check September')
call unit_test('mo2v', mo2v('Oct')        ==   10  ,msg='Check October')
call unit_test('mo2v', mo2v('Nov')        ==   11  ,msg='Check November')
call unit_test('mo2v', mo2v('December')   ==   12  ,msg='Check December')
call unit_test('mo2v', mo2v('jax')        ==  -1   ,msg='Check "jax"')
call unit_test('mo2v', mo2v('ja')         ==   1   ,msg='Check "ja"')
call unit_test('mo2v', mo2v('j')          ==  -1   ,msg='Check "j"')
call unit_test('mo2v', mo2v('')           ==  -1   ,msg='Check ""')

call unit_test_end('mo2v') ! assume if got here passed checks

end subroutine test_mo2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_now
call unit_test_end('now')
end subroutine test_now
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_locale()
integer          :: dat(8)     ! input date array
integer          :: weekday
character(len=9) :: day
integer          :: ierr
character(len=:),allocatable :: month_names(:), weekday_names(:), month_names_abbreviated(:), weekday_names_abbreviated(:)
real(kind=realtime) :: julian
character(len=:),allocatable   :: expected
character(len=:),allocatable   :: returned

call to_upper_extended_ascii()

dat = [1957, 3, 2,-240, 2,0,0,0]

expected='|MAR|MARCH|SAT|SATURDAY|'
returned=fmtdate(dat,'|%l|%L|%w|%W|')
call unit_test('locale',returned.eq.expected,'macros: expected',expected,'returned',returned)

expected='|MARCH|MAR|MAR|SAT|SAT|SATURDAY|'
returned=fmtdate(dat,'|MONTH|Month|Mth|Weekday|wkday|WEEKDAY|')
call unit_test('locale',returned.eq.expected,'keywords: expected',expected,'returned',returned)

! go forward one day
call date_to_julian(dat,julian,ierr)
julian=julian+1
dat=j2d(julian)

expected='|MAR|MARCH|SUN|SUNDAY|'
returned=fmtdate(dat,'|%l|%L|%w|%W|')
call unit_test('locale',returned.eq.expected,'macros: expected',expected,'returned',returned)

expected='|MARCH|MAR|MAR|SUN|SUN|SUNDAY|'
returned=fmtdate(dat,'|MONTH|Month|Mth|Weekday|wkday|WEEKDAY|')
call unit_test('locale',returned.eq.expected,'keywords: expected',expected,'returned',returned)

call  unit_test('locale',  v2mo(1)  == 'JANUARY',   'JANUARY',   'expected  JANUARY    got', v2mo(1)  )
call  unit_test('locale',  v2mo(2)  == 'FEBRUARY',  'FEBRUARY',  'expected  FEBRUARY   got', v2mo(2)  )
call  unit_test('locale',  v2mo(3)  == 'MARCH',     'MARCH',     'expected  MARCH      got', v2mo(3)  )
call  unit_test('locale',  v2mo(4)  == 'APRIL',     'APRIL',     'expected  APRIL      got', v2mo(4)  )
call  unit_test('locale',  v2mo(5)  == 'MAY',       'MAY',       'expected  MAY        got', v2mo(5)  )
call  unit_test('locale',  v2mo(6)  == 'JUNE',      'JUNE',      'expected  JUNE       got', v2mo(6)  )
call  unit_test('locale',  v2mo(7)  == 'JULY',      'JULY',      'expected  JULY       got', v2mo(7)  )
call  unit_test('locale',  v2mo(8)  == 'AUGUST',    'AUGUST',    'expected  AUGUST     got', v2mo(8)  )
call  unit_test('locale',  v2mo(9)  == 'SEPTEMBER', 'SEPTEMBER', 'expected  SEPTEMBER  got', v2mo(9)  )
call  unit_test('locale',  v2mo(10) == 'OCTOBER',   'OCTOBER',   'expected  OCTOBER    got', v2mo(10) )
call  unit_test('locale',  v2mo(11) == 'NOVEMBER',  'NOVEMBER',  'expected  NOVEMBER   got', v2mo(11) )
call  unit_test('locale',  v2mo(12) == 'DECEMBER',  'DECEMBER',  'expected  DECEMBER   got', v2mo(12) )

call date_and_time(values=dat)
dat=[1957,3,2,dat(4),12,0,0,0]
call dow(dat, weekday, day, ierr)
call unit_test('locale',day == 'SATURDAY'.and.weekday == 6,'expected SATURDAY,6 got ',day,weekday)

dat(3)=dat(3)+1 ! next day
call dow(dat, weekday, day, ierr)
call unit_test('locale',day == 'SUNDAY'.and.weekday == 7,'expected SUNDAY,7 got ',day,weekday)

dat(3)=dat(3)+1 ! next day
call dow(dat, weekday, day, ierr)
call unit_test('locale',day == 'MONDAY'.and.weekday == 1,'expected MONDAY,1 got ',day,weekday)

dat(3)=dat(3)+1 ! next day
call dow(dat, weekday, day, ierr)
call unit_test('locale',day == 'TUESDAY'.and.weekday == 2,'expected TUESDAY,2 got ',day,weekday)

dat(3)=dat(3)+1 ! next day
call dow(dat, weekday, day, ierr)
call unit_test('locale',day == 'WEDNESDAY'.and.weekday == 3,'expected WEDNESDAY,3 got ',day,weekday)

dat(3)=dat(3)+1 ! next day
call dow(dat, weekday, day, ierr)
call unit_test('locale',day == 'THURSDAY'.and.weekday == 4,'expected THURSDAY,4 got ',day,weekday)

dat(3)=dat(3)+1 ! next day
call dow(dat, weekday, day, ierr)
call unit_test('locale',day == 'FRIDAY'.and.weekday == 5,'expected FRIDAY,5 got ',day,weekday)


if(unit_test_level > 0)then
   call locale('show')
endif
call locale('reset')
if(unit_test_level > 0)then
   call locale('show')
endif

dat=[1957, 3, 2,-240, 2,0,0,0]
expected='|Mar|March|Sat|Saturday|'
returned=fmtdate(dat,'|%l|%L|%w|%W|')
call unit_test('locale',returned.eq.expected,'after reset macros: expected',expected,'returned',returned)

expected='|March|Mar|Mar|Sat|Sat|Saturday|'
returned=fmtdate(dat,'|MONTH|Month|Mth|Weekday|wkday|WEEKDAY|')
call unit_test('locale',returned.eq.expected,'after reset keywords: expected',expected,'returned',returned)

call unit_test_end('locale')
end subroutine test_locale
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fmtdate
character(len=80)              :: date1
character(len=80)              :: date2
character(len=80)              :: iso_week_date
character(len=132)             :: comment
character(len=372),allocatable :: line(:)
integer                        :: dat(8)
integer                        :: i
real(kind=realtime)            :: julian
character(len=:),allocatable   :: expected
character(len=:),allocatable   :: returned
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
   if(unit_test_level > 0)then
      call unit_test_msg('fmtdate','GIVEN:'//trim(date1)//' '//trim(comment))
   endif

   call guessdate(date1,dat)                                         ! convert date string to DAT
   call unit_test('fmtdate',fmtdate(dat,'year-month-day') == trim(date2),'GOT',fmtdate(dat,'year-month-day'),'expected',date2)

   ! convert DAT to ISO week date, all generated dates should match ISO week date
   call unit_test('fmtdate',fmtdate(dat,"%I") == iso_week_date, msg=iso_week_date)
enddo

dat=[1957, 3, 2,-240, 2,0,0,0]
expected='|Mar|March|Sat|Saturday|'
returned=fmtdate(dat,'|%l|%L|%w|%W|')
call unit_test('fmtdate',returned.eq.expected,'macros: expected',expected,'returned',returned)

expected='|March|Mar|Mar|Sat|Sat|Saturday|'
returned=fmtdate(dat,'|MONTH|Month|Mth|Weekday|wkday|WEEKDAY|')
call unit_test('fmtdate',returned.eq.expected,'keywords: expected',expected,'returned',returned)

call unit_test_end('fmtdate')

end subroutine test_fmtdate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fmtdate_usage
call unit_test_end('fmtdate_usage')
end subroutine test_fmtdate_usage
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_guessdate
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
do i=1,size(line)-1
 read(line(i),*)date1,date2,iso_week_date,comment
 call guessdate(date1,dat)                                         ! convert date string to DAT
 call unit_test('guessdate',&
 &fmtdate(dat,"%I") == iso_week_date,'input',date1,'produced',fmtdate(dat,"%I"),'expected',iso_week_date)
 call unit_test('guessdate',&
 &fmtdate(dat,"year-month-day") == date2,'input',date1,'produced',fmtdate(dat,"year-month-day"),'expected',date2)
enddo

call unit_test_end('guessdate')

end subroutine test_guessdate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dow
integer          :: dat(8)     ! input date array
integer          :: weekday
character(len=9) :: day
integer          :: ierr
call date_and_time(values=dat)
dat=[1957,3,2,dat(4),12,0,0,0]

call dow(dat, weekday, day, ierr)
call unit_test('dow',day == 'Saturday'.and.weekday == 6,' expected Saturday,6 and got ',day,weekday)

dat(3)=dat(3)+1
call dow(dat, weekday, day, ierr)
call unit_test('dow',day == 'Sunday'.and.weekday == 7,' expected Sunday,7 and got ',day,weekday)

dat(3)=dat(3)+1
call dow(dat, weekday, day, ierr)
call unit_test('dow',day == 'Monday'.and.weekday == 1,' expected Monday,1 and got ',day,weekday)

dat(3)=dat(3)+1
call dow(dat, weekday, day, ierr)
call unit_test('dow',day == 'Tuesday'.and.weekday == 2,' expected Tuesday,2 and got ',day,weekday)

dat(3)=dat(3)+1
call dow(dat, weekday, day, ierr)
call unit_test('dow',day == 'Wednesday'.and.weekday == 3,' expected Wednesday,3 and got ',day,weekday)

dat(3)=dat(3)+1
call dow(dat, weekday, day, ierr)
call unit_test('dow',day == 'Thursday'.and.weekday == 4,' expected Thursday,4 and got ',day,weekday)

dat(3)=dat(3)+1
call dow(dat, weekday, day, ierr)
call unit_test('dow',day == 'Friday'.and.weekday == 5,' expected Friday,5 and got ',day,weekday)

call unit_test_end('dow')
end subroutine test_dow
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_w2d
character(len=372),allocatable :: line(:)
integer            :: y,m,d
integer            :: iso_year
integer            :: iso_week
integer            :: iso_weekday
integer            :: dat(8)
integer            :: i

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
   call unit_test('w2d',dat(1) == y.and.dat(2) == m.and.dat(3) == d,msg=line(i))   ! all should match
enddo

call unit_test_end('w2d')

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
call box_month(dat,calendar)
if(unit_test_level > 0)then
   write(*,'(a)')calendar
   write(*,'(a)')mnth
endif
call unit_test('box_month',all(calendar == mnth),'July 2016')
call unit_test_end('box_month')
end subroutine test_box_month
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2j
real(kind=realtime)         :: julian
integer :: dat(8)

   dat=[1970, 1, 1,0, 0,0,0,0]
   julian=d2j( dat )
   call unit_test('d2j',abs(julian-2440587.5d0) < 0.00001 ,msg="Dec 31st, 1969  8:00(2440587.5)")

   dat=[1995, 1, 1,0,12,0,0,0]
   julian=d2j( dat )
   call unit_test('d2j',int(julian) == 2449719 ,msg="Jan  1st, 1995 12:00(2449719)")

   dat=[1995,10,19,0,12,0,0,0]
   julian=d2j( dat )
   call unit_test('d2j',int(julian) == 2450010, msg="Oct 19th, 1995 12:00(2450010)")

   dat=[1995,12,31,0,12,0,0,0]
   julian=d2j( dat )
   call unit_test('d2j',int(julian) == 2450083, msg="Dec 31st, 1995 12:00(2450083)")

   dat=[1996, 1, 1,0,12,0,0,0]
   julian=d2j( dat )
   call unit_test('d2j',int(julian) == 2450084, msg="Jan  1st, 1996 12:00(2450084)")

   dat=[1996,12,31,0,12,0,0,0]
   julian=d2j( dat )
   call unit_test('d2j',int(julian) == 2450449, msg="Dec 31th, 1996 12:00(2450449)")

call unit_test_end('d2j')

end subroutine test_d2j
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_j2d
real(kind=realtime)          :: juliandate
character(len=:),allocatable :: expected
character(len=:),allocatable :: resulted
integer                      :: dat(8)

   juliandate=2457589.129d0                 ! set sample Julian Date

   expected='2016-07-19 11:05:45'
   dat=j2d(juliandate)
   resulted=fmtdate(dat,'year-month-day hour:minute:second')
   call unit_test('j2d',resulted == expected, juliandate,'==> EXPECTED ',expected,' GOT ',resulted)

   ! go back one day
   expected='2016-07-18 11:05:45'
   dat=j2d(juliandate-1.0d0)
   resulted=fmtdate(dat,'year-month-day hour:minute:second')
   call unit_test('j2d',resulted == expected, juliandate,'==> EXPECTED ',expected,' GOT ',resulted)

   ! go forward one day
   expected='2016-07-20 11:05:45'
   dat=j2d(juliandate+1.0d0)
   resulted=fmtdate(dat,'year-month-day hour:minute:second')
   call unit_test('j2d',resulted == expected, juliandate,'==> EXPECTED ',expected,' GOT ',resulted)

call unit_test_end('j2d')

end subroutine test_j2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2u()
integer,parameter :: aday(*)= [2017,03,29,-240,01,46,47,0]

!  Note that time zones are usually -HHMM or -HH:MM and not MM, which is what the DAT array uses
!  Comparing to Unix date(1) command:
!    date --date "Wed Mar 29 01:46:47 EDT 2017" +%s      ! 1490766407
!    date --date "Wed Mar 29 01:46:47 2017" +%s          ! 1490766407
!    date --date "Wed Mar 29 01:46:47 -400 2017" +%s     ! 1490766407
!    date --date "Wed Mar 29 01:46:47 UTC-400 2017" +%s  ! 1490766407
!    date --date "Wed Mar 29 01:46:47 UTC-4:00 2017" +%s ! 1490766407

   ! nint() changed to int(anint() to avoid gfortran OpenBSD bug on i386
   call unit_test('d2u',int(anint(d2u(aday))) == 1490766407,d2u(aday) )

   call unit_test_end('d2u')

end subroutine test_d2u
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_u2d
integer :: ex(8)
integer :: re(8)
integer :: utime
   utime=1490766407
   ex=[2017,03,29,-240,01,46,47,0]
   re=u2d(1490766407)
   call unit_test('u2d',all(re == ex),&
   & 'EXPECTED',1490766407, &
   & 'GOT',d2u(ex) )
call unit_test_end('u2d')
end subroutine test_u2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sec2days()

   call unit_test('sec2days',sec2days(129860) ==              '1-12:04:20','129860 is 1-12:04:20')
   call unit_test('sec2days',sec2days(80000.0d0) ==           '0-22:13:20','80000.0d0 is 0-22:13:20')
   call unit_test('sec2days',sec2days(80000,crop=.true.) ==     '22:13:20','80000 is 22:13:20')
   call unit_test('sec2days',sec2days('1day 2hr 3 min 4s') == '1-02:03:04','1day 2hr 3 min 4s is 1-02:03:04')

   call unit_test_end('sec2days')

end subroutine test_sec2days
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_days2sec()
   ! add 0 to nint function because of gfortran-11 bug passing some arguments with functions to class(*)
   call unit_test('days2sec',0+nint(days2sec('1')) ==              1, 'expected',1,'got',0+nint(days2sec('1')))
   call unit_test('days2sec',0+nint(days2sec('1:00')) ==          60,'expected',60,'got',0+nint(days2sec('1:00')))
   call unit_test('days2sec',0+nint(days2sec('1:00:00')) ==     3600, 'expected',3600,'got',0+nint(days2sec('1:00:00')))
   call unit_test('days2sec',0+nint(days2sec('1-00:00:00')) == 86400, 'expected',86400,'got',0+nint(days2sec('1-00:00:00')))
   call unit_test('days2sec',&
   &nint(days2sec('1d2h 3.0 minutes 4sec')) == 93784,'expected',1,'got',0+nint(days2sec('1d2h 3.0 minutes 4sec')))

   call unit_test('days2sec',0+nint(days2sec(' 1-12:04:20              '))  ==  129860, &
   & 'expected',129860,'got',0+nint(days2sec('1.12:03:20')))
   call unit_test('days2sec',0+nint(days2sec(' 1.5 days                '))  ==  129600, &
   & 'expected',129600,'got',0+nint(days2sec('1.5 days')))
   call unit_test('days2sec',0+nint(days2sec(' 1.5 days 4hrs 30minutes '))  ==  145800, &
   & 'expected',145800,'got',0+nint(days2sec('1.5 days 4hrs 30minutes')))
   call unit_test('days2sec',0+nint(days2sec(' 1.5d                    '))  ==  129600, &
   & 'expected',129600,'got',0+nint(days2sec('1.5d')))
   call unit_test('days2sec',0+nint(days2sec(' 1d2h3m4s                '))  ==  93784, &
   & 'expected',93784,'got',0+nint(days2sec('1d2h3m4s')))
   call unit_test('days2sec',0+nint(days2sec(' 1d1d1d                  '))  ==  259200, &
   & 'expected',259200,'got',0+nint(days2sec('1d1d1d')))
   call unit_test('days2sec',0+nint(days2sec(' 4d-12h                  '))  ==  302400, &
   & 'expected',302400,'got',0+nint(days2sec('4d-12h')))
   call unit_test('days2sec',0+nint(days2sec(' 3  d  1 2   h           '))  ==  302400, &
   & 'expected',302400,'got',0+nint(days2sec('3 d 1 s  h')))

   call unit_test_end('days2sec')

end subroutine test_days2sec
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_phase_of_moon
integer  :: dat(8)=[2018,11,3,-240,20,18,44,245]
call unit_test('phase_of_moon',phase_of_moon(dat) == 'Waning crescent')
call unit_test_end('phase_of_moon')
end subroutine test_phase_of_moon
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_moon_fullness
integer  :: dat(8)=[2018,11,3,-240,20,18,44,245]
call unit_test('moon_fullness', moon_fullness(dat) == -30)
call unit_test_end('moon_fullness')
end subroutine test_moon_fullness
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_easter()
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

do ii=1,size(tests)
   readme=tests(ii) ! cannot do internal read on a parameter
   read(readme,*)inyear,tmonth,tday
   call easter(inyear,dat)
   outmonth=dat(2)
   outday=dat(3)
   call unit_test('easter', tmonth == outmonth.and.tday == outday ,tests(ii),'month=',mon(outmonth))
enddo

call unit_test_end('easter') ! assume if got here passed checks

end subroutine test_easter
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_now_ex
call unit_test_end('now_ex')
end subroutine test_now_ex
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_d2w()

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

   call unit_test_end('d2w') ! assume if got here passed checks
end subroutine test_d2w

subroutine showme(string)
character(len=*) :: string
integer          :: iyear,iweek,iweekday
character(len=10):: name
   call d2w(dat,iyear,iweek,iweekday,name)
   call unit_test('d2w', name == string ,iyear,iweek,iweekday,name,string)
end subroutine showme
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
