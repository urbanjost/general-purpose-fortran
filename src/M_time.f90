!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_time
use M_strings, only : upper, lower,  substitute, split, adjustc
use M_strings, only : string_to_values, s2v, v2s
use M_strings, only : compact, transliterate
use, intrinsic :: iso_fortran_env, only : int64
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdout=>OUTPUT_UNIT,stdin=>INPUT_UNIT
implicit none !(external,type)

! ident_1="@(#) M_time M_time(3f) date and time function module"

private
!-----------------------------------------------------------------------------------------------------------------------------------
! version: 6E61627255202E53206E686F4A-2022-03-18
!-----------------------------------------------------------------------------------------------------------------------------------
! UNIT TESTS

! EPOCH TIME (UT starts at 0000 on 1 Jan. 1970)
   public date_to_unix   !(dat,UNIXTIME,IERR)                 ! Convert date array to Unix Time
   public unix_to_date   !(unixtime,DAT,IERR)                 ! Convert Unix Time to date array
   public d2u            !(dat) result(UNIXTIME)              ! Convert date array to Unix Time
   public u2d            !(unixtime) result(DAT)              ! Convert Unix Time to date array
! JULIAN
   public julian_to_date !(julian,DAT,IERR)                   ! Convert Julian Date to date array
   public date_to_julian !(dat,JULIAN,IERR)                   ! Convert date array to Julian Date
   public d2j            !(dat) result(JULIAN)                ! Convert date array to Julian Date
   public j2d            !(julian) result(DAT)                ! Convert Julian Date to date array
! DAY OF WEEK
   public dow            !(dat,[WEEKDAY],[DAY],IERR)          ! Convert date array to day of the week as number(Mon=1) and name
! WEEK OF YEAR
   public d2w !(dat,ISO_YEAR,ISO_WEEK,ISO_WEEKDAY,ISO_NAME)   ! Calculate iso-8601 Week numerically and as string "yyyy-Www-d"
   public w2d !(iso_year,iso_week,iso_weekday,DAT)            ! given iso-8601 Week-numbering year date yyyy-Www-d calculate date
              !(iso_wee_string,DAT,IERR)
! ORDINAL DAY
   public d2o            !(dat) result(ORDINAL)               ! given date array return ordinal day of year, Jan 1st=1
   public o2d            !(ordinal) result(DAT)               ! given ordinal day of year return date array, Jan 1st=1
   public ordinal_to_date!(year,ordinal_day,DAT)              ! given ordinal day of year return date array, Jan 1st=1
   public ordinal_seconds!()                                  ! seconds since the beginning of current year
! PRINTING DATES
   public fmtdate        !(dat,format) result(TIMESTR)        ! Convert date array to string using format
   public fmtdate_usage  !(indent)                            ! display macros recognized by fmtdate(3f)
   public now            !(format) result(NOW)                ! return string representing current time given format
   public box_month      !(dat,CALEN)                         ! print specified month into character array
! PRINTING DURATIONS
   public sec2days       !(seconds) result(dhms)              ! converts seconds to string D-HH:MM:SS
   public days2sec       !(str) result(seconds)               ! converts string D-HH:MM:SS to seconds from small to large
! MONTH NAME
   public mo2v           !(month_name) result(MONTH_NUMBER)       ! given month name return month number
   public v2mo           !(month_number,short) result(MONTH_NAME) ! given month number return month name
   public mo2d           !(month_name,year) result(DAT)           ! given month name and year return date array for 1st day of month
! LOCALE
   public locale         !(locale_name,mths,wkds,mths_abbr,wkds_abbr,ierr)  ! user-specified strings to use for month and weekday
! ASTROLOGICAL
   public easter         !(year,dat)                          ! calculate month and day Easter falls on for given year
   public moon_fullness  !(dat) result(FULLNESS)              ! percentage of moon phase from new to full
   public phase_of_moon  !(dat) result(PHASE)                 ! return name for phase of moon for given date
!x! public ephemeris      !(dat,planet,DD,DM,DC,AH,AM)         ! ephemeris position of planets for adjusting an equatorial telescope
! READING DATES
   public guessdate      !(anot,dat)                          ! Converts a date string to a date array, in various formats
! C INTERFACE
   public system_sleep   !(wait_seconds)                      ! Call sleep(3c)
   private call_sleep
   private call_usleep
!-----------------------------------------------------------------------------------------------------------------------------------
integer,parameter          :: dp=kind(0.0d0)
integer,parameter,public   :: realtime=kind(0.0d0)           ! type for unix epoch time and julian days
!-----------------------------------------------------------------------------------------------------------------------------------
! INTERNAL
real(kind=realtime),parameter,private :: SECDAY=86400.0_dp    ! 24:00:00 hours as seconds
!-----------------------------------------------------------------------------------------------------------------------------------
!  integer,parameter       :: igreg_1582=15+31*(10+12*1582)   ! ASSUMES: Gregorian Calendar was adopted 15 Oct. 1582 (588829)
!  integer,parameter       :: igreg_1752=03+31*( 9+12*1752)   ! ASSUMES: Gregorian Calendar was adopted 3 Sep. 1752 (652026)
!  integer,save            :: igreg=igreg_1582
!-----------------------------------------------------------------------------------------------------------------------------------
! CONVENIENT CONSTANTS FOR USE WITH + AND - OPERATORS
real(kind=realtime),public,parameter :: dt_minute=60.0_dp     ! one minute in seconds
real(kind=realtime),public,parameter :: dt_hour=3600.0_dp     ! one hour in seconds
real(kind=realtime),public,parameter :: dt_day=86400.0_dp     ! 24:00:00 hours in seconds
real(kind=realtime),public,parameter :: dt_week=dt_day*7.0_dp ! one week in seconds
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: gen='(*(g0,1x))'
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=:),save,allocatable,public :: M_time_weekday_names(:)
character(len=:),save,allocatable,public :: M_time_month_names(:)
character(len=:),save,allocatable,public :: M_time_weekday_names_abbr(:)
character(len=:),save,allocatable,public :: M_time_month_names_abbr(:)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: G_month_names(12)=[                               &
   &'January  ', 'February ', 'March    ', 'April    ', 'May      ', 'June     ', &
   &'July     ', 'August   ', 'September', 'October  ', 'November ', 'December ']

character(len=3),parameter   :: G_month_names_abbr(12)=G_month_names(:)(1:3)

character(len=*),parameter   :: G_weekday_names(7)=[character(len=9) :: &
   & 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday' ]

character(len=3),parameter   :: G_weekday_names_abbr(7)=G_weekday_names(:)(1:3)
!-----------------------------------------------------------------------------------------------------------------------------------
interface w2d
   module procedure w2d_numeric
   module procedure w2d_string
end interface w2d
!-----------------------------------------------------------------------------------------------------------------------------------
 contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    date_to_julian(3f) - [M_time:JULIAN] converts DAT date-time array to
!!    Julian Date
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine date_to_julian(dat,juliandate,ierr)
!!
!!     integer,intent(in)               :: dat(8)
!!     real(kind=realtime),intent(out)  :: juliandate
!!     integer,intent(out)              :: ierr
!!
!!##DESCRIPTION
!!   Converts a DAT date-time array to a Julian Date value.
!!
!!   Julian Dates (abbreviated JD) are simply a continuous count
!!   of days and fractions since noon Universal Time on January 1, 4713
!!   BC (on the Julian calendar). Julian dates are widely used as time
!!   variables within astronomical software. Typically, a 64-bit floating
!!   point (double precision) variable can represent an epoch expressed as
!!   a Julian date to about 20 microsecond precision.
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!           dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!
!!##RETURNS
!!    juliandate  A Julian Date (JD) is the number of days since
!!                noon (not midnight) on January 1st, 4713 BC.
!!
!!    ierr        Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_date_to_julian
!!     use M_time, only : date_to_julian,realtime
!!     implicit none
!!     integer             :: dat(8)
!!     real(kind=realtime) :: juliandate
!!     integer             :: ierr
!!        ! generate DAT array
!!        call date_and_time(values=dat)
!!        ! show DAT array
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        ! convert DAT to Julian Date
!!        call date_to_julian(dat,juliandate,ierr)
!!        write(*,*)'Julian Date is ',juliandate
!!        write(*,*)'ierr is ',ierr
!!     end program demo_date_to_julian
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:11:3:13:821
!!     Julian Date is    2457589.1272432986
!!     ierr is            0
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
subroutine date_to_julian(dat,julian,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!! AUTHOR:    John S. Urban
!!##VERSION:   2.0 2022-01-16
!! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19
! * There is no year zero
! * Julian Date must be non-negative
! * Julian Date starts at noon; while Civil Calendar date starts at midnight
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_2="@(#) M_time date_to_julian(3f) Converts proleptic Gregorian DAT date-time array to Julian Date"

integer,intent(in)               :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
real(kind=realtime),intent(out)  :: julian   ! Julian Date (non-negative, but may be non-integer)
integer,intent(out)              :: ierr     ! Error return: 0 =successful execution,-1=invalid year,-2=invalid month,-3=invalid day
                                             ! -4=invalid date (29th Feb, non leap-year)
integer                          :: year, month, day, utc, hour, minute
real(kind=realtime)              :: second
integer                          :: A, Y, M, JDN
!-----------------------------------------------------------------------------------------------------------------------------------
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0_dp   ! Second   ! correction for time zone and milliseconds
!-----------------------------------------------------------------------------------------------------------------------------------
   julian = -HUGE(99999)                  ! this is the date if an error occurs and IERR is < 0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(year==0 .or. year < -4713) then
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or February, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for February
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0_dp + dble(minute)/1440.0_dp + second/86400.0_dp
!-----------------------------------------------------------------------------------------------------------------------------------
   if(julian<0.0_dp) then                  ! Julian Day must be non-negative
      ierr=1
   else
      ierr=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine date_to_julian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    julian_to_date(3f) - [M_time:JULIAN] converts a JD(Julian Date)
!!    to a DAT date-time array.
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine julian_to_date(julian,dat,ierr)
!!
!!     real(kind=realtime),intent(in) :: julian
!!     integer,intent(out)            :: dat(8)
!!     integer,intent(out)            :: ierr
!!
!!##DESCRIPTION
!!   Converts a Julian Date(JD) value to a DAT date-time
!!   array.
!!
!!   Julian dates are simply a continuous count of days and
!!   fractions since noon Universal Time on January 1, 4713 BC (on the
!!   Julian calendar). Julian dates are widely used as time variables
!!   within astronomical software. Typically, a 64-bit floating point
!!   (double precision) variable can represent an epoch expressed as a
!!   Julian date to about 20 microsecond precision.
!!
!!##OPTIONS
!!     julian  Julian Date (days)
!!
!!##RETURNS
!!     dat     Integer array holding a "DAT" array, similar in structure
!!             to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!              dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!     Sample program:
!!
!!      program demo_julian_to_date
!!      use M_time, only : julian_to_date, fmtdate, realtime
!!      implicit none
!!      integer,parameter   :: dp=kind(0.0d0)
!!      real(kind=realtime) :: juliandate
!!      integer             :: dat(8)
!!      integer             :: ierr
!!         ! set sample Julian Date
!!         juliandate=2457589.129_dp
!!         ! create DAT array for this date
!!         call julian_to_date(juliandate,dat,ierr)
!!         write(*,*)'Sample Date=',fmtdate(dat)
!!         ! go back one day
!!         call julian_to_date(juliandate-1.0_dp,dat,ierr)
!!         write(*,*)'Day Before =',fmtdate(dat)
!!         ! go forward one day
!!         call julian_to_date(juliandate+1.0_dp,dat,ierr)
!!         write(*,*)'Day After  =',fmtdate(dat)
!!      end program demo_julian_to_date
!!
!!     Results:
!!
!!      Sample Date=Tuesday, July 19th, 2016 11:05:45 AM UTC-04:00
!!      Day Before =Monday, July 18th, 2016 11:05:45 AM UTC-04:00
!!      Day After  =Wednesday, July 20th, 2016 11:05:45 AM UTC-04:00
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
subroutine julian_to_date(julian,dat,ierr)

! ident_3="@(#) M_time julian_to_date(3f) Converts Julian Date to DAT date-time array"

real(kind=realtime),intent(in) :: julian            ! Julian Date (non-negative)
integer,intent(out)            :: dat(8)
integer,intent(out)            :: ierr              ! 0 for successful execution, otherwise 1
integer                        :: tz
real(kind=realtime)            :: second
integer                        :: year
integer                        :: month
integer                        :: day
integer                        :: hour
integer                        :: minute
integer                        :: jalpha,ja,jb,jc,jd,je,ijul

   if(julian<0.0_dp) then                     ! Negative Julian Date not allowed
      ierr=1
      return
   else
      ierr=0
   endif
   tz=get_timezone()

   ijul=idint(julian)                           ! Integral Julian Date
   second=sngl((julian-dble(ijul))*secday)      ! Seconds from beginning of Jul. Day
   second=second+(tz*60)

   if(second>=(secday/2.0_dp)) then           ! In next calendar day
      ijul=ijul+1
      second=second-(secday/2.0_dp)             ! Adjust from noon to midnight
   else                                         ! In same calendar day
      second=second+(secday/2.0_dp)             ! Adjust from noon to midnight
   endif

   if(second>=secday) then                    ! Final check to prevent time 24:00:00
      ijul=ijul+1
      second=second-secday
   endif

   minute=int(second/60.0_dp)                   ! Integral minutes from beginning of day
   second=second-dble(minute*60)                ! Seconds from beginning of minute
   hour=minute/60                               ! Integral hours from beginning of day
   minute=minute-hour*60                        ! Integral minutes from beginning of hour

   !---------------------------------------------
   jalpha=idint((dble(ijul-1867216)-0.25_dp)/36524.25_dp) ! Correction for Gregorian Calendar
   ja=ijul+1+jalpha-idint(0.25_dp*dble(jalpha))
   !---------------------------------------------

   jb=ja+1524
   jc=idint(6680.0_dp+(dble(jb-2439870)-122.1_dp)/365.25_dp)
   jd=365*jc+idint(0.25_dp*dble(jc))
   je=idint(dble(jb-jd)/30.6001_dp)
   day=jb-jd-idint(30.6001_dp*dble(je))
   month=je-1

   if(month>12)then
      month=month-12
   endif

   year=jc-4715
   if(month>2)then
      year=year-1
   endif

   if(year<=0)then
      year=year-1
   endif

   dat=[ year, month, day, tz, hour, minute, int(second), int((second-int(second))*1000.0)]
   ierr=0

end subroutine julian_to_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    date_to_unix(3f) - [M_time:UNIX_EPOCH] converts DAT date-time array to Unix
!!    Epoch Time
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine date_to_unix(dat,unixtime,ierr)
!!
!!     integer,intent(in)               :: dat(8)
!!     real(kind=realtime),intent(out)  :: unixtime
!!     integer,intent(out)              :: ierr
!!
!!##DESCRIPTION
!!   Converts a DAT date-time array to a UET (Unix Epoch Time).
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!              dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!##RETURNS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since
!!              00:00:00 on January 1st, 1970, UTC.
!!
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!     Sample program:
!!
!!      program demo_date_to_unix
!!      use M_time, only : date_to_unix, realtime
!!      implicit none
!!      integer             :: dat(8)
!!      real(kind=realtime) :: unixtime
!!      integer             :: ierr
!!         call date_and_time(values=dat)
!!         write(*,'(" Today is:",*(i0:,":"))')dat
!!         call date_to_unix(dat,unixtime,ierr)
!!         write(*,*)'Unix Epoch time is ',unixtime
!!         write(*,*)'ierr is ',ierr
!!      end program demo_date_to_unix
!!
!!     results:
!!
!!      Today is:2016:7:18:-240:23:44:20:434
!!      Unix Epoch time is    1468899860.4340105
!!      ierr is            0
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
subroutine date_to_unix(dat,unixtime,ierr)

! ident_4="@(#) M_time date_to_unix(3f) Convert DAT date-time array to Unix Epoch Time"

integer,intent(in)              :: dat(8)       ! date time array similar to that returned by DATE_AND_TIME
real(kind=realtime),intent(out) :: unixtime     ! Unix time (seconds)
integer,intent(out)             :: ierr         ! return 0 on success, otherwise 1
real(kind=realtime)             :: julian
real(kind=realtime),save        :: julian_at_epoch
logical,save                    :: first=.true.
integer,parameter               :: ref(*)=[1970,1,1,0,0,0,0,0]
!-----------------------------------------------------------------------------------------------------------------------------------
   if (first) then                                    ! Convert zero of Unix Epoch Time to Julian Date and save
      call date_to_julian(ref,julian_at_epoch,ierr)
      if(ierr /= 0) return                            ! Error
      first=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call date_to_julian(dat,julian,ierr)
   if(ierr /= 0) return                               ! Error
   unixtime=(julian-julian_at_epoch)*secday
end subroutine date_to_unix
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unix_to_date(3f) - [M_time:UNIX_EPOCH] converts Unix Epoch Time to
!!    DAT date-time
!!    array
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine unix_to_date(unixtime,dat,ierr)
!!
!!     real(kind=realtime),intent(in) :: unixtime
!!     integer,intent(out)            :: dat(8)
!!     integer,intent(out)            :: ierr
!!
!!##DESCRIPTION
!!   Converts a Unix Epoch Time (UET) to a DAT date-time array.
!!
!!##OPTIONS
!!
!!    unixtime  The "Unix Epoch" time, or the number of seconds since
!!              00:00:00 on January 1st, 1970, UTC; of type
!!              real(kind=realtime).
!!
!!##RETURNS
!!     dat      Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!               dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!     Sample program:
!!
!!      program demo_unix_to_date
!!      use M_time, only : unix_to_date, u2d, fmtdate, realtime
!!      implicit none
!!      integer,parameter :: dp=kind(0.0d0)
!!      real(kind=realtime)           :: unixtime
!!      ! seconds in a day
!!      real(kind=realtime),parameter :: DAY=86400.0_dp
!!      integer                       :: dat(8)
!!      integer                       :: ierr
!!         ! sample Unix Epoch time
!!         unixtime=1468939038.4639933_dp
!!         ! create DAT array for today
!!         call unix_to_date(unixtime,dat,ierr)
!!         write(*,*)'Sample Date=',fmtdate(dat)
!!         ! go back one day
!!         call unix_to_date(unixtime-DAY,dat,ierr)
!!         ! subtract day and print
!!         write(*,*)'Day Before =',fmtdate(dat)
!!         ! go forward one day
!!         call unix_to_date(unixtime+DAY,dat,ierr)
!!         ! add day print
!!         write(*,*)'Day After  =',fmtdate(dat)
!!      end program demo_unix_to_date
!!
!!    Results:
!!
!!     Sample Date=Tuesday, July 19th, 2016 10:37:18 AM
!!     Day Before =Monday, July 18th, 2016 10:37:18 AM
!!     Day After  =Wednesday, July 20th, 2016 10:37:18 AM
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
subroutine unix_to_date(unixtime,dat,ierr)

! ident_5="@(#) M_time unix_to_date(3f) Converts Unix Time to DAT date-time array"

class(*),intent(in)      :: unixtime                             ! Unix time (seconds)
integer,intent(out)      :: dat(8)                               ! date and time array
integer,intent(out)      :: ierr                                 ! 0 for successful execution, otherwise 1
real(kind=realtime)      :: julian                               ! Unix time converted to a Julian Date
real(kind=realtime)      :: local_unixtime
real(kind=realtime),save :: Unix_Origin_as_Julian                ! start of Unix Time as Julian Date
logical,save             :: first=.TRUE.
integer,parameter        :: ref(8)=[1970,1,1,0,0,0,0,0]
!  Notice that the value UNIXTIME can be any of several types ( INTEGER,REAL,REAL(KIND=REALTIME))
   select type(unixtime)
   type is (integer);             local_unixtime=dble(unixtime)
   type is (real);                local_unixtime=dble(unixtime)  ! typically not precise enough for UET values.
   type is (real(kind=realtime)); local_unixtime=unixtime
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   if(first)then                                                 ! Initialize calculated constants on first call
      call date_to_julian(ref,Unix_Origin_as_Julian,ierr)        ! Compute start of Unix Time as a Julian Date
      if(ierr/=0) return                                         ! Error
      first=.FALSE.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   julian=(local_unixtime/secday)+Unix_Origin_as_Julian          ! convert seconds from Unix Epoch to Julian Date
   call julian_to_date(julian,dat,ierr)                          ! calculate date-time array from Julian Date
   !dat(4)=get_timezone()                                        ! need to get time zone
end subroutine unix_to_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2o(3f) - [M_time:ORDINAL_DAY] converts DAT date-time array to Ordinal day
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function d2o(dat) result(ordinal)
!!
!!     integer,intent(in),optional :: dat(8)
!!     integer                     :: ordinal
!!
!!##DESCRIPTION
!!   Given a date in the form of a "DAT" array return the Ordinal Day,
!!   (ie. "the day of the year").
!!
!!##OPTIONS
!!     dat  Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!              dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!##RETURNS
!!     ordinal  The day of the year calculated for the given input date,
!!              where Jan 1st=1.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2o
!!     use M_time, only : d2o
!!     implicit none
!!     integer :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        write(*,*)'Day of year is:',d2o(dat)
!!
!!        ! year,month,day,timezone,hour,minute,seconds,milliseconds
!!        dat=[2020,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2021,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2022,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2023,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!        dat=[2024,12,31,-240,12,0,0,0]
!!        write(*,*)dat(1),' Days in year is:',d2o(dat)
!!
!!     end program demo_d2o
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:20:1:19:829
!!     Day of year is:         201
!!            2020  Days in year is:         366
!!            2021  Days in year is:         365
!!            2022  Days in year is:         365
!!            2023  Days in year is:         365
!!            2024  Days in year is:         366
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function d2o(dat) result(ordinal)

! ident_6="@(#) M_time d2o(3f) Converts DAT date-time array to Ordinal day"

! JSU 2015-12-13
integer,intent(in),optional :: dat(8)                 ! date time array similar to that returned by DATE_AND_TIME
integer                     :: dat_local(8)
integer                     :: ordinal                ! the returned number of days
real(kind=realtime)         :: unixtime               ! Unix time (seconds)
real(kind=realtime)         :: unix_first_day
integer                     :: ierr                   ! return 0 on success, otherwise 1 from date_to_unix(3f)
integer                     :: temp_dat(8)
   if(present(dat))then
     dat_local=dat
   else
     dat_local=getnow()
   endif
   call date_to_unix(dat_local,unixtime,ierr)         ! convert date to Unix Epoch Time
   if(ierr/=0)then
      write(stderr,gen)'<ERROR>*d2o*: bad date array'
      ordinal=-1                                      ! initialize to bad value
   else
      temp_dat=[dat_local(1),1,1,dat_local(4),0,0,0,0]
      call date_to_unix(temp_dat,unix_first_day,ierr)
      ordinal=int((unixtime-unix_first_day)/secday)+1
   endif
end function d2o
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    ordinal_seconds(3f) - [M_time:ORDINAL_DAY] seconds since beginning of year
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    function ordinal_seconds()
!!
!!     integer :: ordinal_seconds
!!##DESCRIPTION
!!   Return number of seconds since beginning of current year.
!!
!!   Before using this routine consider the consequences if the application
!!   is running at the moment a new year begins.
!!
!!##EXAMPLE
!!
!!    sample program
!!
!!     program demo_ordinal_seconds
!!     use M_time, only : ordinal_seconds
!!     implicit none
!!     character(len=*),parameter :: gen='(*(g0))'
!!     integer          :: i, ios, istart, iend
!!     real,volatile    :: x
!!     istart = ordinal_seconds()
!!     x = 0.0
!!     do i = 1, 1000000000
!!        x = x+sqrt(real(i))
!!     enddo
!!     print gen, 'x=',x
!!     iend = ordinal_seconds()
!!     print gen, 'that took ',iend-istart,' seconds'
!!     print gen, iend,'-',istart,'=',iend-istart
!!     end program demo_ordinal_seconds
!!
!!    Results:
!!
!!     > x=0.549755814E+12
!!     > that took 4 seconds
!!     > 23659912-23659908=4
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
integer function ordinal_seconds()

! ident_7="@(#) M_time ordinal_seconds(3f) seconds since beginning of year"

integer   :: v(8)
integer   :: ordinal_day_of_year
   associate(year=>v(1), month=>v(2), day=>v(3), timezone=>v(4), hour=>v(5), minutes=>v(6), seconds=>v(7), milliseconds=>v(8))
    v=getnow()
    ordinal_day_of_year=d2o(v)
    ordinal_seconds=ordinal_day_of_year*24*60*60 +hour*60*60 +minutes*60 +seconds
   end associate
end function ordinal_seconds
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    ordinal_to_date(3f) - [M_time:ORDINAL_DAY] when given a valid year and
!!    day of the year returns the DAT array for the date
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!      subroutine ordinal_to_date(yyyy, ddd, dat)
!!
!!       integer, intent(in)   :: yyyy
!!       integer, intent(in)   :: ddd
!!       integer, intent(out)  :: dat
!!##DESCRIPTION
!!   When given a valid year, YYYY, and day of the year, DDD, returns the
!!   date as a DAT date array
!!##OPTIONS
!!       yyyy  known year
!!       ddd   known ordinal day of the year
!!##RETURNS
!!       dat   DAT array describing the date
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_ordinal_to_date
!!     use M_time, only : ordinal_to_date
!!     implicit none
!!     integer :: yyyy, ddd, mm, dd, yy
!!     integer :: dat(8)
!!     integer :: i, iostat
!!     character(len=:),allocatable :: fakefile(:)
!!       fakefile=[character(len=80) :: ' 2024 273 ','2024 001']
!!       do i=1,size(fakefile)
!!          ! Enter year YYYY and ordinal day of year DD
!!          read(fakefile(i),*,iostat=iostat)yyyy,ddd
!!          if(iostat/=0)exit
!!          ! recover month and day from year and day number.
!!          call ordinal_to_date(yyyy, ddd, dat)
!!          yy=dat(1)
!!          mm=dat(2)
!!          dd=dat(3)
!!          write(*,'(*(g0))')'For Year ',yyyy,' and Ordinal day ',ddd
!!          write(*,'(*(g0))')' Month is ',mm,' and Day of Month is ',dd, &
!!          & ' and Year is ',yy
!!       enddo
!!     end program demo_ordinal_to_date
!!
!!    Result:
!!
!!     > For Year 2024 and Ordinal day 273
!!     > Month is 9 and Day of Month is 29 and Year is 2024
!!     > For Year 2024 and Ordinal day 1
!!     > Month is 1 and Day of Month is 1 and Year is 2024
subroutine ordinal_to_date(yyyy,ddd,dat)
!x!use M_time, only : d2j,j2d, realtime

! ident_8="@(#) M_time ordinal_to_date(3f) given a valid year and day of the year returns the DAT array for the date"

integer :: yyyy
integer :: ddd
integer :: dat(8)
integer :: temp_dat(8)
   !dat=[year,month,day,timezone,hour,minutes,seconds,milliseconds]
   ! find Julian day for first day of given year and add ordinal day -1 and convert back to a DAT
   temp_dat=[yyyy,1,1,0,12,0,0,0]
   dat=j2d( d2j(temp_dat) + real(ddd-1,kind=realtime) )
end subroutine ordinal_to_date
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    o2d(3f) - [M_time:ORDINAL_DAY] converts Ordinal day to DAT date-time array
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function o2d(ordinal,[year]) result(dat)
!!
!!     integer,intent(in) :: ordinal  ! the day of the year
!!     integer,optional   :: year     ! year
!!     integer            :: dat(8)   ! date time array
!!
!!##DESCRIPTION
!!   Given an Ordinal day of the year return a date in the form of a
!!   "DAT" array.
!!
!!##OPTIONS
!!     ordinal  The day of the year for the given year, where Jan 1st=1.
!!
!!     year     An optional year for the ordinal day. If not present the
!!              current year is assumed.
!!
!!##RETURNS
!!     dat   Integer array holding a "DAT" array, similar in structure
!!           to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!              dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!
!!           The timezone value is from the current time on the current
!!           platform.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_o2d
!!     use M_time, only : o2d,fmtdate
!!     implicit none
!!     integer :: year
!!        do year=2004,2008
!!           write(*,'(*(g0))')&
!!           & '100th day of ',year,' is ',fmtdate(o2d(100,year))
!!        enddo
!!        write(*,'(*(g0))')'100th day of this year is ',fmtdate(o2d(100))
!!     end program demo_o2d
!!
!!    Results:
!!
!!     > 100th day of 2004 is Friday, April 9th, 2004 12:00:00 AM UTC-05:00
!!     > 100th day of 2005 is Sunday, April 10th, 2005 12:00:00 AM UTC-05:00
!!     > 100th day of 2006 is Monday, April 10th, 2006 12:00:00 AM UTC-05:00
!!     > 100th day of 2007 is Tuesday, April 10th, 2007 12:00:00 AM UTC-05:00
!!     > 100th day of 2008 is Wednesday, April 9th, 2008 12:00:00 AM UTC-05:00
!!     > 100th day of this year is Monday, April 10th, 2023 12:00:00 AM UTC-05:00
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function o2d(ordinal,year) result(dat)

! ident_9="@(#) M_time o2d(3f) Converts ordinal day to DAT date-time array"

integer                     :: dat(8)                 ! date time array similar to that returned by DATE_AND_TIME
integer,intent(in)          :: ordinal                ! the returned number of days
integer,intent(in),optional :: year
real(kind=realtime)         :: unixtime               ! Unix time (seconds)
integer                     :: ierr                   ! return 0 on success, otherwise 1 from date_to_unix(3f)
   if(present(year))then
      dat=[year,1,ordinal,get_timezone(),0,0,0,0]     ! initialize DAT with parameters and set timezone, set HH:MM:SS.XX to zero
   else
      dat=getnow()                                    ! set year and timezone to current values
      dat=[dat(1),1,ordinal,dat(4),0,0,0,0]           ! apply ordinal parameter to January of current year, set HH:MM:SS.XX to zero
   endif
   ierr=0
   call date_to_unix(dat,unixtime,ierr)               ! convert date to Unix Epoch Time
   if(ierr/=0)then
      write(stderr,gen) '<ERROR>*o2d*: bad date array'
   else
      dat=u2d(unixtime)
   endif
end function o2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    v2mo(3f) - [M_time:MONTH_NAME] returns the month name of a Common
!!    month number
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function v2mo(imonth,short) result(month_name)
!!
!!     integer,intent(in)           :: imonth      ! month number (1-12)
!!     character(len=:),allocatable :: month_name  ! month name
!!     logical,intent(in),optional  :: short
!!
!!##DESCRIPTION
!!   Given a Common Calendar month number, return the name of the month
!!   as a string.
!!
!!##OPTIONS
!!    imonth      Common month number (1-12). If out of the allowable range
!!                the month name returned will be 'UNKNOWN'.
!!    short       Flag whether to return short or long name
!!##RETURNS
!!    month_name  A string representing a month name or the word 'UNKNOWN'
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_v2mo
!!     use M_time, only : v2mo
!!     implicit none
!!     integer :: i
!!        write(*,*)(v2mo(i),i=1,13)
!!     end program demo_v2mo
!!
!!    results:
!!
!!     January
!!     February
!!     March
!!     April
!!     May
!!     June
!!     July
!!     August
!!     September
!!     October
!!     November
!!     December
!!     UNKNOWN.
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function v2mo(imonth,short) result(month_name)

! ident_10="@(#) M_time v2mo(3f) returns the month name of a Common month number"

! JSU 2015-12-13
character(len=:),allocatable :: month_name                                        ! string containing month name or abbreviation.
integer,intent(in)           :: imonth                                            ! the number of the month(1-12)
logical,intent(in),optional  :: short
logical                      :: short_

   if(present(short))then
      short_=short
   else
      short_=.false.
   endif

   if(short_)then ! short names
      if(allocated(M_time_month_names_abbr))then                                          ! user user-specified month names
         if(size(M_time_month_names_abbr).ne.12)then
            write(stderr,gen) '<ERROR>*v2mo*: month name abbr. count not 12:',size(M_time_month_names_abbr)
            month_name='UNKNOWN'
         else
            select case(imonth)
            case (1:12);     month_name=trim(M_time_month_names_abbr(imonth))
            case default;    month_name='UNKNOWN'
            end select
         endif
      else
         select case(imonth)
         case (1:12);        month_name=trim(G_month_names(imonth)(1:3))
         case default;       month_name='UNKNOWN'
         end select
      endif
   else  ! long names
      if(allocated(M_time_month_names))then                                          ! user user-specified month names
         if(size(M_time_month_names).ne.12)then
            write(stderr,gen) '<ERROR>*v2mo*: month name count not 12:',size(M_time_month_names)
            month_name='UNKNOWN'
         else
            select case(imonth)
            case (1:12);     month_name=trim(M_time_month_names(imonth))
            case default;    month_name='UNKNOWN'
            end select
         endif
      else
         select case(imonth)
         case (1:12);        month_name=trim(G_month_names(imonth))
         case default;       month_name='UNKNOWN'
         end select
      endif
   endif

end function v2mo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    mo2d(3f) - [M_time:MONTH_NAME] given month name return DAT date-time
!!    array for beginning of that month in specified year
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!       function mo2d(month_name,year) result(dat)
!!
!!        character(len=*),intent(in) :: month_name
!!        integer,intent(in),optional :: year
!!        integer                     :: dat(8)
!!
!!##DESCRIPTION
!!   Given a Common Calendar month name, return the date as a "DAT" array
!!   for the 1st day of the month. An optional year that defaults to the
!!   current year may be specified.
!!
!!##OPTIONS
!!    month_name  A string representing a Common Calendar month name.
!!    year        Optional year. Defaults to current year
!!##RETURNS
!!    dat         An integer array that has the same structure as the array
!!                returned by the Fortran intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_mo2d
!!     use M_time, only : mo2d
!!     implicit none
!!        write(*,'("MARCH:",*(i0:,":"))')mo2d('March')
!!     end program demo_mo2d
!!
!!    Results:
!!
!!          > MARCH:2016:3:1:-240:0:0:0:0
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function mo2d(month_name,year) result(dat)

! ident_11="@(#) M_time mo2d(3f) month name to DAT date-time array for 1st of that month in specified year"

character(len=*),intent(in) :: month_name
integer,intent(in),optional :: year
integer                     :: dat(8)
   dat=getnow()
   if(present(year))then
      dat(1)=year
   endif
   dat(2)=mo2v(month_name) ! convert given month name to a number
   if(dat(2)<=0)then
      write(stderr,gen) '<ERROR>*mo2d*: bad month name:',trim(month_name)
      dat(2)=1
   endif
   dat(3)=1  ! set day to first of month
   dat(5)=0  ! set hour to zero
   dat(6)=0  ! set minutes to zero
   dat(7)=0  ! set seconds to zero
   dat(8)=0  ! set milliseconds to zero
end function mo2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    mo2v(3f) - [M_time:MONTH_NAME] given month name return month number
!!    (1-12) of that month
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    elemental function mo2v(month_name) result(imonth)
!!
!!      character(len=*),intent(in):: month_name ! month name
!!      integer                    :: imonth     ! month number
!!
!!##DESCRIPTION
!!   Given a string representing the name or abbreviation of a Gregorian
!!   Calendar month return a number representing the position of the
!!   month in the calendar starting with 1 for January and ending with
!!   12 for December.
!!
!!##OPTIONS
!!    month_name  name or abbreviation of month. Case is ignored.
!!##RETURNS
!!    imonth      month number returned. If the name is not recognized a -1
!!                is returned.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_mo2v
!!     use M_time, only : mo2v
!!     implicit none
!!        write(*,*)mo2v("April")
!!        write(*,*)mo2v('Apr')
!!        write(*,*)mo2v('sexember')
!!        write(*,*)mo2v('unknown')  ! returns -1
!!     end program demo_mo2v
!!
!!    results:
!!
!!       >  4
!!       >  4
!!       > -1
!!       > -1
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
elemental impure function mo2v(month_name) result(imonth)

! ident_12="@(#) M_time mo2v(3f) given month name return month number (1-12) of that month"

! JSU 2015-12-13
character(len=*),intent(in)  :: month_name   ! string containing month name or abbreviation.
integer                      :: imonth       ! the number of the month(1-12), or -1 if the name could not be recognized.
character(len=:),allocatable :: months(:)
character(len=:),allocatable :: upper_months(:)
character(len=:),allocatable :: upper_month_name
  upper_month_name = upper(month_name)     ! Case is ignored; test string now guaranteed to have three characters
  imonth = 0
  if(allocated(M_time_month_names))then
     upper_months=upper(M_time_month_names)
  else
     upper_months=upper(G_month_names)
  endif
  if(size(upper_months).ne.12)then
     write(stderr,gen) '<ERROR>*mo2v*: month name count not 12:',size(upper_months)
     months=['UNKNOWN']
  else
     months=pack(upper_months//'',upper_month_name.eq.upper_months(:)(:len_trim(upper_month_name))) ! concatenate for gfortran bug
     if(size(months).gt.1)months=pack(months//'',upper_month_name.eq.months)
     if(size(months).eq.0)months=['UNKNOWN']
  endif
  imonth=findloc(upper_months//'', months(1)//'',dim=1) ! concatenation avoids gfortran bug
  if(imonth.eq.0)imonth=-1
end function mo2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    now(3f) - [M_time:DATE_PRINTING] return string representing current
!!    time given one of many formats to present with
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function now(format) result(timestr)
!!
!!     character(len=*),intent(in)     :: format  ! input format string
!!     character(len=:),allocatable    :: timestr ! formatted date
!!
!!##DESCRIPTION
!!   The now(3f) function is a call to the fmtdate(3f) function using the
!!   current date and time. That is, it is a convenient way to print the
!!   current date and time.
!!
!!##OPTIONS
!!     format      string describing how to format the current date and time.
!!                 For a complete description of the formatting macros
!!                 supported see fmtdate_usage(3f).
!!##RETURNS
!!     timestr     formatted output string representing date
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_now
!!     use M_time, only : now
!!     implicit none
!!        write(*,*)now("The current date is &
!!           &year/month/day hour:minute:second timezone")
!!        write(*,*)now("The current date is &
!!           &WEEKDAY at HOUR GOOD, MONTH DAY, year")
!!        write(*,*)now("The current date is &
!!           &%w, %l %d, %Y %H:%m:%s %N")
!!        write(*,*)now("iso")
!!     end program demo_now
!! ```
!! Results:
!!
!!  >  The current date is 2024/06/28 14:56:36 -0400
!!  >  The current date is Friday at 2 PM, June 28th, 2024
!!  >  The current date is Fri, Jun 28th, 2024 2:56:36 PM
!!  >  2024-06-28T14:56:36-04:00
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function now(format)

! ident_13="@(#) M_time now(3f) return string representing current time given format"

! JSU 2015-10-24
character(len=*),intent(in),optional :: format
character(len=:),allocatable         :: now
integer                              :: values(8)
!-----------------------------------------------------------------------------------------------------------------------------------
   values=getnow()
   if(present(format))then
      now=fmtdate(values,format)
   else
      now=trim(fmtdate(values))
   endif
end function now
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmtdate(3f) - [M_time:DATE_PRINTING] given DAT date-time array return
!!    date as string using specified format
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function fmtdate(values,format) result(timestr)
!!
!!     integer,dimension(8),intent(in)      :: values
!!     character(len=*),intent(in),optional :: format
!!     character(len=:),allocatable         :: timestr
!!
!!##DESCRIPTION
!!   The fmtdate(3f) procedure lets you reformat a DAT array in
!!   many common formats using a special string containing macro names
!!   beginning with '%'. To see the allowable macros call or see the
!!   fmtdate_usage(3f) routine.
!!
!!##OPTIONS
!!     values   date in a "DAT" array, which is the same format as
!!              the values returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!     format   string describing how to format the "DAT" array.
!!              For a complete description of the formatting macros
!!              supported see fmtdate_usage(3f).
!!##RETURNS
!!     timestr  formatted output string representing date
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_fmtdate
!!     use M_time, only : fmtdate
!!     implicit none
!!     integer :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,*)fmtdate(dat,"current date: %w, %l %d, %Y %H:%m:%s %N")
!!     end program demo_fmtdate
!!
!!    results:
!!
!!       The current date is Sun, Jul 17th, 2016 01:21:35 PM
!!
!!##AUTHOR
!!    John S. Urban, 2015-12-19
!!
!!##LICENSE
!!    MIT
function fmtdate(values,format) result(timestr)

! ident_14="@(#) M_time fmtdate(3f) given DAT date-time array return date as string using format"

! JSU 2015-10-24
integer,dimension(8),intent(in)      :: values    ! numeric time values as DATE_AND_TIME(3f) intrinsic returns
character(len=*),intent(in),optional :: format    ! input format string
character(len=:),allocatable         :: timestr
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
integer,dimension(8)                 :: valloc    ! numeric time values as DATE_AND_TIME(3f) intrinsic returns
integer,parameter                    :: longest=4096
character(len=1)                     :: chara     ! character being looked at in format string
character(len=10)                    :: iso_name
character(len=2)                     :: dayend
character(len=:),allocatable         :: day       ! day of week
character(len=:),allocatable         :: local_format
character(len=longest)               :: text      ! character array
character(len=longest)               :: xxxx
integer                              :: i,ii,i10
integer                              :: ierr
integer                              :: iout
integer                              :: iso_year, iso_week, iso_weekday
integer                              :: systemclock, countrate
integer                              :: weekday
integer,save                         :: called=0
logical                              :: keyword   ! flag that previous character was a % character
logical,save                         :: since=.FALSE.
real(kind=realtime)                  :: cputime
real(kind=realtime)                  :: julian
real(kind=realtime)                  :: unixtime
real(kind=realtime),save             :: unixtime_last

   valloc=values
   if(present(format))then
      local_format=format
   else
      local_format=' '
   endif

   if(allocated(M_time_weekday_names))then      ! day must be allocated, make sure long enough for user-define names
      day=repeat(' ',len(M_time_weekday_names))
   else
      day=repeat(' ',len(G_weekday_names))
   endif

   select case(local_format)
   case('iso-8601W','isoweek') ; local_format='%I'                    ! 2016-W24-5 (yyyy-Www-d)
   case('iso-8601','iso')      ; local_format='%Y-%M-%DT%h:%m:%s%z'   ! 2006-08-14T02:34:56-0600
   case('sql')       ; local_format='"%Y-%M-%D %h:%m:%s.%x"'          !
   case('sqlday')    ; local_format='"%Y-%M-%D"'                      !
   case('sqltime')   ; local_format='"%h:%m:%s.%x"'                   !
   case('dash')      ; local_format='%Y-%M-%D'                        !
   case('rfc-2822')  ; local_format='%w, %D %l %Y %h:%m:%s %T'        ! Mon, 14 Aug 2006 02:34:56 -0600
   case('rfc-3339')  ; local_format='%Y-%M-%DT%h:%m:%s%z'             ! 2006-08-14 02:34:56-06:00
   case('suffix')    ; local_format='%Y%D%M%h%m%s'                    ! 20170122210327
   case('date')      ; local_format='%w %l %D %h:%m:%s UTC%z %Y'      ! Mon Jul 25 03:19:21 UTC-4:00 2016
   case('short')     ; local_format='%w, %l %d, %Y %H:%m:%s %N UTC%z' ! Fri, Jun 17th, 2016 06:31:00 PM UTC-04:00
   case('long')      ; local_format='%W, %L %d, %Y %H:%m:%s %N UTC%z' ! Friday, June 17th, 2016 06:31:00 PM UTC-04:00
   case(' ')         ; local_format='%W, %L %d, %Y %H:%m:%s %N UTC%z' ! Friday, June 17th, 2016 06:31:00 PM UTC-04:00
   case('formal')    ; local_format='The %d of %L %Y'                 ! The 9th of November 2014
   case('lord')  ; local_format='the %d day of %L in the year of our Lord %Y' ! the 9th day of November in the year of our Lord 2014
   case('usage','?','help') ; local_format='%?'                       ! call fmtdate_usage
   case('easter')
      call easter(values(1), valloc)                                  ! given year get month and day Easter falls on
      local_format="Easter day: the %d day of %L in the year of our Lord %Y"
   case('all')
     local_format='&
     & Civil Calendar:%t%W %L %d%n&
     & Civil Date:%t%t%Y-%M-%D %h:%m:%s %z%n&
     & Julian Date:%t%t%J%n&
     & Unix Epoch Time:%t%E%n&
     & Day Of Year:%t%t%O%n&
     & ISO-8601 week:%t%t%I&
     &'
   case default
      xxxx=local_format
      if(index(xxxx,'%')==0)then               ! if no % characters try to guess what macros are present

         call substitute(xxxx,'year','%Y')
         call substitute(xxxx,'month','%M')
         call substitute(xxxx,'MONTH','%L')
         call substitute(xxxx,'Month','%l')
         call substitute(xxxx,'Mth','%l')

         call substitute(xxxx,'weekday','%u')
         call substitute(xxxx,'WEEKDAY','%W')
         call substitute(xxxx,'Weekday','%w')
         call substitute(xxxx,'wkday','%w')
         call substitute(xxxx,'today','%Y%M%D')
         call substitute(xxxx,'day','%D')
         call substitute(xxxx,'DAY','%d')

         call substitute(xxxx,'GOOD','%N')
         call substitute(xxxx,'HOUR','%H')

         call substitute(xxxx,'hour','%h')
         call substitute(xxxx,'minute','%m')
         call substitute(xxxx,'timezone','%T')
         call substitute(xxxx,'TIMEZONE','%z')
         call substitute(xxxx,'Timezone','%Z')

         call substitute(xxxx,'millisecond','%x')
         call substitute(xxxx,'second','%s')

         call substitute(xxxx,'epoch','%e')
         call substitute(xxxx,'julian','%j')
         call substitute(xxxx,'ordinal','%O')
         call substitute(xxxx,'AGE','%a')
         call substitute(xxxx,'age','%A')

         if(index(xxxx,'%')==0)then            ! if no % characters change every char to %char if a format macro letter
            do i=65,122
             select case(achar(i))
             case('A','B':'E','H':'J','L':'Q','S','T','U','W','Y','Z','a','b':'e','h':'m','n','o':'q','s':'u','w','x','z')
                 call substitute(xxxx,achar(i),'%'//achar(i))
             end select
            enddo
         endif
      endif
      local_format=trim(xxxx)
   end select

   text=' '
!  write string, when encounter a percent character do a substitution
   keyword=.FALSE.
   iout=1

   do i10=1,len(local_format)                  ! Read the FORMAT string and replace the "%" strings per the following rules:
      chara=local_format(i10:i10)
      if(chara=='%'.and..not.keyword)then
            keyword=.TRUE.
            cycle
      endif
      if(keyword)then
         keyword=.FALSE.
         select case(chara)
         !=====================================================================================
         case('%'); write(text(iout:),'(A1)')chara                        ! literal percent character
         !=====================================================================================
         case('a'); write(text(iout:),'(G0)')sec2days(d2u()-d2u(valloc))  ! time since now in d-h:m:s format
         !=====================================================================================
         case('A'); write(text(iout:),'(G0)')(d2u()-d2u(valloc))          ! time since now in seconds
         !=====================================================================================
         case('b'); write(text(iout:),'(A1)')' '                          ! space character
         !=====================================================================================
         case('B'); write(text(iout:),'(A1)')'!'                          ! exclamation (bang) character
         !=====================================================================================
         case('c'); call cpu_time(cputime)                                ! CPU_TIME()
                    write(text(iout:),'(G0)')cputime
         !=====================================================================================
         case('C'); called = called + 1                                   ! number of times this routine called
                    write(text(iout:),'(I0)')called
         !=====================================================================================
         case('d');                                                       ! the day of the month 1st..31st
                    dayend='  '
                    select case(valloc(3))
                    case(1,21,31); dayend='st'
                    case(2,22); dayend='nd'
                    case(3,23); dayend='rd'
                    case(4:20,24:30); dayend='th'
                    case default
                    end select
                    write(text(iout:),'(I0,a)')valloc(3),dayend
         !=====================================================================================
         case('D'); write(text(iout:),'(I2.2)')valloc(3)                  ! the day of the month 1..31
         !=====================================================================================
         case('e'); call date_to_unix(valloc,unixtime,ierr)               ! integer Unix Epoch time in seconds
                    write(text(iout:),'(G0)')nint(unixtime)
         !=====================================================================================
         case('E'); call date_to_unix(valloc,unixtime,ierr)               ! Unix Epoch time in seconds
                    write(text(iout:),'(G0)')unixtime
         !=====================================================================================
         case('h'); write(text(iout:),'(I2.2)')valloc(5)                  ! the hour of the day, in the range of 0 to 23
         !=====================================================================================
         case('H'); ii=mod(valloc(5),12)                                  ! hour of day in range 1..12
                    if(ii==0)then
                       ii=12
                    endif
                    write(text(iout:),'(I0)')ii
         !=====================================================================================
         case('i'); call d2w(valloc,iso_year,iso_week,iso_weekday,iso_name) ! return ISO-8601 week of year
                    write(text(iout:),'(I0)')iso_week
         !=====================================================================================
         case('I'); call d2w(valloc,iso_year,iso_week,iso_weekday,iso_name) ! return ISO-8601 Week as string of form "yyyy-Www-d"
                    write(text(iout:),'(a)')iso_name
         !=====================================================================================
         case('j'); call date_to_julian(valloc,julian,ierr)               ! integer Julian Day (truncated to integer)
                    write(text(iout:),'(I0)')int(julian)
         !=====================================================================================
         case('J'); call date_to_julian(valloc,julian,ierr)               ! Julian Date out to milliseconds
                    !write(text(iout:),'(I0,".",i3.3)')int(julian),nint((julian-int(julian))*1000.0)
                    write(text(iout:),'(g0)')julian
         !=====================================================================================
         case('k'); call system_clock(count=systemclock,count_rate=countrate)  ! systemclock/countrate
                    write(text(iout:),'(G0)')real(systemclock)/countrate
         !=====================================================================================
         case('K'); call system_clock(count=systemclock,count_rate=countrate)  ! system clock count
                    write(text(iout:),'(I0)') systemclock
         !=====================================================================================
         case('l'); write(text(iout:),'(A)')v2mo(valloc(2),short=.true.)  ! short name of the month of the year
         !=====================================================================================
         case('L'); write(text(iout:),'(A)')v2mo(valloc(2))               ! name of the month of the year
         !=====================================================================================
         case('m'); write(text(iout:),'(I2.2)')valloc(6)                  ! the minutes of the hour, in the range 0 to 59
         !=====================================================================================
         case('M'); write(text(iout:),'(I2.2)')valloc(2)                  ! month of year (1..12)
         !=====================================================================================
         case('N'); if( valloc(5)>=12)then                              ! AM||PM
                       write(text(iout:),'("PM")')
                    else
                       write(text(iout:),'("AM")')
                    endif
         !=====================================================================================
         case('n');
                    write(text(iout:),'(a)')new_line("A")
         !=====================================================================================
         case('O'); write(text(iout:),'(I3.3)')d2o(valloc)                ! Ordinal day of year
         !=====================================================================================
         case('o'); call date_to_unix(valloc,unixtime,ierr)               ! integer Unix Epoch time in seconds
                    write(text(iout:),'(G0)')floor(unixtime/86400)        ! number of whole days since Epoch time
         !=====================================================================================
         !=====================================================================================
         case('p'); write(text(iout:),'(A)')phase_of_moon(valloc)         ! phase of moon
         !=====================================================================================
         case('P'); write(text(iout:),'(i0,"%")')moon_fullness(valloc)    ! percent of fullness
         !=====================================================================================
         case('q'); write(text(iout:),'("''")')                           ! single quote (apostrophe)
         !=====================================================================================
         case('Q'); write(text(iout:),'(''"'')')                          ! double quote
         !=====================================================================================
         case('s'); write(text(iout:),'(I2.2)')valloc(7)                  ! the seconds of the minute, in the range 0 to 59
         !=====================================================================================
         case('S'); if(.not.since)then                                    ! seconds since last called
                       since=.TRUE.
                       call date_to_unix(valloc,unixtime_last,ierr)
                    endif
                    call date_to_unix(valloc,unixtime,ierr)
                    write(text(iout:),'(G0)')unixtime-unixtime_last
                    unixtime_last=unixtime
         !=====================================================================================
         case('t'); write(text(iout:),'(a1)')achar(9)                     ! tab character
         !=====================================================================================
         case('T'); write(text(iout:),'(SP,I3.2,SS,I2.2)')int(valloc(4)/60),abs(mod(valloc(4),60)) ! time from UTC as +-hhmm
         !=====================================================================================
         case('U'); call dow(valloc,weekday,day,ierr)
                    write(text(iout:),'(I1)')mod(weekday+7,7)+1           ! Return the day of the week, 1..7 Sunday=1
         !=====================================================================================
         case('u'); call dow(valloc,weekday,day,ierr)                     ! Return the day of the week, 1..7 Monday=1
                    write(text(iout:),'(I1)')weekday
         !=====================================================================================
         case('W'); call dow(valloc,weekday,day,ierr)                     ! Return the name of the day of the week
                    write(text(iout:),'(a)')day
         !=====================================================================================
         case('w'); call dow(valloc,weekday,day,ierr)                     ! Return the first abbreviation of the day of the week
                    if(ierr.ne.0)then
                       text(iout:)='ERROR'
                    else
                       if(allocated(M_time_weekday_names_abbr))then
                          text(iout:)=trim(M_time_weekday_names_abbr(weekday))
                       else
                          write(text(iout:),'(A)')trim(G_weekday_names_abbr(weekday))
                       endif
                    endif
         !=====================================================================================
         case('x'); write(text(iout:),'(I3.3)')valloc(8)                  ! the milliseconds of the second, in the range 0 to 999
         !=====================================================================================
         case('Y'); write(text(iout:),'(I0.4)')valloc(1)                  ! the year, including the century (for example, 1990)
         !=====================================================================================
         case('Z'); write(text(iout:),'(SP,I5.4,"m")')valloc(4)           ! time difference with respect to UTC in minutes
         !=====================================================================================
         case('z'); write(text(iout:),'(SP,I3.2,":",SS,I2.2)')int(valloc(4)/60),abs(mod(valloc(4),60)) ! time from UTC as +-hh:mm
         case('?'); write(text(iout:),'()'); call fmtdate_usage()
         !=====================================================================================
         case default
            write(text(iout:),'(A1)')chara
         !=====================================================================================
         end select
         !=====================================================================================
         iout=len_trim(text)+1
         if(iout>=longest)exit
      else
         write(text(iout:),'(A1)')chara;iout=iout+1
      endif
   enddo
   timestr=trim(text)
end function fmtdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmtdate_usage(3f) - [M_time:DATE_PRINTING] display macros recognized
!!    by fmtdate(3f) and now(3f)
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine fmtdate_usage(indent)
!!
!!     integer,intent(in),optional      :: indent
!!
!!##DESCRIPTION
!!
!!   The fmtdate_usage(3f) subroutine displays the formatting options
!!   available for use in procedures such as fmtdate(3f) and now(3f).
!!   It is typically used to produce up-to-date help text in commands
!!   that use the M_time(3fm) module, so that the formatting information
!!   only needs maintained in one place (this routine) and is easily
!!   displayed so users can quickly obtain a description of the formatting
!!   macros.
!!
!!##OPTIONS
!!     indent      how many spaces to prefix the output with, so that
!!                 calling programs can position the output. Default
!!                 for this optional parameter is three (3).
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_fmtdate_usage
!!     use M_time, only : fmtdate_usage
!!     implicit none
!!        call fmtdate_usage() ! see all formatting options
!!     end program demo_fmtdate_usage
!!
!!    results (actually call the routine to ensure this is up to date):
!!
!!     Description                                        Example
!!
!!     Base time array:
!!     (1) %Y -- year, yyyy                                2016
!!     (2) %M -- month of year, 01 to 12                   07
!!     (3) %D -- day of month, 01 to 31                    29
!!         %d -- day of month, with suffix (1st, 2nd,...)  29th
!!     (4) %Z -- minutes from UTC                          -0240m
!!         %z -- -+hh:mm from UTC                          -04:00
!!         %T -- -+hhmm  from UTC                          -0400
!!     (5) %h -- hours, 00 to 23                           10
!!         %H -- hour (1 to 12, or twelve-hour clock)      10
!!         %N -- midnight< AM <=noon; noon<= PM <midnight  AM
!!     (6) %m -- minutes, 00 to 59                         54
!!     (7) %s -- sec, 00 to 59                             08
!!     (8) %x -- milliseconds 000 to 999                   521
!!     Conversions:
!!         %E -- Unix Epoch time                           1469804048.5220029
!!         %e -- integer value of Unix Epoch time          1469804049
!!         %J -- Julian  date                              2457599.121
!!         %j -- integer value of Julian Date(Julian Day)  2457599
!!         %O -- Ordinal day (day of year)                 211
!!         %o -- Whole days since Unix Epoch date          17011
!!         %U -- day of week, 1..7 Sunday=1                6
!!         %u -- day of week, 1..7 Monday=1                5
!!         %i -- ISO week of year 1..53                    30
!!         %I -- iso-8601 week-numbering date(yyyy-Www-d)  2016-W30-5
!!      Names:
!!         %l -- abbreviated month name                    Jul
!!         %L -- full month name                           July
!!         %w -- first three characters of weekday         Fri
!!         %W -- weekday name                              Friday
!!         %p -- phase of moon                             New
!!         %P -- percent of way from new to full moon      -1%
!!      Literals:
!!         %% -- a literal %                               %
!!         %t -- tab character
!!         %b -- blank character
!!         %B -- exclamation(bang) character
!!         %n -- new line (system dependent)
!!         %q -- single quote (apostrophe)
!!         %Q -- double quote
!!      Duration:
!!         %a -- Time since now as d-h:m:s               1-12:34:30
!!         %A -- TIme since now as seconds               12810.4500
!!      Program timing:
!!         %c -- CPU_TIME(3f) output                     .21875000000000000
!!         %C -- number of times this routine is used    1
!!         %S -- seconds since last use of this format   .0000000000000000
!!         %k -- time in seconds from SYSTEM_CLOCK(3f)   723258.812
!!         %K -- time in clicks from SYSTEM_CLOCK(3f)    723258812
!!   Help:
!!         %? -- call fmtdate_usage
!!
!!    If no percent (%) is found in the format one of several
!!    alternate substitutions occurs.
!!
!!    If the format is composed entirely of one of the following
!!    keywords the following substitutions occur:
!!
!!      "iso-8601",
!!      "iso"           ==> %Y-%M-%DT%h:%m:%s%z
!!      "iso-8601W",
!!      "isoweek"       ==> %I 2016-W30-5
!!      "sql"           ==> "%Y-%M-%D %h:%m:%s.%x"
!!      "sqlday"        ==> "%Y-%M-%D"
!!      "dash"          ==> %Y-%M-%D
!!      "sqltime"       ==> "%h:%m:%s.%x"
!!      "rfc-2822"      ==> %w, %D %l %Y %h:%m:%s %T
!!      "rfc-3339"      ==> %Y-%M-%DT%h:%m:%s%z
!!      "date"          ==> %w %l %D %h:%m:%s UTC%z %Y
!!      "short"         ==> %w, %l %d, %Y %H:%m:%s %N UTC%z
!!      "long"," "      ==> %W, %L %d, %Y %H:%m:%s %N UTC%z
!!      "suffix"        ==> %Y%D%M%h%m%s
!!      "formal"        ==> The %d of %L %Y
!!      "lord"          ==> the %d day of %L in the year of our Lord %Y
!!      "easter"        ==> FOR THE YEAR OF THE CURRENT DATE:
!!                       Easter day: the %d day of %L in the year of our Lord %Y
!!      "all"           ==> A SAMPLE OF DATE FORMATS
!!      "usage|help|?"  ==> %?
!!
!!   Examples of single keywords
!!
!!    iso-8601
!!    iso       : 2024-06-29T08:56:48-04:00
!!    iso-8601W
!!    isoweek   : 2024-W26-6
!!    sql       : "2024-06-29 08:56:48.750"
!!    sqlday    : "2024-06-29"
!!    dash      : 2024-06-29
!!    sqltime   : 08:56:48.833
!!    rfc-2822  : Sat, 29 Jun 2024 08:56:48 -0400
!!    rfc-3339  : 2024-06-29T08:56:48-04:00
!!    date      : Sat Jun 29 08:56:48 UTC-04:00 2024
!!    short     : Sat, Jun 29th, 2024 8:56:48 AM UTC-04:00
!!    long      : Saturday, June 29th, 2024 8:56:48 AM UTC-04:00
!!    suffix    : 20242906085648
!!    formal    : The 29th of June 2024
!!    lord      : the 29th day of June in the year of our Lord 2024
!!    easter    : Easter day: the 31st day of March in the year of our Lord 2024
!!    all       : Civil Calendar: Saturday June 29th
!!                Civil Date: 2024-06-29 08:56:49 -04:00
!!                Julian Date: 2460491.0394568751
!!                Unix Epoch Time: 1719665809.0740056
!!                Day Of Year: 181
!!                ISO-8601 week: 2024-W26-6
!!
!!    otherwise the following words are replaced with the most
!!    common macros:
!!
!!    numeric values:
!!
!!       year     %Y  2016
!!       month    %M  07
!!       day      %D  29
!!       hour     %h  10
!!       minute   %m  54
!!       second   %s  08
!!       timezone %T  0400
!!
!!       epoch    %e  1469804049
!!       julian   %j  2457599
!!       ordinal  %O  211
!!       weekday  %u  5
!!       age      %A  13238944.3030
!!
!!    string values:
!!
!!       MONTH          %L  July
!!       Month|Mth      %l  Jul
!!       WEEKDAY        %W  Thursday
!!       Weekday|wkday  %w  Thu
!!       DAY            %d  7th
!!       TIMEZONE       %z  -04:00
!!       Timezone       %Z  -240m
!!       GOOD           %N  AM
!!       HOUR           %H  10
!!       AGE            %a  1200-10:30:40
!!
!!    If none of these keywords are found then every letter that
!!    is a macro is assumed to have an implied percent in front
!!    of it. For example:
!!
!!       YMDhms ==> %Y%M%D%h%m%s ==> 20160729105408
!!
!!##AUTHOR
!!    John S. Urban, 2015-10-24
!!
!!##LICENSE
!!    MIT
subroutine fmtdate_usage(indent)

! ident_15="@(#) M_time fmtdate_usage(3f) display macros recognized by fmtdate(3f)"

integer,intent(in),optional    :: indent
character(len=128),allocatable :: usage(:)
integer                        :: i,ii
character(len=:),allocatable   :: blanks
   if(present(indent))then ! set indent to passed value or, if value is not present, set indent to 3
      ii=indent
   else
      ii=3
   endif
   blanks=repeat(' ',ii)   ! define a prefix string to create specified indent
usage=[ CHARACTER(LEN=128) :: &
! 123456789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890 123456789 123456789 123456789 123456789 12345678
&'Description                                        Example%b ',&
&'%b                                                           ',&
&'%bBase time array:                                           ',&
&' (1) %%Y -- year, yyyy                                %Y     ',&
&' (2) %%M -- month of year, 01 to 12                   %M     ',&
&' (3) %%D -- day of month, 01 to 31                    %D     ',&
&'     %%d -- day of month, with suffix (1st, 2nd,...)  %d     ',&
&' (4) %%Z -- minutes from UTC                          %Z     ',&
&'     %%z -- -+hh:mm from UTC                          %z     ',&
&'     %%T -- -+hhmm  from UTC                          %T     ',&
&' (5) %%h -- hours, 00 to 23                           %h     ',&
&'     %%H -- hour (1 to 12, or twelve-hour clock)      %H     ',&
&'     %%N -- midnight< AM <=noon; noon<= PM <midnight  %N     ',&
&' (6) %%m -- minutes, 00 to 59                         %m     ',&
&' (7) %%s -- sec, 00 to 59                             %s     ',&
&' (8) %%x -- milliseconds 000 to 999                   %x     ',&
&'%bConversions:                                               ',&
&'     %%E -- Unix Epoch time                           %E     ',&
&'     %%e -- integer value of Unix Epoch time          %e     ',&
&'     %%J -- Julian  date                              %J     ',&
&'     %%j -- integer value of Julian Date(Julian Day)  %j     ',&
&'     %%O -- Ordinal day (day of year)                 %O     ',&
&'     %%o -- Whole days since Unix Epoch date          %o     ',&
&'     %%U -- day of week, 1..7 Sunday=1                %U     ',&
&'     %%u -- day of week, 1..7 Monday=1                %u     ',&
&'     %%i -- ISO week of year 1..53                    %i     ',&
&'     %%I -- iso-8601 week-numbering date(yyyy-Www-d)  %I     ',&
&'%b Names:                                                    ',&
&'     %%l -- abbreviated month name                    %l     ',&
&'     %%L -- full month name                           %L     ',&
&'     %%w -- first three characters of weekday         %w     ',&
&'     %%W -- weekday name                              %W     ',&
&'     %%p -- phase of moon                             %p     ',&
&'     %%P -- percent of way from new to full moon      %P     ',&
&'%b Literals:                                                 ',&
&'     %%%% -- a literal %%                               %%   ',&
&'     %%t -- tab character                             %t     ',&
&'     %%b -- blank character                           %b     ',&
&'     %%B -- exclamation(bang) character               %B     ',&
&'     %%n -- new line (system dependent)               %n     ',&
&'     %%q -- single quote (apostrophe)                 %q     ',&
&'     %%Q -- double quote                              %Q     ',&
&'%b Duration:                                                 ',&
&'     %%a -- Time since now as d-hh:mm:ss              %a     ',&
&'     %%A -- Time since now as seconds                 %A     ',&
&'%b Program timing:                                           ',&
&'     %%c -- CPU_TIME(3f) output                       %c     ',&
&'     %%C -- number of times this routine is used      %C     ',&
&'     %%S -- seconds since last use of this format     %S     ',&
&'     %%k -- time in seconds from SYSTEM_CLOCK(3f)     %k     ',&
&'     %%K -- time in clicks from SYSTEM_CLOCK(3f)      %K     ',&
&'%b Help:                                                     ',&
&'     %%? -- call fmtdate_usage()                             ',&
&'%b                                                           ',&
&'%bIf no percent (%%) is found in the format one of several   ',&
&'%balternate substitutions occurs.                            ',&
&'%b                                                           ',&
&'%bIf the format is composed entirely of one of the following ',&
&'%bkeywords the following substitutions occur:                ',&
&'%b "iso-8601",                                              ',&
&'%b "iso"          ==> %%Y-%%M-%%DT%%h:%%m:%%s%%z ==> %Y-%M-%DT%h:%m:%s%z   ',&
&'%b "iso-8601W",                                                                  ',&
&'%b "isoweek"      ==> %%I ==> %I                          ',&
&'%b "sql"          ==> "%%Y-%%M-%%D %%h:%%m:%%s.%%x" ==> "%Y-%M-%D %h:%m:%s.%x"',&
&'%b "sqlday"       ==> "%%Y-%%M-%%D" ==> "%Y-%M-%D"                ',&
&'%b "sqltime"      ==> "%%h:%%m:%%s.%%x" ==> "%h:%m:%s.%x"            ',&
&'%b "dash"         ==> %%Y-%%M-%%D ==> %Y-%M-%D                 ',&
&'%b "rfc-2822"     ==> %%w, %%D %%l %%Y %%h:%%m:%%s %%T      ',&
&'%b                    %w, %D %l %Y %h:%m:%s %T              ',&
&'%b "rfc-3339"     ==> %%Y-%%M-%%DT%%h:%%m:%%s%%z ==> %Y-%M-%DT%h:%m:%s%z   ',&
&'%b "date"         ==> %%w %%l %%D %%h:%%m:%%s UTC%%z %%Y      ',&
&'%b                    %w %l %D %h:%m:%s UTC%z %Y              ',&
&'%b "short"        ==> %%w, %%l %%d, %%Y %%H:%%m:%%s %%N UTC%%z',&
&'%b                    %w, %l %d, %Y %H:%m:%s %N UTC%z         ',&
&'%b "long"," "     ==> %%W, %%L %%d, %%Y %%H:%%m:%%s %%N UTC%%z',&
&'%b                    %W, %L %d, %Y %H:%m:%s %N UTC%z         ',&
&'%b "suffix"       ==> %%Y%%D%%M%%h%%m%%s ==> %Y%D%M%h%m%s           ',&
&'%b "formal"       ==> The %%d of %%L %%Y ==> The %d of %L %Y           ',&
&'%b "lord"         ==> the %%d day of %%L in the year of our Lord %%Y               ',&
&'%b                    the %d day of %L in the year of our Lord %Y                  ',&
&'%b "easter"       ==> FOR THE YEAR OF THE CURRENT DATE:       ',&
&'%b                    Easter day: the %%d day of %%L in the year of our Lord %%Y ',&
&'%b "all"          ==> A SAMPLE OF DATE FORMATS                ',&
&'%b "usage|help|?" ==> call fmtdate_usage                    ',&
&'%botherwise the following words are replaced with the most   ',&
&'%bcommon macros:                                             ',&
&'%b   year            %%Y  %Y                                 ',&
&'%b   month           %%M  %M                                 ',&
&'%b   day             %%D  %D                                 ',&
&'%b   timezone        %%z  %z                                 ',&
&'%b   hour            %%h  %h                                 ',&
&'%b   minute          %%m  %m                                 ',&
&'%b   second          %%s  %s                                 ',&
&'%b   millisecond     %%x  %x                                 ',&
&'%b   epoch           %%e  %e                                 ',&
&'%b   julian          %%j  %j                                 ',&
&'%b   ordinal         %%O  %O                                 ',&
&'%b   weekday         %%u  %u                                 ',&
&'%b   MONTH           %%L  July                               ',&
&'%b   Month|Mth       %%l  Jul                                ',&
&'%b   DAY             %%d  7th                                ',&
&'%b   HOUR            %%H  10                                 ',&
&'%b   GOOD            %%N  AM                                 ',&
&'%b   Weekday|wkday   %%w  Thu                                ',&
&'%b   WEEKDAY         %%W  Thursday                           ',&
&'%b   Timezone        %%Z  -240m                              ',&
&'%b   TIMEZONE        %%z  -04:00                             ',&
&'%b   age             %%a  100-22:59:01                       ',&
&'%b   AGE             %%A  23423008.543                       ',&
&'%bIf none of these keywords are found then every letter that ',&
&'%bis a macro is assumed to have an implied percent in front  ',&
&'%bof it. For example:                                        ',&
&'%b   YMDhms ==> %%Y%%M%%D%%h%%m%%s ==> %Y%M%D%h%m%s          ',&
&'%b                                                           ']
   write(*,'(a,a)')(blanks,(trim(now(usage(i)))),i=1,size(usage))
end subroutine fmtdate_usage
!-----------------------------------------------------------------------------------------------------------------------------------
! C for reference
! %U     week number of year, with Sunday as first day of week (00..53)
! %Z     alphabetic time zone abbreviation (e.g., EDT)
!        By default, date pads numeric fields with zeroes. The following optional flags may follow '%':
!        -      (hyphen) do not pad the field
!        _      (underscore) pad with spaces
!        0      (zero) pad with zeros
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    guessdate(3f) - [M_time:READING_DATES] reads in a date, in various formats
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine guessdate(anot,dat)
!!
!!     character(len=*),intent(in) :: anot
!!     integer,intent(out)         :: dat(8)
!!
!!##DESCRIPTION
!!
!!   Read in strings and except for looking for month names remove
!!   non-numeric characters and try to convert a string assumed to represent
!!   a date to a date-time array.
!!
!!   Years should always be expressed as four-digit numbers, and except for
!!   the special format yyyy-mm-dd the day should come after the year. Named
!!   months are preferred. If ambiguous the order is assumed to be day -
!!   month - year. Times are assumed to be of the form HH:MM:SS
!!
!!   It is planned that this routine will be superseded. As an alternative,
!!   a C routine exists in the standard C libraries that allows for
!!   expansive features when reading dates that can be called via the
!!   ISO_C_BINDING interface.
!!
!!##OPTIONS
!!    anot  A string assumed to represent a date including a year, month
!!          and day.
!!
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_guessdate
!!     use M_time, only : guessdate, fmtdate
!!     implicit none
!!     character(len=20),allocatable :: datestrings(:)
!!     character(len=:),allocatable  :: answer
!!     integer                       :: dat(8)
!!     integer                       :: i
!!        datestrings=[ &
!!        & 'January 9th, 2001   ',&
!!        & ' Tue Jul 19 2016    ',&
!!        & ' 21/12/2016         ',&
!!        & ' 4th of Jul 2004    ' ]
!!        do i=1,size(datestrings)
!!           write(*,'(a)')repeat('-',80)
!!           write(*,*)'TRYING ',datestrings(i)
!!           call guessdate(datestrings(i),dat)
!!           write(*,*)'DAT ARRAY ',dat
!!           answer=fmtdate(dat)
!!           write(*,*)'FOR '//datestrings(i)//' GOT '//trim(answer)
!!        enddo
!!     end program demo_guessdate
!!
!!    results:
!!
!!     ---------------------------------------------------------------------
!!     TRYING January 9th, 2001
!!     DAT ARRAY         2001  1  9   -240    0   0   0    0
!!     FOR January 9th, 2001  GOT Tuesday, January 9th, 2001 12:00:00 AM
!!     ---------------------------------------------------------------------
!!     TRYING  Tue Jul 19 2016
!!     DAT ARRAY         2016  7  19  -240    0   0   0    0
!!     FOR  Tue Jul 19 2016   GOT Tuesday, July 19th, 2016 12:00:00 AM
!!     ---------------------------------------------------------------------
!!     TRYING  21/12/2016
!!     DAT ARRAY         2016  12 21  -240    0   0   0    0
!!     FOR  21/12/2016        GOT Wednesday, December 21st, 2016 12:00:00 AM
!!     ---------------------------------------------------------------------
!!     TRYING  4th of Jul 2004
!!     DAT ARRAY         2004  7  4   -240    0   0   0    0
!!     FOR  4th of Jul 2004   GOT Sunday, July 4th, 2004 12:00:00 AM
!!
!!##LICENSE
!!    MIT
subroutine guessdate(datestring,dat,ier)

! ident_16="@(#) M_time guessdate(3f) Guess format of a date string to create a DAT date-time array"

! partially based on a concept from JRH 1991-03-19
! JSU, 20160729
!
! makes an odd number of assumptions trying to guess what date format is being used. If you know the format of your date
! values READ(3f) and parse them directly instead of using this procedure, even though it does a good job with common USA formats.
!
!x! REDO more rigorously with regular expressions and recognize standard formats directly

! NOTE : Main constraint is that day is input BEFORE year unless use YYYY-MM-DD and a : implies HH:MM:SS, no timezone names
!        Not rigorous. Gets most common formats but can easily make errors in all but simple unambiguous common date formats
character(len=*),intent(in)       :: datestring ! Date in string format
character(len=:),allocatable      :: datestring_local ! Date in string format
character(len=:),allocatable      :: temp
integer,intent(out)               :: dat(8)
integer,optional                  :: ier
integer                           :: ier_local
integer                           :: iye,mon,idy  ! Year, Month, Day
integer                           :: ihr,imi,ise  ! Hour, Minute, Second
integer                           :: itz, imill   ! Timezone, Milliseconds
character(len=len(datestring)*2)  :: buff
integer                           :: i,idum,ind
logical                           :: alpha
integer                           :: ios
integer                           :: itries
character(len=3),parameter        :: amon(12)=['JAN','FEB','MAR','APR','MAY','JUN', 'JUL','AUG','SEP','OCT','NOV','DEC']
integer,parameter                 :: idmon(12)=[31 , 28  , 31  , 30  , 31  , 30 , 31 , 31  , 30  , 31  , 30  , 31]
character(len=:),allocatable      :: scratch(:)
integer,parameter                 :: isize=40
real                              :: rvalues(isize)
character(len=2)                  :: ampm
integer                           :: iend, inums, ierr
logical                           :: number
logical                           :: verbose
integer                           :: loops

   dat=getnow()                                             ! get time zone of current process and set defaults
   iye=dat(1)
   mon=dat(2)
   idy=dat(3)
   itz=dat(4)                                               ! default is to use current timezone
   ihr=0
   imi=0
   ise=0
   imill=0

   ier_local=0
   rvalues=0.0
   datestring_local=''
   verbose=.false.

!-----------------------------------------------------------------------------------------------------------------------------------
   temp=' '//trim(upper(datestring))
   if(len(temp)>=2)then
      if(temp(2:2)=='?')then
         verbose=.true.
         temp=temp(3:)
      endif
   endif
   if(verbose)write(*,gen)'*guessdate* a',temp,'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   number=.false.                                        ! when transition from letter to number add a space
   do i=1,len(temp)
      select case(temp(i:i))
      case('A':'Z','/')
         if(number)then
            datestring_local=datestring_local//' '
         endif
         number=.false.
      case default
         number=.true.
      end select
      datestring_local=datestring_local//temp(i:i)
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------

   if(verbose)write(*,gen)'*guessdate* b',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill
   datestring_local=datestring_local//'                 '  ! pad string so substitute will fit if old string shorter than new string
   !make sure spaces are around month names
   call substitute(datestring_local,'JANUARY',' JAN ')
   call substitute(datestring_local,'FEBRUARY',' FEB ')
   call substitute(datestring_local,'MARCH',' MAR ')
   call substitute(datestring_local,'APRIL',' APR ')
   call substitute(datestring_local,'MAY',' MAY ')
   call substitute(datestring_local,'JUNE',' JUN ')
   call substitute(datestring_local,'JULY',' JUL ')
   call substitute(datestring_local,'AUGUST',' AUG ')
   call substitute(datestring_local,'SEPTEMBER',' SEP ')
   call substitute(datestring_local,'OCTOBER',' OCT ')
   call substitute(datestring_local,'NOVEMBER',' NOV ')
   call substitute(datestring_local,'DECEMBER',' DEC ')
   call substitute(datestring_local,'SEPT',' SEP ')

   call substitute(datestring_local,'JAN',' JAN ')
   call substitute(datestring_local,'FEB',' FEB ')
   call substitute(datestring_local,'MAR',' MAR ')
   call substitute(datestring_local,'APR',' APR ')
   call substitute(datestring_local,'MAY',' MAY ')
   call substitute(datestring_local,'JUN',' JUN ')
   call substitute(datestring_local,'JUL',' JUL ')
   call substitute(datestring_local,'AUG',' AUG ')
   call substitute(datestring_local,'SEP',' SEP ')
   call substitute(datestring_local,'OCT',' OCT ')
   call substitute(datestring_local,'NOV',' NOV ')
   call substitute(datestring_local,'DEC',' DEC ')

   ! assume T[0=9] is from yyyyy-mm-ddThh:mm:ss.xx ISO-8601 format (or SEPTnn,OCTnn AUGUSTnn, where space was added or name changed)
   call substitute(datestring_local,'T0',' 0')
   call substitute(datestring_local,'T1',' 1')
   call substitute(datestring_local,'T2',' 2')
   call substitute(datestring_local,'T3',' 3')
   call substitute(datestring_local,'T4',' 4')
   call substitute(datestring_local,'T5',' 5')
   call substitute(datestring_local,'T6',' 6')
   call substitute(datestring_local,'T7',' 7')
   call substitute(datestring_local,'T8',' 8')
   call substitute(datestring_local,'T9',' 9')

   call substitute(datestring_local,': ',':')
   call substitute(datestring_local,' :',':')

   if(verbose)write(*,gen)'*guessdate* A ',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   call substitute(datestring_local,'UTC',' ')
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(datestring_local,scratch,' ;,"''')
   if(verbose)write(*,gen)'*guessdate* B ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                                                       ! a leading +/- is assumed to be a timezone
      if( index("+-",scratch(i)(1:1)) /= 0)then
         if(index(scratch(i),':')/=0)then                                   ! assumed to be +-hh:mm
            call string_to_values(scratch(i),isize,rvalues,inums,':',ierr)
            if(inums>=2)then
               itz=60*nint(rvalues(1))+nint(rvalues(2))
            elseif(inums==1)then
               itz=60*nint(rvalues(1))
            endif
         else                                                                ! assumed to be +-mm
            itz=nint(s2v(scratch(i)))
         endif
         scratch(i)=' '
      endif
   enddo
   if(verbose)write(*,gen)'*guessdate* C ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                      ! AM and PM are assumed to only occur significantly (not end of day or month name, ...)
      if(len_trim(scratch(i))>=2)then
         iend=len_trim(scratch(i))
         ampm=scratch(i)(iend-1:iend)
         select case (ampm)
         case('AM')
            call substitute(scratch(i),'AM',':')
         case('PM')
            ihr=ihr+12
            call substitute(scratch(i),'PM',':')
         end select
      endif
   enddo
   if(verbose)write(*,gen)'*guessdate* E ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                                                      ! look for HH:MM:SS
      if(index(scratch(i),':')/=0)then
         buff=scratch(i)
         call substitute(buff,'-',' -')
         call substitute(buff,'+',' +')
         call string_to_values(buff,isize,rvalues,inums,':/',ierr)
         if(inums>=1) ihr=ihr+nint(rvalues(1))
         if(inums>=2) imi=nint(rvalues(2))
         if(inums>=3) ise=nint(rvalues(3))
         if(inums>=4) itz=nint(rvalues(4))
         scratch(i)=' '
      endif
   enddo
   if(verbose)write(*,gen)'*guessdate* F ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(scratch)                                                       ! assume yyyy-mm-dd if found a dash
      if(index(scratch(i),"-")/=0)then
            call string_to_values(scratch(i),isize,rvalues,inums,'-',ierr)
            select case(inums)
            case(3)
               iye=nint(rvalues(1))
               mon=nint(rvalues(2))
               idy=nint(rvalues(3))
               scratch(i)=v2s(nint(rvalues(3)))//' '//v2s(nint(rvalues(2)))//' '//v2s(nint(rvalues(1)))
            case(2)
               iye=nint(rvalues(1))
               mon=nint(rvalues(2))
               scratch(i)=v2s(nint(rvalues(2)))//' '//v2s(nint(rvalues(1)))
            case default
            end select
      endif
   enddo
   if(verbose)write(*,gen)'*guessdate* D ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   datestring_local=''
   do i=1,size(scratch)
      datestring_local=datestring_local//' '//adjustl(trim(scratch(i)))
   enddo
   if(verbose)write(*,gen)'*guessdate* G ',(trim(scratch(i)),'|',i=1,size(scratch)),'::',iye,mon,idy,itz,ihr,imi,ise,imill
!-----------------------------------------------------------------------------------------------------------------------------------
   if(datestring_local==' ')then
     loops=0
   else
     loops=1000
   endif
   if(verbose)write(*,gen)'*guessdate* Ga',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill,loops
   INFINITE: do itries=1,loops                              ! give up after 1000 passes
      buff=datestring_local                                 ! copy to buffer
      alpha=.false.

      do i=1,12
         ind=index(buff,amon(i))
         if(ind/=0) then                                  ! Found a matching month
            mon=i
            buff(ind:ind+2)='   '                           ! Delete month
            alpha=.true.                                    ! Alphabetic month
            exit
         endif
      enddo

      do i=1,len(buff)                                      ! First remove all non-numeric characters
         idum=iachar(buff(i:i))
         if(idum<48.or.idum>57)then
            buff(i:i)=' '
         endif
      enddo

      if(alpha) then                                        ! Alphabetic month
         read(buff,*,iostat=ios) idy,iye
         if(ios/=0)cycle INFINITE
      else
         read(buff,*,iostat=ios) idy,mon,iye
         if(ios/=0)cycle INFINITE
      endif
      !x!if(iye<=99)then
      !x!   iye=iye+2000                                       ! Cope with two digit year (assume 21st century.)
      !x!endif
      if(mon<1.or.mon>12) cycle INFINITE              ! Check range of months
      if(mon==2) then                                     ! Special check for Feb.
         if((iye/4)*4==iye) then                          ! Leap year
            if(idy<1.or.idy>29) cycle INFINITE
         else                                               ! Non-leap year
            if(idy<1.or.idy>28) cycle INFINITE
         endif
      else
         if(idy<1.or.idy>idmon(mon)) cycle INFINITE   ! Error ..... re-input
      endif
      exit
   enddo INFINITE
   if(verbose)write(*,gen)'*guessdate* H ',datestring_local,'::',iye,mon,idy,itz,ihr,imi,ise,imill
   if(itries>=1000)then
      write(stderr,gen)'<ERROR>*guessdate*: could not extract date for',trim(datestring)
   endif
   dat(1)=iye
   dat(2)=mon
   dat(3)=idy
   dat(4)=itz
   dat(5)=ihr
   dat(6)=imi
   dat(7)=ise
   dat(8)=imill
   if(present(ier))ier=ier_local
end subroutine guessdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dow(3f) - [M_time:DAY_OF_WEEK] given a date-time array DAT return
!!    the day of the week
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine dow(values, weekday, day, ierr, short)
!!
!!     integer,intent(in)                    :: values(8)
!!     integer,intent(out),optional          :: weekday
!!     character(len=*),intent(out),optional :: day
!!     integer,intent(out),optional          :: ierr
!!     logical,intent(in),optional           :: short
!!
!!##DESCRIPTION
!!   Given a date array DAT
!!   return the day of the week as a number and a name, Mon=1.
!!
!!##OPTIONS
!!    values   "DAT" array (an integer array of the same format as
!!             the array returned by the intrinsic DATE_AND_TIME(3f))
!!             describing the date to be used to calculate the day
!!             of the week.
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!##RETURNS
!!    weekday  The numeric day of the week, starting with Monday=1.
!!             Optional.
!!    day      The name of the day of the week.
!!             Optional.
!!    ierr     Error code
!!
!!             o [ 0] correct
!!             o [-1] invalid input date
!!             o [-2] neither day nor weekday
!!               return values were requested.
!!
!!             If the error code is not returned and an error occurs,
!!             the program is stopped.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_dow
!!     use M_time, only : dow
!!     implicit none
!!     integer          :: dat(8)     ! input date array
!!     integer          :: weekday
!!     character(len=9) :: day
!!     integer          :: ierr
!!       call date_and_time(values=dat)
!!       call dow(dat, weekday, day, ierr)
!!       write(*,'(a,i0)')'weekday=',weekday
!!       write(*,'(a,a)')'day=',trim(day)
!!       write(*,'(a,i0)')'ierr=',ierr
!!     end program demo_dow
!!
!!    results:
!!
!!     weekday=1
!!     day=Monday
!!     ierr=0
!!
!!##AUTHOR
!!    John S. Urban, 2015-12-19
!!
!!##LICENSE
!!    MIT
subroutine dow(values, weekday, day, ierr, short)

! ident_17="@(#) M_time dow(3f) Given DAT date-time array return the day of the week"

integer,intent(in)                    :: values(8) ! date and time array used to get time zone
integer,intent(out),optional          :: weekday   ! The day of the week, 1 = Monday, 7 = Sunday
character(len=*),intent(out),optional :: day       ! The name of the day of the week, e.g. 'Sunday'. Minimum length = 9
integer,intent(out),optional          :: ierr      ! Error code,0=correct,-1=invalid input date,-2=neither day nor weekday specified
logical,intent(in),optional           :: short
real(kind=realtime)                   :: julian    ! the Julian Date for which the weekday is required,
integer                               :: iweekday
integer                               :: ierr_local
logical                               :: short_

   call date_to_julian(values,julian,ierr_local)   ! need Julian Date to calculate day of week for first day of month
   ierr_local = 0
   iweekday=0  ! bad value.

   if(julian < 0) then
      ierr_local = -1
   elseif(.not.present(day).and. .not.present(weekday)) then
      ierr_local=-2
   else
      ! Julian Day is in Z time zone and starts at noon so add 1/2 day; and add time zone
      !iweekday = mod(int((julian+dble(values(4)/60.0_dp/24.0_dp)+0.5_dp)+1.0_dp), 7)
      ! REAL nint() changed to int(anint()) added to avoid bug in OpenBSD gfortran on i386
      iweekday = mod(int(anint(julian+dble(values(4)/60.0_dp/24.0_dp)))+1, 7)
      iweekday = iweekday +1  ! change range from 0 to 6 to 1 to 7
      iweekday = mod(iweekday+5,7)+1  ! change from Sunday=1 to Monday=1

      if(present(day)) then
         if(present(short))then
            short_=short
         else
            short_=.false.
         endif
         if(short_)then
            if(allocated(M_time_weekday_names_abbr))then
               if(size(M_time_weekday_names_abbr).ne.7)then
                  write(stderr,gen) '<ERROR>*dow*: weekday name abbr. count not 7:',size(M_time_weekday_names_abbr)
                  day='error'
               else
                  select case(iweekday)
                  case(1:7)   ;day = trim(M_time_weekday_names_abbr(iweekday))
                  case default;day = 'error'
                  end select
               endif
            else
               select case(iweekday)
               case(1:7)   ;day = trim(G_weekday_names_abbr(iweekday))
               case default;day = 'error'
               end select
            endif
         else
            if(allocated(M_time_weekday_names))then
               if(size(M_time_weekday_names).ne.7)then
                  write(stderr,gen) '<ERROR>*dow*: weekday name count not 7:',size(M_time_weekday_names)
                  day='error'
               else
                  select case(iweekday)
                  case(1:7)   ;day = M_time_weekday_names(iweekday)
                  case default;day = 'error'
                  end select
               endif
            else
               select case(iweekday)
               case(1:7)   ;day = G_weekday_names(iweekday)
               case default;day = 'error'
               end select
            endif
         endif
      endif

   endif

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local/=0)then
      write(stderr,gen) '<ERROR>*dow*: Unprocessed Error',ierr_local,'stopping.'
      stop 2
   endif

   if(present(weekday))then
      weekday=iweekday
   endif

end subroutine dow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2w(3f) - [M_time:WEEK_OF_YEAR] calculate iso-8601 Week, both
!!    numerically and as a string of the form "yyyy-Wmm-d" given a DAT
!!    date-time array
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine d2w(dat,iso_year,iso_week,iso_weekday,iso_name)
!!
!!     integer,intent(in)              :: dat(8)     ! input date array
!!     integer,intent(out)             :: iso_year, iso_week, iso_weekday
!!     character(len=10),intent(out)   :: iso_name
!!
!!##DESCRIPTION
!!   Given a "DAT" array defining a date and time, return the ISO-8601
!!   Week in two formats -- as three integer values defining the ISO year,
!!   week of year and weekday; and as a string of the form "yyyy-Www-d".
!!
!!##OPTIONS
!!    dat          "DAT" array (an integer array of the same format as
!!                 the array returned by the intrinsic DATE_AND_TIME(3f))
!!                 describing the date,
!!
!!                     dat=[ year,month,day,timezone,hour,&
!!                      & minutes,seconds,milliseconds]
!!##RETURNS
!!    iso_year     ISO-8601 year number for the given date
!!    iso_week     ISO-8601 week number for the given date
!!    iso_weekday  ISO-8601 weekday number for the given date
!!    iso_name     ISO-8601 Week string for the data in the form "yyyy-Www-d".
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2w
!!     use M_time, only : d2w
!!     implicit none
!!     integer           :: dat(8)     ! input date array
!!     integer           :: iso_year, iso_week, iso_weekday
!!     character(len=10) :: iso_name
!!        call date_and_time(values=dat)
!!        call d2w(dat,iso_year,iso_week,iso_weekday,iso_name)
!!        write(*,'("ISO-8601 Week:   ",a)')iso_name
!!        write(*,'(a,i0)')'ISO-8601 year    ',iso_year
!!        write(*,'(a,i0)')'ISO-8601 week    ',iso_week
!!        write(*,'(a,i0)')'ISO-8601 weekday ',iso_weekday
!!     end program demo_d2w
!!
!!    results:
!!
!!     ISO-8601 Week:   2016-W29-1
!!     ISO-8601 year    2016
!!     ISO-8601 week    29
!!     ISO-8601 weekday 1
!!
!!##DEFINITION
!!   The ISO-8601 date and time standard was issued by the International
!!   Organization for Standardization (ISO). It is used (mainly) in
!!   government and business for fiscal years, as well as in timekeeping.
!!   The system specifies a week year atop the Gregorian calendar by defining
!!   a notation for ordinal weeks of the year.
!!
!!   o An ISO week-numbering year (also called ISO year informally) has 52
!!     or 53 full weeks. That is 364 or 371 days instead of the usual 365
!!     or 366 days.
!!   o The extra week is referred to here as a leap week, although ISO-8601
!!     does not use this term.  Weeks start with Monday.
!!   o The first week of a year is the week that contains the first Thursday
!!     of the year (and, hence, always contains 4 January).  ISO week year
!!     numbering therefore slightly deviates from the Gregorian for some days
!!     close to January 1st.
!!
!!##CALCULATION
!!   The ISO-8601 week number of any date can be calculated, given its
!!   ordinal date (i.e. position within the year) and its day of the week.
!!
!!##METHOD
!!     Using ISO weekday numbers (running from 1 for Monday to 7 for Sunday),
!!     subtract the weekday from the ordinal date, then add 10. Divide the
!!     result by 7. Ignore the remainder; the quotient equals the week
!!     number. If the week number thus obtained equals 0, it means that
!!     the given date belongs to the preceding (week-based) year. If a
!!     week number of 53 is obtained, one must check that the date is not
!!     actually in week 1 of the following year.
!!
!!     These two statements are assumed true when correcting the dates
!!     around January 1st:
!!
!!     o The number of weeks in a given year is equal to the corresponding
!!       week number of 28 December.
!!     o January 4th is always in the first week.
!!
!!##ISO_NAME
!!   Week date representations are in the format YYYYWww-D.
!!
!!     o [YYYY] indicates the ISO week-numbering year which is slightly
!!       different from the traditional Gregorian calendar year.
!!     o [Www] is the week number prefixed by the letter W, from W01
!!       through W53.
!!     o [D] is the weekday number, from 1 through 7, beginning with Monday
!!       and ending with Sunday.
!!
!!   For example, the Gregorian date 31 December 2006 corresponds to the
!!   Sunday of the 52nd week of 2006, and is written
!!
!!     2006-W52-7 (extended form)
!!     or
!!     2006W527 (compact form).
!!
!!##REFERENCE
!!    From Wikipedia, the free encyclopedia 2015-12-19
!!
!!##AUTHOR
!!    John S. Urban, 2015-12-19
!!
!!##LICENSE
!!    MIT
subroutine d2w(dat,iso_year,iso_week,iso_weekday,iso_name)

! ident_18="@(#) M_time d2w(3f) DAT date-time array to iso-8601 Week-numbering year date yyyy-Www-d"

integer,intent(in)              :: dat(8)     ! input date array
integer,intent(out)             :: iso_year, iso_week, iso_weekday
character(len=10),intent(out)   :: iso_name
integer                         :: shared_weekday
integer                         :: last_week_this_year
integer                         :: dec28_lastyear(8)   ! December 28th is always in last week
integer                         :: dec28_thisyear(8)   ! December 28th is always in last week
character(len=9)                :: day
integer                         :: ierr
   iso_year=dat(1)                                               ! initially assume the iso_year is the same as the data array year
   iso_week=uncorrected_week_of_year(dat)                        ! this is the week number unless around January 1st
   iso_weekday=shared_weekday                                    ! this is the number of the day of the week assuming Monday=1
   dec28_thisyear=[dat(1),12,28,dat(4),0,0,0,0]                  ! Dec 28th is always in last week; use this to get number of weeks
   last_week_this_year=uncorrected_week_of_year(dec28_thisyear)  ! get the number of the last week of the year (52 or 53)

   ! correct dates around January 1st
   if(iso_week  < 1)then                                         ! if week < 1 then week = lastWeek(year -1)
      dec28_lastyear=[dat(1)-1,12,28,dat(4),0,0,0,0]             ! Dec 28th is always in last week, we want its week number
      iso_week=uncorrected_week_of_year(dec28_lastyear)          ! got the week number for the last week of last year (52 or 53)
      iso_year=dat(1)-1                                          ! our date belongs to last year
   elseif(iso_week >last_week_this_year)then                     ! if week > lastweek(year) then week = 1
      iso_week=iso_week-last_week_this_year                      ! our date belongs to next year
      iso_year=dat(1)+1
   endif

   write(iso_name,'(i4.4,"-W",i2.2,"-",i1)')iso_year,iso_week,iso_weekday ! create ISO string designation for our date

contains
   function uncorrected_week_of_year(datin)
   implicit none
   integer            :: uncorrected_week_of_year
   integer,intent(in) :: datin(8)
   integer            :: ordinal
      call dow(datin,shared_weekday,day,ierr)                 ! formula needs day of week 1..7 where Monday=1
      ordinal=d2o(datin)                                      ! formula needs ordinal day of year where Jan 1st=1
      uncorrected_week_of_year=(ordinal-shared_weekday+10)/7
   end function uncorrected_week_of_year

end subroutine d2w
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    w2d(3f) - [M_time:WEEK_OF_YEAR] calculate DAT date-time array from iso-8601
!!    numeric Week values or from string "yyyy-Www-d"
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    either
!!
!!       subroutine w2d(iso_year,iso_week,iso_weekday,dat)
!!
!!        integer,intent(in)      :: iso_year, iso_week, iso_weekday
!!        integer,intent(out)     :: dat(8)     ! output date array
!!
!!    or
!!
!!       subroutine w2d(iso_week,dat,ierr)
!!
!!        character(len=*),intent(in)  :: iso8601_week
!!        integer,intent(out)          :: dat(8)     ! output date array
!!        integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!   Given an ISO-8601 week return a "DAT" array defining a date and time,
!!   The ISO-8601 is supplied as three integer values defining the ISO
!!   year, week of year and weekday.
!!
!!##OPTIONS
!!    iso_year      ISO-8601 year number for the given date
!!    iso_week      ISO-8601 week number for the given date.
!!                  Valid values are from 1 to 53.
!!    iso_weekday   ISO-8601 weekday number for the given date.
!!                  Valid values are from 1 to 7, where 1 is Monday.
!!
!!    iso8601_week  ISO-8601 Week string for the data in the form
!!                  "yyyy-Www-D", "yyyyWwwD", "yyyy-Www", and "yyyyWww"
!!                  where yyyy is the year, ww is the iso_week, and D is
!!                  the weekday.
!!
!!##RETURNS
!!    dat          "DAT" array (an integer array of the same format as
!!                 the array returned by the intrinsic DATE_AND_TIME(3f))
!!                 describing the date to be used
!!
!!                     dat=[ year,month,day,timezone,hour,&
!!                      & minutes,seconds,milliseconds]
!!
!!    ierr         optional error code result. If non-zero an error occurred.
!!                 If an error occurs and IERR is not present the program
!!                 terminates.
!!##NOTES
!!
!!   If D is omitted 1 is returned although this does not appear in the
!!   iso-8601 standard at the current time.
!!
!!   The returned dat array is currently always assumed to have the local
!!   timezone. This might be changed to always assume ZULU time.
!!
!!##EXAMPLE
!!
!!
!!  Sample program:
!!
!!     program demo_w2d
!!     use M_time, only : w2d, fmtdate
!!     implicit none
!!       write(*,'(a)')&
!!       & 'Given Monday 29 December 2008 is written "2009-W01-1"'
!!       call printit(2009,1,1)
!!       write(*,'(a)')&
!!       & 'Given Sunday 3 January 2010 is written "2009-W53-7"'
!!       call printit(2009,53,7)
!!       write(*,'(a)')&
!!       & 'Given the Gregorian date Sun 31 December 2006 &
!!       &is written 2006-W52-7'
!!       call printit(2006,52,7)
!!       write(*,'(a)')&
!!       & 'Given 27 September 2008 is 2008-W39-6'
!!       call printit(2008,39,6)
!!
!!       string : block
!!          character(len=*),parameter :: array(4)=[character(len=80) ::  &
!!          & '2008-W39-6', '2008W396', '2008-W39', '2008W39' ]
!!          integer  :: dat(8)
!!          integer  :: i
!!          do i=1,size(array)
!!             write(*,'(a)')&
!!             & 'Given string '//array(i)
!!             call w2d(array(i),dat)
!!             write(*,'(a,i0)')'RESULT:          '
!!             write(*,'(a,*(i0:,","))')'   DAT array        ',dat
!!             write(*,'(a,/,67("="))')'    '//fmtdate(dat,'long')
!!          enddo
!!       endblock string
!!     contains
!!     subroutine printit(iso_year,iso_week,iso_weekday)
!!     ! ISO-8601 Week: 2016-W29-1
!!     integer  :: iso_year, iso_week, iso_weekday
!!     ! input date array
!!     integer  :: dat(8)
!!      call w2d(iso_year,iso_week,iso_weekday,dat)
!!      write(*,'(a,i0)')'GIVEN:           '
!!      write(*,'(a,i0)')'ISO-8601 year    ',iso_year
!!      write(*,'(a,i0)')'ISO-8601 week    ',iso_week
!!      write(*,'(a,i0)')'ISO-8601 weekday ',iso_weekday
!!      write(*,'(a,i0)')'RESULT:          '
!!      write(*,'(a,*(i0:,","))')'   DAT array        ',dat
!!      write(*,'(a,/,67("="))')'    '//fmtdate(dat,'long')
!!     end subroutine printit
!!    end program demo_w2d
!!
!!  Results:
!!
!!     Given Monday 29 December 2008 is written "2009-W01-1"
!!     GIVEN:
!!     ISO-8601 year    2009
!!     ISO-8601 week    1
!!     ISO-8601 weekday 1
!!     RESULT:
!!        DAT array        2008,12,29,-240,0,0,0,0
!!         Monday, December 29th, 2008 12:00:00 AM UTC-04:00
!!     =========================================================
!!     Given Sunday 3 January 2010 is written "2009-W53-7"
!!     GIVEN:
!!     ISO-8601 year    2009
!!     ISO-8601 week    53
!!     ISO-8601 weekday 7
!!     RESULT:
!!        DAT array        2010,1,3,-240,0,0,0,0
!!         Sunday, January 3rd, 2010 12:00:00 AM UTC-04:00
!!     =========================================================
!!     Given the Gregorian date Sun 31 December 2006 is written 2006-W52-7
!!     GIVEN:
!!     ISO-8601 year    2006
!!     ISO-8601 week    52
!!     ISO-8601 weekday 7
!!     RESULT:
!!        DAT array        2006,12,31,-240,0,0,0,0
!!         Sunday, December 31st, 2006 12:00:00 AM UTC-04:00
!!     =========================================================
!!     Given 27 September 2008 is 2008-W39-6
!!     GIVEN:
!!     ISO-8601 year    2008
!!     ISO-8601 week    39
!!     ISO-8601 weekday 6
!!     RESULT:
!!        DAT array        2008,9,27,-240,0,0,0,0
!!         Saturday, September 27th, 2008 12:00:00 AM UTC-04:00
!!     =========================================================
!!
!!##DEFINITION
!!   The ISO-8601 date and time standard was issued by the International
!!   Organization for Standardization (ISO). It is used (mainly) in
!!   government and business for fiscal years, as well as in timekeeping.
!!   The system specifies a week year atop the Gregorian calendar by
!!   defining a notation for ordinal weeks of the year.
!!
!!   An ISO week-numbering year (also called ISO year informally) has
!!   52 or 53 full weeks. That is 364 or 371 days instead of the usual
!!   365 or 366 days. The extra week is referred to here as a leap week,
!!   although ISO-8601 does not use this term. Weeks start with Monday.
!!   The first week of a year is the week that contains the first Thursday
!!   of the year (and, hence, always contains 4 January). ISO week year
!!   numbering therefore slightly deviates from the Gregorian for some
!!   days close to January 1st.
!!
!!##METHOD
!!     Calculating a date given the year, week number and weekday
!!
!!     This method requires that one know the weekday of 4 January of the
!!     year in question. Add 3 to the number of this weekday, giving a
!!     correction to be used for dates within this year.
!!
!!     Method: Multiply the week number by 7, then add the weekday. From
!!     this sum subtract the correction for the year. The result is the
!!     ordinal date, which can be converted into a calendar date.  If the
!!     ordinal date thus obtained is zero or negative, the date belongs to
!!     the previous calendar year; if greater than the number of days in
!!     the year, to the following year.
!!
!!     Example: year 2008, week 39, Saturday (day 6)
!!     Correction for 2008: 5 + 3 = 8
!!     (39 x 7) + 6 = 279
!!     279 - 8 = 271
!!     Ordinal day 271 of a leap year is day 271 - 244 = 27 September
!!     Result: 27 September 2008
!!
!!##ISO_NAME
!!   Week date representations are in the format YYYY-Www ,YYYYWww,
!!   YYYY-Www-D or YYYYWwwD
!!
!!     o [YYYY] indicates the ISO week-numbering year which is slightly
!!       different from the traditional Gregorian calendar year.
!!     o [Www] is the week number prefixed by the letter W, from W01
!!       through W53.
!!     o [D] is the weekday number, from 1 through 7, beginning with Monday
!!       and ending with Sunday.
!!
!!   For example, the Gregorian date 31 December 2006 corresponds to the
!!   Sunday of the 52nd week of 2006, and is written
!!
!!     2006-W52-7 (extended form)
!!     or
!!     2006W527 (compact form).
!!
!!##REFERENCE
!!    From Wikipedia, the free encyclopedia 2016-08-08
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine w2d_numeric(iso_year,iso_week,iso_weekday,dat)

! ident_19="@(#) M_time w2d_numeric(3f) convert iso-8601 Week-numbering year date yyyy-Www-d to DAT date-time array"

integer,intent(in)  :: iso_year, iso_week, iso_weekday
integer,intent(out) :: dat(8)     ! output date array
integer             :: jan4weekday
integer             :: correction
integer             :: ordinal
integer             :: ierr
integer             :: temp_dat(8)
   temp_dat=[iso_year,1,4,0,12,0,0,0]
   call dow( temp_dat, jan4weekday, ierr=ierr) ! get day of week for January 4th where Sun=1
   correction=jan4weekday+3                      ! calculate correction
   ordinal=iso_week*7+iso_weekday-correction     ! calculate ordinal day
   dat=o2d(ordinal,iso_year)                     ! convert ordinal to DAT (routine works with negative values or days past year end)
end subroutine w2d_numeric
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine w2d_string(iso8601_week,dat,ierr)

! ident_20="@(#) or form yyyy-Www-d to DAT date-time array"

character(len=*),intent(in)   :: iso8601_week
integer                       :: iso_year, iso_week, iso_weekday
integer,intent(out)           :: dat(8)     ! output date array
integer,intent(out),optional  :: ierr
integer                       :: ierr_
integer                       :: returncode
character(len=:), allocatable :: array(:)
character(len=:), allocatable :: stopmessage

! some additional verification with verify() would be in order that of form yyyyWwwd or yyyy-Www-d

   CALL split(iso8601_week, array, delimiters=' wW-')
   ierr_=1                     ! initialize return to indicate error
   stopmessage="<ERROR>*w2d_string*: string not of format yyyy-Www-dd:"//iso8601_week
   dat=-99999

   if(size(array)==2)then       ! assume compact form of yyyyWwwdd where ww is from 01 to 53 and rearrange to three strings
      if(len_trim(array(2)) > 2)then
         array=[character(len=len(array)) :: array(1),array(2)(1:2),array(2)(3:)]
      elseif(len_trim(array(2)) == 2) then
         array=[character(len=len(array)) :: array(1),array(2),'1']
      endif
   endif

   if(size(array)==3)then  ! assume yyyy-Www-d
      ierr_=0

      iso_year=nint(s2v(array(1),returncode))
      ierr_=ierr_+abs(returncode)

      iso_week=nint(s2v(array(2),returncode))
      ierr_=ierr_+abs(returncode)
      if(iso_week < 1 .or. iso_week > 53 ) then
         stopmessage="<ERROR>*w2d_string*: week out of bounds {1-53} :"//iso8601_week
         ierr_=ierr_+abs(returncode)
      endif

      iso_weekday=nint(s2v(array(3),returncode))
      ierr_=ierr_+abs(returncode)
      if(iso_weekday < 1 .or. iso_weekday > 7) then
         stopmessage="<ERROR>*w2d_string*: day of week out of bounds {1-7} :"//iso8601_week
         ierr_=ierr_+1
      endif

   endif
   if(ierr_ == 0)then
      call w2d_numeric(iso_year,iso_week,iso_weekday,dat)
   endif

   if(present(ierr))then
      ierr=ierr_
   elseif(ierr_ /= 0)then
      write(stderr,'(a)') stopmessage
      stop 4
   endif

end subroutine w2d_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    box_month(3f) - [M_time:DATE_PRINTING] create specified month in a
!!    character array
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine box_month(dat,calen)
!!
!!     integer,intent(in) :: dat(8)
!!     character(len=21)  :: calen(8)
!!
!!##DESCRIPTION
!!   box_month(3f) uses a year and month from a date array to populate
!!   a small character array with a calendar representing the month.
!!
!!##OPTIONS
!!    dat  "DAT" array (an integer array of the same format as
!!          the array returned by the intrinsic DATE_AND_TIME(3f))
!!          describing the date to be used to specify what calendar
!!          month to produce.
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!##RETURNS
!!    calen  returned character array holding a display of the
!!           specified month
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_box_month
!!     use M_time, only : box_month
!!     implicit none
!!     integer           :: dat(8)
!!     character(len=21) :: calendar(8)
!!        call date_and_time(values=dat)
!!        call box_month(dat,calendar)
!!        write(*,'(a)')calendar
!!     end program demo_box_month
!!
!!    results:
!!
!!      >     July 2016
!!      >Mo Tu We Th Fr Sa Su
!!      >             1  2  3
!!      > 4  5  6  7  8  9 10
!!      >11 12 13 14 15 16 17
!!      >18 19 20 21 22 23 24
!!      >25 26 27 28 29 30 31
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
subroutine box_month(dat,calen)

! ident_21="@(#) M_time box_month(3f) generate month specified by DAT date-time array in character array"

integer,parameter    :: wklen=3*7
!-----------------------------------------------------------------------------------------------------------------------------------
! uses year and month from date array DAT to populate a small character array with a calendar representing the month
integer,intent(in)   :: dat(8)
character(len=wklen) :: calen(8)
!-----------------------------------------------------------------------------------------------------------------------------------
real(kind=realtime)  :: julian
integer              :: weekday
integer              :: dat_1st(8)
integer              :: dat_nextday(8)
integer              :: location,ierr,i
!-----------------------------------------------------------------------------------------------------------------------------------
   calen(:)='                    '                                 ! initialize output array to spaces
   dat_1st=[dat(1),dat(2),1,dat(4),0,0,0,0]                        ! create date array for first day in month specified
   call dow(dat_1st, weekday, ierr=ierr)                           ! return the day of the week for first of month
!-----------------------------------------------------------------------------------------------------------------------------------
   calen(1)=adjustc(v2mo(dat(2))//' '//v2s(dat(1)),len(calen(1)))  ! build first line with month and year centered
   calen(2)='Mo Tu We Th Fr Sa Su'                                 ! build second line with days of week
!-----------------------------------------------------------------------------------------------------------------------------------
   location=1+((weekday-1)*3)                                      ! if data were one row where would 3-character day value start?
   call date_to_julian(dat_1st,julian,ierr)                        ! get Julian Date for 1st day of month
   MNTH: do i=1,31                                                 ! put dates into rest of array starting at third line
      write(calen(location/wklen+3)(mod(location,wklen):),'(i2)')i
      if(i>=28)then                                              ! is tomorrow in another month?
         call julian_to_date(julian+i,dat_nextday,ierr)
         if(dat_nextday(2)/=dat(2))then
            exit MNTH
         endif
      endif
      location=location+3
   enddo MNTH
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine box_month
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2j(3f) - [M_time:JULIAN] given DAT date-time array returns Julian Date
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function d2j(dat) result(julian)
!!
!!     integer,intent(in)  :: dat(8)
!!     real(kind=realtime) :: julian
!!
!!##DESCRIPTION
!!   Given DAT date-time array returns Julian Date
!!
!!##OPTIONS
!!    dat       Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!              If not present, use current time.
!!##RETURNS
!!    julian    The Julian Date. Julian dates (abbreviated JD)
!!              are simply a continuous count of days and fractions since
!!              noon Universal Time on January 1, 4713 BC (on the Julian
!!              calendar).
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2j
!!     use M_time, only : d2j
!!     implicit none
!!     integer :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        write(*,*)'Julian Date is ',d2j(dat)
!!     end program demo_d2j
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:2:11:50:885
!!     Julian Date is    2457588.7582278359
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function d2j(dat) result(julian)

! ident_22="@(#) M_time d2j(3f) Given DAT date-time array returns Julian Date"

integer,intent(in),optional :: dat(8)
real(kind=realtime)         :: julian
integer                     :: ierr
integer                     :: dat_local(8)

   if(present(dat))then                      ! if dat array is present use value contained in it
      call date_to_julian(dat,julian,ierr)
   else                                      ! if dat array is not present create one containing current time
      dat_local=getnow()
      call date_to_julian(dat_local,julian,ierr)
   endif

end function d2j
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    j2d(3f) - [M_time:JULIAN] given a JD (Julian Date) returns a
!!    date-time array DAT.
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function j2d(julian) result(dat)
!!
!!     real(kind=realtime),intent(in),optional :: julian
!!     integer                                 :: dat(8)
!!
!!##DESCRIPTION
!!   Converts a Julian Date to a DAT date-time array.
!!
!!##OPTIONS
!!    julian  A Julian Date (JD) is the number of days since
!!            noon (not midnight) on January 1st, 4713 BC.
!!            If not present, use current time.
!!
!!##RETURNS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_j2d
!!     use M_time, only : j2d, d2j, fmtdate, realtime
!!     implicit none
!!     integer,parameter   :: dp=kind(0.0d0)
!!     real(kind=realtime) :: today
!!     integer             :: dat(8)
!!        call date_and_time(values=dat) ! get the date using intrinsic
!!        today=d2j(dat)                  ! convert today to Julian Date
!!        write(*,*)'Today=',fmtdate(j2d(today))
!!        ! math is easy with Julian Days and Julian Dates
!!        write(*,*)'Yesterday=',fmtdate(j2d(today-1.0_dp))
!!        write(*,*)'Tomorrow=',fmtdate(j2d(today+1.0_dp))
!!     end program demo_j2d
!!
!!    results:
!!
!!     Today=Tuesday, July 19th, 2016 08:48:20 AM
!!     Yesterday=Monday, July 18th, 2016 08:48:20 AM
!!     Tomorrow=Wednesday, July 20th, 2016 08:48:20 AM
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function j2d(julian) result(dat)

! ident_23="@(#) M_time j2d(3f) Given Julian Date returns DAT date-time array"

real(kind=realtime),intent(in)   :: julian
integer                          :: dat(8)
integer                          :: ierr
   call julian_to_date(julian,dat,ierr)
end function j2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    d2u(3f) - [M_time:UNIX_EPOCH] given DAT date-time array returns Unix
!!    Epoch Time (UET starts at 0000 on 1 Jan. 1970, UTC)
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function d2u(dat) result(unixtime)
!!
!!       integer,intent(in),optional :: dat(8)
!!       real(kind=realtime)         :: unixtime
!!
!!##DESCRIPTION
!!   Converts a DAT date-time array to a Unix Epoch Time value. Typically
!!   mathematical operations such as sums, sorting and comparison are
!!   performed with simple UET numeric values, and then they are converted
!!   back.
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!          If not present the current time is used
!!
!!##RETURNS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since 00:00:00 on
!!              January 1st, 1970, UTC.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_d2u
!!     use M_time, only : d2u
!!     implicit none
!!     integer           :: dat(8)
!!        call date_and_time(values=dat)
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        write(*,*)'Unix Epoch time is ',d2u(dat)
!!     end program demo_d2u
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:2:0:48:561
!!     Unix Epoch time is    1468908048.5610321
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function d2u(dat) result(unixtime)

! ident_24="@(#) M_time d2u(3f) Given DAT date-time array returns Unix Epoch time"

real(kind=realtime)           :: unixtime
integer,intent(in),optional   :: dat(8)
   integer                    :: datlocal(8)
   integer                    :: ierr
   if(present(dat))then
      datlocal=dat
   else
      datlocal=getnow() ! current time is placed in array
   endif
   call date_to_unix(datlocal,unixtime,ierr)
end function d2u
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    u2d(3f) - [M_time:UNIX_EPOCH] given Unix Epoch Time returns DAT
!!    date-time array
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function u2d(unixtime) result(dat)
!!
!!     class(*),intent(in),optional      :: unixtime
!!     ! integer
!!     ! real
!!     ! real(kind=realtime)
!!
!!     integer                           :: dat(8)
!!
!!##DESCRIPTION
!!   Given Unix Epoch Time returns DAT date-time array
!!
!!##OPTIONS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since
!!              00:00:00 on January 1st, 1970, UTC. If not present, use
!!              current time.
!!
!!##RETURNS
!!    dat       Integer array holding a "DAT" array, similar in structure
!!              to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!                 dat=[ year,month,day,timezone,hour,&
!!                  & minutes,seconds,milliseconds]
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_u2d
!!     use M_time, only : u2d, d2u, fmtdate, realtime
!!     implicit none
!!     integer,parameter :: dp=kind(0.0d0)
!!     real(kind=realtime) :: today
!!     integer :: dat(8)
!!        ! get the date using intrinsic
!!        call date_and_time(values=dat)
!!        ! convert today to Julian Date
!!        today=d2u(dat)
!!        write(*,*)'Today=',fmtdate(u2d(today))
!!        ! subtract day
!!        write(*,*)'Yesterday=',fmtdate(u2d(today-86400.0_dp))
!!        ! add day
!!        write(*,*)'Tomorrow=',fmtdate(u2d(today+86400.0_dp))
!!     end program demo_u2d
!!
!!    results:
!!
!!     Today=Tuesday, July 19th, 2016 11:10:08 AM
!!     Yesterday=Monday, July 18th, 2016 11:10:08 AM
!!     Tomorrow=Wednesday, July 20th, 2016 11:10:08 AM
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function u2d(unixtime) result(dat)

! ident_25="@(#) M_time u2d(3f) Given Unix Epoch Time returns DAT date-time array"

class(*),intent(in),optional   :: unixtime
integer                        :: dat(8)
real(kind=realtime)            :: local_unixtime
integer                        :: ierr

   if(present(unixtime))then
      select type(unixtime)
      type is (integer);             local_unixtime=unixtime
      type is (integer(kind=int64)); local_unixtime=unixtime
      type is (real);                local_unixtime=unixtime
      type is (real(kind=realtime)); local_unixtime=unixtime
      end select
      call unix_to_date(local_unixtime,dat,ierr)
   else
      dat=getnow() ! current time is placed in array
   endif

end function u2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function get_timezone() result(tz)
integer :: tz
integer :: timezone(8)
   timezone=getnow()
   tz=timezone(4)
   if(tz>0)then  ! gfortran bug on new-years
      write(stderr,gen)'<ERROR>*get_timezone*: TZ=',tz
      tz=mod(tz,1440)-1440
   endif
end function get_timezone
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sec2days(3f) - [M_time:DURATION] convert seconds to string of form
!!    dd-hh:mm:ss
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function sec2days(seconds,crop) result(dhms)
!!
!!     real(kind=realtime),intent(in) :: seconds
!!       or
!!     integer,intent(in)             :: seconds
!!       or
!!     real,intent(in)                :: seconds
!!       or
!!     character(len=*)               :: seconds
!!
!!     logical,intent(in),optional    :: crop
!!     character(len=:),allocatable   :: dhms
!!
!!##DESCRIPTION
!!   Given a number of seconds convert it to a string of the form
!!
!!       dd-hh:mm:ss
!!
!!   where dd is days, hh hours, mm minutes and ss seconds.
!!
!!##OPTIONS
!!    seconds    number of seconds to convert to string of form dd-hh:mm:ss. May
!!               be of type INTEGER, REAL, REAL(KIND=REALTIME), or CHARACTER.
!!
!!               CHARACTER input strings may be of the form
!!               [NNd][NNh][NNm][NNs][NNw]. Case, spaces and underscores are
!!               ignored. Allowed aliases for d,h,m, and s units are
!!
!!                   d -  days,day
!!                   m -  minutes,minute,min
!!                   h -  hours,hour,hrs,hr
!!                   s -  seconds,second,sec
!!
!!               The numeric values may represent floating point numbers.
!!
!!               " is converted to "s", and ' is converted to "m" to support
!!               the closest ANSI representation to the "prime" character,
!!               as in 3h10'20".
!!
!!    crop       if .true., remove leading zero day values or day and hour values.
!!               Optional, defaults to .false. .
!!##RETURNS
!!    dmhs       the returned string of form [d:h:]m:s
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_sec2days
!!     use M_time, only : sec2days
!!     implicit none
!!     integer,parameter :: dp=kind(0.0d0)
!!        write(*,*)sec2days(129860)
!!        write(*,*)sec2days(80000.0_dp)
!!        write(*,*)sec2days(80000.0,crop=.true.)
!!        write(*,*)sec2days('1 day 2.0hr 100 min 300.0seconds')
!!     end program demo_sec2days
!!
!!    results:
!!
!!     1-12:04:20
!!     0-22:13:20
!!     22:13:20
!!     1-03:45:00
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function sec2days(seconds,crop) result(dhms)
use, intrinsic :: iso_fortran_env, only : int64

! ident_26="@(#) M_time sec2days(3f) converts seconds or string of form IId JJh KKm LLs to string showing days of form D-HH MM SS"

! on this platform, (select_int_kind(i),i=1,100) returns
! 1:2=1 ,3:4=2 ,5:9=4 ,10:18= 8 ,19:38=16 ,39:=-1
!integer,parameter              :: k(38)=[(selected_int_kind(i),i=1,38)]
integer                        :: i
class(*),intent(in)            :: seconds
logical,intent(in),optional    :: crop
character(len=:),allocatable   :: dhms
real(kind=realtime), parameter :: units_hl(4)=[ 86400.0_dp, 3600.0_dp, 60.0_dp, 1.0_dp ]
character(len=40)              :: scratch
integer(kind=int64)            :: days, hours, minutes, secsleft
integer,parameter              :: one_day=86400
integer,parameter              :: one_hour=3600
integer,parameter              :: one_minute=60
logical                        :: crop_local
integer                        :: iprint
logical                        :: negative
integer                        :: ilast
character(len=:),allocatable   :: strlocal
character(len=:),allocatable   :: array(:)
doubleprecision                :: dtime

   !  Convert input value to nearest integer
   !  Notice that the value SECONDS can be any of several types ( INTEGER,REAL,REAL(KIND=REALTIME))
   select type(seconds)
   type is (integer);               secsleft=seconds
   type is (real);                  secsleft=nint(seconds)
   type is (real(kind=realtime));   secsleft=nint(seconds)
   type is (character(len=*))

      !remove spaces, commas, and underscores
      ! note _ is removed from input strings to allow use of _ every three digits in a number as sometimes seen in Java, perl, ...
      strlocal=compact(seconds,'')                                     ! remove whitespace
      strlocal=lower(transliterate(strlocal," _,",''))//repeat(' ',16) ! lowercase and append whitespace to make room for spaces

      call substitute(strlocal,'days','d')                     ! from long names to short names substitute common aliases for units
      call substitute(strlocal,'day','d')
      call substitute(strlocal,'hours','h')
      call substitute(strlocal,'hour','h')
      call substitute(strlocal,'hrs','h')
      call substitute(strlocal,'hr','h')
      call substitute(strlocal,'minutes','m')
      call substitute(strlocal,'minute','m')
      call substitute(strlocal,'min','m')
      call substitute(strlocal,"'",'m')
      call substitute(strlocal,'seconds','s')
      call substitute(strlocal,'second','s')
      call substitute(strlocal,'secs','s')
      call substitute(strlocal,'sec','s')
      call substitute(strlocal,'"','s')
      call substitute(strlocal,'weeks','w')
      call substitute(strlocal,'week','w')
      call substitute(strlocal,'wks','w')
      call substitute(strlocal,'wk','w')
      !do i=2,len_trim(strlocal)
      ! maybe filter out other characters obviously not part of values?
      ! if a letter not in smhdw remove but leave numeric values alone. Allow sign and e?
      ! or parse
      !enddo
      call substitute(strlocal,'s','s ')          ! assuming only one suffix character and not too many to exceed length of strlocal
      call substitute(strlocal,'m','m ')
      call substitute(strlocal,'h','h ')
      call substitute(strlocal,'d','d ')
      call substitute(strlocal,'w','w ')

      dtime=0.0_dp
      call split(strlocal,array,' ')

      do i=1,size(array)
         ilast=len_trim(array(i))
         select case(array(i)(ilast:ilast))
         case('w'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(1)*7
         case('d'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(1)
         case('h'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(2)
         case('m'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(3)
         case('s'); dtime=dtime+s2v(array(i)(:ilast-1))*units_hl(4)
         case default
            dtime=dtime+s2v(array(i))
         end select
      enddo
      secsleft=int(dtime,kind=int64)
   end select

   if(present(crop))then    ! whether to trim cases where(days=0) and (hours=0 when days=0) from output or always show dd-hh:mm:ss
      crop_local=crop
   else
      crop_local=.false.
   endif

   if(secsleft<0)then
      secsleft=-secsleft
      negative=.true.
   else
      negative=.false.
   endif

   iprint=4

   days=secsleft/one_day                  ! get whole number of days
   if(days==0) iprint=3
   secsleft=secsleft-days*one_day         ! calculate remainder

   hours=secsleft/one_hour                ! get whole number of hours
   if(days==0.and.hours==0) iprint=2
   secsleft=secsleft-hours*one_hour

   minutes=secsleft/one_minute            ! get whole number of minutes
   secsleft=secsleft-minutes*one_minute

   if(.not.crop_local)then
      iprint=4
   endif

   select case(iprint)                    ! select format if cropping is on and leading zero values are present
   case(2)
      write(scratch,'(i2.2,":",i2.2)')minutes,secsleft
   case(3)
      write(scratch,'(i2.2,":",i2.2,":",i2.2)')hours,minutes,secsleft
   case default
      write(scratch,'(i0,"-",i2.2,":",i2.2,":",i2.2)')days,hours,minutes,secsleft
   end select

   if(negative)then
      dhms='-'//trim(scratch)
   else
      dhms=trim(scratch)
   endif

end function sec2days
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    days2sec(3f) - [M_time:DURATION] convert string of form
!!    [[-]dd-]hh:mm:ss.nn to seconds
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    elemental impure function days2sec(str) result(time)
!!
!!     character(len=*),intent(in) :: str
!!     real(kind=realtime)         :: time
!!
!!##DESCRIPTION
!!   Given a string representing a duration of the form
!!   [-][[[dd-]hh:]mm:]ss or [NNd][NNh][NNm[]NNs][NNw]
!!   return a value representing seconds.
!!
!!   If "dd-" is present, units for the numbers are assumed to
!!   proceed from day to hour to minute to second. But if no
!!   day is present, the units are assumed to proceed from second
!!   to minutes to hour from left to right. That is ...
!!
!!         [-]dd-hh:mm:ss
!!         [-]dd-hh:mm
!!         [-]dd-hh
!!
!!         hh:mm:ss
!!         mm:ss
!!         ss
!!
!!         Where dd is days, hh hours, mm minutes and ss seconds.
!!
!!   A decimal fraction is supported on the seconds (Actually,
!!   any of the numeric values may represent positive floating
!!   point numbers). Spaces are ignored.
!!
!!   Simple numeric values may also be used with unit suffixes; where
!!   s,m,h, or d represents seconds, minutes, hours or days and w
!!   represents a week. Allowed aliases for w,d,h,m, and s units are
!!
!!        [NNd][NNh][NNm][NNs][NNw]
!!
!!          d -  days,day
!!          m -  minutes,minute,min,mins
!!          h -  hours,hour,hr,hrs
!!          s -  seconds,second,sec,secs
!!          w -  week, weeks, wk, wks
!!
!!   The numeric values may represent floating point numbers.
!!
!!   Spaces, commas and case are ignored.
!!
!!   " is converted to "s", and ' is converted to "m" to support
!!   the closest ANSI representation to the "prime" character,
!!   as in 3h10'20".
!!
!!##OPTIONS
!!       str   string of the general form dd-hh:mm:ss.nn
!!##RETURNS
!!       time  the number of seconds represented by the input string
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_days2sec
!!     use M_time, only : days2sec
!!     implicit none
!!        write(*,*)days2sec('1-12:04:20')
!!        write(*,*)'one second ',days2sec('1')
!!        write(*,*)'one minute ',days2sec('1:00')
!!        write(*,*)'one hour ',days2sec('1:00:00')
!!        write(*,*)'one day ',days2sec('1-00:00:00')
!!        write(*,*)nint(days2sec(' 1-12:04:20              ')) == 129860
!!        write(*,*)nint(days2sec(' 1.5 days                ')) == 129600
!!        write(*,*)nint(days2sec(' 1.5 days 4hrs 30minutes ')) == 145800
!!        write(*,*)nint(days2sec(' 1.5d                    ')) == 129600
!!        write(*,*)nint(days2sec(' 1d2h3m4s                ')) == 93784
!!        ! duplicates
!!        write(*,*)nint(days2sec(' 1d1d1d                  ')) == 259200
!!        ! negative values
!!        write(*,*)nint(days2sec(' 4d-12h                  ')) == 302400
!!     end program demo_days2sec
!!
!!    Results:
!!
!!     > 129860.00000000000
!!     > one second    1.0000000000000000
!!     > one minute    60.000000000000000
!!     > one hour    3600.0000000000000
!!     > one day    86400.000000000000
!!     > T
!!     > T
!!     > T
!!     > T
!!     > T
!!     > T
!!     > T
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
elemental impure function days2sec(str) result(time)

! ident_27="@(#) M_time days2sec(3f) convert string [[-]dd-]hh mm ss.nn to seconds or string IId JJh KKm LLs to seconds"

character(len=*),intent(in)    :: str
real(kind=realtime)            :: time
! Supported input syntax:
!    [-]dd-hh:mm:ss
!          hh:mm:ss
!          mm:ss
!          ss
!
character(len=:),allocatable   :: strlocal
character(len=:),allocatable   :: array(:)
real(kind=realtime), parameter :: units_lh(4)=[ 1.0_dp, 60.0_dp, 3600.0_dp, 86400.0_dp ]
real(kind=realtime), parameter :: units_hl(4)=[ 86400.0_dp, 3600.0_dp, 60.0_dp, 1.0_dp ]
integer                        :: i, icount, iwords, ilast
logical                        :: negative

   time=0.0_dp
   strlocal=compact(str,'')                            ! remove whitespace
   strlocal=transliterate(strlocal,"_,",'')            ! remove commas, and underscores sometimes used in numbers in Java, perl, ...
   strlocal=lower(strlocal)//repeat(' ',len(strlocal)) ! change to lowercase and add whitespace to make room for spaces

   if(len(strlocal)==0)then
      time=0.0_dp
   elseif(scan(strlocal,'smhdw')/=0)then               ! unit code values not DD-HH:MM:SS either plain number or unit numbers
      call substitute(strlocal,'days','d')             ! from long names to short names substitute common aliases for units
      call substitute(strlocal,'day','d')
      call substitute(strlocal,'hours','h')
      call substitute(strlocal,'hour','h')
      call substitute(strlocal,'hrs','h')
      call substitute(strlocal,'hr','h')
      call substitute(strlocal,'minutes','m')
      call substitute(strlocal,'minute','m')
      call substitute(strlocal,'mins','m')
      call substitute(strlocal,'min','m')
      call substitute(strlocal,"'",'m')
      call substitute(strlocal,'seconds','s')
      call substitute(strlocal,'second','s')
      call substitute(strlocal,'secs','s')
      call substitute(strlocal,'sec','s')
      call substitute(strlocal,'"','s')
      call substitute(strlocal,'weeks','w')
      call substitute(strlocal,'week','w')
      call substitute(strlocal,'wks','w')
      call substitute(strlocal,'wk','w')

      call substitute(strlocal,'s','s ')          ! assuming only one suffix character and not too many to exceed length of strlocal
      call substitute(strlocal,'m','m ')
      call substitute(strlocal,'h','h ')
      call substitute(strlocal,'d','d ')
      call substitute(strlocal,'w','w ')
      call split(strlocal,array,' ')
      iwords=size(array)
      icount=0
      do i=iwords,1,-1
         icount=icount+1
         ilast=len_trim(array(i))
         select case(array(i)(ilast:ilast))
         case('w'); time=time+s2v(array(i)(:ilast-1))*units_hl(1)*7
         case('d'); time=time+s2v(array(i)(:ilast-1))*units_hl(1)
         case('h'); time=time+s2v(array(i)(:ilast-1))*units_hl(2)
         case('m'); time=time+s2v(array(i)(:ilast-1))*units_hl(3)
         case('s'); time=time+s2v(array(i)(:ilast-1))*units_hl(4)
         case default
            time=time+s2v(array(i))
         end select
      enddo
   else

      if(strlocal(1:1)=='-')then          ! allow negative prefix as first character but remove it and change sign of value at end
         negative=.true.
         strlocal(1:1)=' '
      else
         negative=.false.
      endif

      call split(trim(strlocal),array,' -:')
      iwords=size(array)

      if(iwords>4)then
         write(stderr,gen)'<ERROR>*days2sec*: too many values in',trim(strlocal)
         iwords=4
      endif

      if(index(strlocal,'-')>0)then                ! found a dash, assume has days and form DD-HH:MM:SS, DD-, DD-HH, DD-HH:MM
         do i=1,iwords
            time=time+s2v(array(i))*units_hl(i)
         enddo
      else                                            ! no dash, assume no days, either HH:MM:SS or MM:SS, SS
         icount=0
         do i=iwords,1,-1
            icount=icount+1
            ilast=len_trim(array(i))
            time=time+s2v(array(i))*units_lh(icount)
         enddo
      endif

      if(negative)time=-time

   endif

end function days2sec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! locale(3f) - [M_time:DATE_PRINTING] allow for selecting languages to represent
!!              month and weekday names
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine locale(name,month_names,weekday_names, &
!!    & month_names_abbr,weekday_names_abbr,IERR)
!!
!!     character(len=*),intent(in)           :: name
!!     character(len=*),intent(in),optional  :: month_names(12)
!!     character(len=*),intent(in),optional  :: month_names_abbr(12)
!!     character(len=*),intent(in),optional  :: weekday_names(7)
!!     character(len=*),intent(in),optional  :: weekday_names_abbr(7)
!!     integer,intent(out)                   :: ierr
!!
!!##DESCRIPTION
!!   given a pre-defined locale name or strings to substitute for month names
!!   and weekday names provide some basic support for non-POSIX labels in
!!   date representation.
!!
!!   The parameters are default character types and so may be limited to the
!!   basic ASCII character set, but are typically limited to the extended
!!   ASCII set.
!!
!!   This is only a basic attempt to support internationalization and
!!   currently just supports basic substitution of the default POSIX names
!!   with the alternate strings. As support for UTF-8 grows among Fortran
!!   compilers something more robust will hopefully emerge to provide full
!!   internationalization of the date representations.
!!
!!##OPTIONS
!!   name   predefined name or reserved name "user"
!!
!!   month_names        12 month names
!!   weekday_names       7 weekday names
!!   month_names_abbr   12 month name abbreviations
!!   weekday_names_abbr  7 weekday name abbreviations
!!
!!   ierr               if non-zero an error occurred
!!
!! The NAME parameter may be a pre-defined name or the special name "user".
!! The current pre-defined names are
!!
!!    'bokmal','catalan','czech','dansk'/'danish','deutsch'/'german','dutch',
!!    'eesti'/'estonian','english','finnish','french','galego'/'galician',
!!    'hrvatski'/'croation','hungarian','icelandic','italian','korean',
!!    'lithuanian','norwegian','nynorsk','polish','portuguese','romanian',
!!    'slovak','slovene'/'slovenian','spanish','swedish','turkish'
!!
!! These non-ISO-8859 character sets are defined in terms of ISO-8859 but will
!! not work on most platforms
!!
!!    'greek', 'russian','thai', 'hebrew','japanese'
!!
!! The remaining reserved names take special actions
!!
!!    o POSIX            load POSIX names
!!    o LANGUAGE         use value of environment variable LANGUAGE
!!    o user             placeholder indicating to expect at least one of the
!!                       optional values to be set
!!    o reset,ISO-8601   reset back to initial defaults
!!
!!    o show    print user-defined values to stdout
!!    o chars   dump characters from chars([(i,i=0,255)])
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_locale
!!     use M_time, only : locale, now
!!     implicit none
!!        call locale('POSIX')
!!        write(*,*)now()
!!        call locale('french')
!!        write(*,*)now()
!!        call mine()
!!        write(*,*)now()
!!     contains
!!     subroutine mine()
!!     character(len=*),parameter :: months(12)=[ character(len=9) :: &
!!     &'JANUARY','FEBRUARY','MARCH    ','APRIL  ','MAY     ','JUNE    ', &
!!     &'JULY   ','AUGUST  ','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER']
!!     character(len=*),parameter :: weekdays(7)=[character(len=9) :: &
!!     &'MONDAY','TUESDAY','WEDNESDAY','THURSDAY','FRIDAY','SATURDAY','SUNDAY']
!!     character(len=3),parameter :: short_months(12)=months(:)(1:3)
!!     character(len=3),parameter :: short_weekdays(7)=weekdays(:)(1:3)
!!     integer :: ierr
!!       call locale('user',months,short_months,weekdays,short_weekdays,ierr)
!!     end subroutine mine
!!     end program demo_locale
!!
!!    Results:
!!
!!     Sunday, September 29th, 2024 7:55:00 PM UTC-04:00
!!     dimanche, septembre 29th, 2024 7:55:00 PM UTC-04:00
!!     JUL, SEPTEMBER 29th, 2024 7:55:00 PM UTC-04:00
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
subroutine locale(name,month_names,weekday_names,month_names_abbr,weekday_names_abbr,IERR)
character(len=*),intent(in)           :: name
character(len=*),intent(in),optional  :: month_names(12)
character(len=*),intent(in),optional  :: month_names_abbr(12)
character(len=*),intent(in),optional  :: weekday_names(7)
character(len=*),intent(in),optional  :: weekday_names_abbr(7)
integer,intent(out),optional          :: ierr
integer                               :: i
character(len=:),allocatable          :: name_
   name_=lower(name)
   if(name_.eq.'language')then
      name_=lower(get_env('LANGUAGE',''))
   endif
   select case(name_)
   case('posix','english','en_us');      call  locale_POSIX()
   case('deutsch','german','de_de');     call  locale_deutsch()
   case('slovak');               call  locale_slovak()
   case('czech');                call  locale_czech()
   case('spanish');              call  locale_spanish()
   case('slovene','slovenian');  call  locale_slovene()
   case('dansk','danish');       call  locale_dansk()
   case('galego','galician');    call  locale_galego()
   case('eesti','estonian');     call  locale_eesti()
   case('hrvatski','croation');  call  locale_hrvatski()
   case('dutch');                call  locale_dutch()
   case('finnish');              call  locale_finnish()
   case('icelandic');            call  locale_icelandic()
   case('hungarian');            call  locale_hungarian()
   case('swedish');              call  locale_swedish()
   case('korean');               call  locale_korean()
   case('nynorsk');              call  locale_nynorsk()
   case('turkish');              call  locale_turkish()
   case('romanian');             call  locale_romanian()
   case('portuguese');           call  locale_portuguese()
   case('polish');               call  locale_polish()
   case('lithuanian');           call  locale_lithuanian()
   case('catalan');              call  locale_catalan()
   case('italian');              call  locale_italian()
   case('french','fr_fr');               call  locale_french()
   case('bokmal');               call  locale_bokmal()
   case('norwegian');            call  locale_norwegian()

   case('greek');                call  locale_greek()
   case('russian');              call  locale_russian()
   case('thai');                 call  locale_thai()
   case('hebrew');               call  locale_hebrew()
   case('japanese');             call  locale_japanese()

   case('iso-8601')
      M_time_month_names=G_month_names
      M_time_month_names_abbr=G_month_names_abbr
      M_time_weekday_names=G_weekday_names
      M_time_weekday_names_abbr=G_weekday_names_abbr
   case('reset')
      if(allocated( M_time_month_names))        deallocate(M_time_month_names)
      if(allocated( M_time_month_names_abbr))   deallocate(M_time_month_names_abbr)
      if(allocated( M_time_weekday_names))      deallocate(M_time_weekday_names)
      if(allocated( M_time_weekday_names_abbr)) deallocate(M_time_weekday_names_abbr)
   case('user','')
   case('chars')
      do i=0,255
         write(stdout,gen)i,char(i)
      enddo
   case('show')
      call printit('Month Names',               M_time_month_names )
      call printit('Month Names abbreviated',   M_time_month_names_abbr )
      call printit('Weekday Names',             M_time_weekday_names )
      call printit('Weekday Names abbreviated', M_time_weekday_names_abbr )
   end select

   if(present( month_names        )) M_time_month_names        = month_names
   if(present( month_names_abbr   )) M_time_month_names_abbr   = month_names_abbr
   if(present( weekday_names      )) M_time_weekday_names      = weekday_names
   if(present( weekday_names_abbr )) M_time_weekday_names_abbr = weekday_names_abbr

   if(present(ierr))  then
      ierr=0
   endif
contains

subroutine printit(header,strs)
character(len=*),parameter              :: fmt="(*('""',g0,'""':,','))"
character(len=*),intent(in)             :: header
character(len=:),allocatable,intent(in) :: strs(:)
integer                                 :: i
   if(allocated(strs))then
      write(stdout,fmt)header,(trim(strs(i)),i=1,size(strs))
   else
      select case(header)
      case('Month Names')
         if(allocated(M_time_month_names))then
            write(stdout,fmt) header,(trim(M_time_month_names(i)),i=1,size(M_time_month_names))
         else
            write(stdout,fmt) header,(trim(G_month_names(i)),i=1,size(G_month_names)),"POSIX"
         endif
      case('Month Names abbreviated')
         if(allocated(M_time_month_names_abbr))then
            write(stdout,fmt) header,(trim(M_time_month_names_abbr(i)),i=1,size(M_time_month_names_abbr))
         else
            write(stdout,fmt) header,(trim(G_month_names_abbr(i)),i=1,size(G_month_names_abbr)),"POSIX"
         endif
      case('Weekday Names')
         if(allocated(M_time_weekday_names))then
            write(stdout,fmt) header,(trim(M_time_weekday_names(i)),i=1,size(M_time_weekday_names))
         else
            write(stdout,fmt) header,(trim(G_weekday_names(i)),i=1,size(G_weekday_names)),"POSIX"
         endif
      case('Weekday Names abbreviated')
         if(allocated(M_time_weekday_names_abbr))then
            write(stdout,fmt) header,(trim(M_time_weekday_names_abbr(i)),i=1,size(M_time_weekday_names_abbr))
         else
            write(stdout,fmt) header,(trim(G_weekday_names_abbr(i)),i=1,size(G_weekday_names_abbr)),"POSIX"
         endif
      end select
   endif
end subroutine printit

end subroutine locale
!!----------------------------------------------------------------------------------------------------------------------------------
!include "locale.ffinc"
subroutine locale_deutsch()
! LANG=deutsch
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "Mo","Di","Mi","Do","Fr","Sa","So"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "Jan","Feb","Mrz","Apr","Mai","Jun", &
& "Jul","Aug","Sep","Okt","Nov","Dez" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "Januar","Februar","März","April","Mai","Juni", &
& "Juli","August","September","Oktober","November","Dezember" ]
 ! ASCII weekdays
months(3)(2:2)=char(228)
 ! ASCII weekdays_abbr
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_deutsch


subroutine locale_slovak()
! LANG=slovak
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "po","ut","st","¹t","pi","so","ne"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "pondelok","utorok","streda","¹tvrtok","piatok","sobota","nedeµa"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan","feb","mar","apr","máj","jún", &
& "júl","aug","sep","okt","nov","dec" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "január","február","marec","apríl","máj","jún", &
& "júl","august","september","október","november","december" ]
weekdays(4)(1:1)=char(185)
weekdays(7)(5:5)=char(181)
months(1)(5:5)=char(225)
months(2)(6:6)=char(225)
months(4)(4:4)=char(237)
months(5)(2:2)=char(225)
months(6)(2:2)=char(250)
months(7)(2:2)=char(250)
months(10)(4:4)=char(243)
weekdays_abbr(4)(1:1)=char(185)
months_abbr(5)(2:2)=char(225)
months_abbr(6)(2:2)=char(250)
months_abbr(7)(2:2)=char(250)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_slovak


subroutine locale_czech()
! LANG=czech
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "po","út","st","èt","pá","so","ne"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "pondìlí","úterý","støeda","ètvrtek","pátek","sobota","nedìle"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "led","úno","bøe","dub","kvì","èvn", &
& "èvc","srp","záø","øíj","lis","pro" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "leden","únor","bøezen","duben","kvìten","èerven", &
& "èervenec","srpen","záøí","øíjen","listopad","prosinec" ]
weekdays(1)(5:5)=char(236)
weekdays(1)(7:7)=char(237)
weekdays(2)(1:1)=char(250)
weekdays(2)(5:5)=char(253)
weekdays(3)(3:3)=char(248)
weekdays(4)(1:1)=char(232)
weekdays(5)(2:2)=char(225)
weekdays(7)(4:4)=char(236)
months(2)(1:1)=char(250)
months(3)(2:2)=char(248)
months(5)(3:3)=char(236)
months(6)(1:1)=char(232)
months(7)(1:1)=char(232)
months(9)(2:2)=char(225)
months(9)(3:3)=char(248)
months(9)(4:4)=char(237)
months(10)(1:1)=char(248)
months(10)(2:2)=char(237)
weekdays_abbr(2)(1:1)=char(250)
weekdays_abbr(4)(1:1)=char(232)
weekdays_abbr(5)(2:2)=char(225)
months_abbr(2)(1:1)=char(250)
months_abbr(3)(2:2)=char(248)
months_abbr(5)(3:3)=char(236)
months_abbr(6)(1:1)=char(232)
months_abbr(7)(1:1)=char(232)
months_abbr(9)(2:2)=char(225)
months_abbr(9)(3:3)=char(248)
months_abbr(10)(1:1)=char(248)
months_abbr(10)(2:2)=char(237)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_czech

subroutine locale_catalan()
! LANG=catalan
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "dl.","dt.","dc.","dj.","dv.","ds.","dg."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "dilluns","dimarts","dimecres","dijous","divendres","dissabte","diumenge"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "gen.","febr.","març","abr.","maig","juny", &
& "jul.","ag.","set.","oct.","nov.","des." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "gener","febrer","març","abril","maig","juny", &
& "juliol","agost","setembre","octubre","novembre","desembre" ]
 ! ASCII weekdays
months(3)(4:4)=char(231)
 ! ASCII weekdays_abbr
months_abbr(3)(4:4)=char(231)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_catalan


subroutine locale_spanish()
! LANG=spanish
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "lu.","ma.","mi.","ju.","vi.","sá.","do."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "lunes","martes","miércoles","jueves","viernes","sábado","domingo"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "ene.","feb.","mar.","abr.","may.","jun.", &
& "jul.","ago.","sep.","oct.","nov.","dic." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "enero","febrero","marzo","abril","mayo","junio", &
& "julio","agosto","septiembre","octubre","noviembre","diciembre" ]
weekdays(3)(3:3)=char(233)
weekdays(6)(2:2)=char(225)
 ! ASCII months
weekdays_abbr(6)(2:2)=char(225)
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_spanish


subroutine locale_russian()
! LANG=russian
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "¿Ý","²â","Áà","Çâ","¿â","ÁÑ","²á"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "ßÞÝÕÔÕÛìÝØÚ","ÒâÞàÝØÚ","áàÕÔÐ","çÕâÒÕàÓ","ßïâÝØæÐ","áãÑÑÞâÐ","ÒÞáÚàÕáÕÝìÕ"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "ïÝÒ","äÕÒ","ÜÐà","Ðßà","ÜÐÙ","ØîÝ", &
& "ØîÛ","ÐÒÓ","áÕÝ","ÞÚâ","ÝÞï","ÔÕÚ" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "ÏÝÒÐàì","ÄÕÒàÐÛì","¼Ðàâ","°ßàÕÛì","¼ÐÙ","¸îÝì", &
& "¸îÛì","°ÒÓãáâ","ÁÕÝâïÑàì","¾ÚâïÑàì","½ÞïÑàì","´ÕÚÐÑàì" ]
weekdays(1)(1:1)=char(223)
weekdays(1)(2:2)=char(222)
weekdays(1)(3:3)=char(221)
weekdays(1)(4:4)=char(213)
weekdays(1)(5:5)=char(212)
weekdays(1)(6:6)=char(213)
weekdays(1)(7:7)=char(219)
weekdays(1)(8:8)=char(236)
weekdays(1)(9:9)=char(221)
weekdays(1)(10:10)=char(216)
weekdays(1)(11:11)=char(218)
weekdays(2)(1:1)=char(210)
weekdays(2)(2:2)=char(226)
weekdays(2)(3:3)=char(222)
weekdays(2)(4:4)=char(224)
weekdays(2)(5:5)=char(221)
weekdays(2)(6:6)=char(216)
weekdays(2)(7:7)=char(218)
weekdays(3)(1:1)=char(225)
weekdays(3)(2:2)=char(224)
weekdays(3)(3:3)=char(213)
weekdays(3)(4:4)=char(212)
weekdays(3)(5:5)=char(208)
weekdays(4)(1:1)=char(231)
weekdays(4)(2:2)=char(213)
weekdays(4)(3:3)=char(226)
weekdays(4)(4:4)=char(210)
weekdays(4)(5:5)=char(213)
weekdays(4)(6:6)=char(224)
weekdays(4)(7:7)=char(211)
weekdays(5)(1:1)=char(223)
weekdays(5)(2:2)=char(239)
weekdays(5)(3:3)=char(226)
weekdays(5)(4:4)=char(221)
weekdays(5)(5:5)=char(216)
weekdays(5)(6:6)=char(230)
weekdays(5)(7:7)=char(208)
weekdays(6)(1:1)=char(225)
weekdays(6)(2:2)=char(227)
weekdays(6)(3:3)=char(209)
weekdays(6)(4:4)=char(209)
weekdays(6)(5:5)=char(222)
weekdays(6)(6:6)=char(226)
weekdays(6)(7:7)=char(208)
weekdays(7)(1:1)=char(210)
weekdays(7)(2:2)=char(222)
weekdays(7)(3:3)=char(225)
weekdays(7)(4:4)=char(218)
weekdays(7)(5:5)=char(224)
weekdays(7)(6:6)=char(213)
weekdays(7)(7:7)=char(225)
weekdays(7)(8:8)=char(213)
weekdays(7)(9:9)=char(221)
weekdays(7)(10:10)=char(236)
weekdays(7)(11:11)=char(213)
months(1)(1:1)=char(207)
months(1)(2:2)=char(221)
months(1)(3:3)=char(210)
months(1)(4:4)=char(208)
months(1)(5:5)=char(224)
months(1)(6:6)=char(236)
months(2)(1:1)=char(196)
months(2)(2:2)=char(213)
months(2)(3:3)=char(210)
months(2)(4:4)=char(224)
months(2)(5:5)=char(208)
months(2)(6:6)=char(219)
months(2)(7:7)=char(236)
months(3)(1:1)=char(188)
months(3)(2:2)=char(208)
months(3)(3:3)=char(224)
months(3)(4:4)=char(226)
months(4)(1:1)=char(176)
months(4)(2:2)=char(223)
months(4)(3:3)=char(224)
months(4)(4:4)=char(213)
months(4)(5:5)=char(219)
months(4)(6:6)=char(236)
months(5)(1:1)=char(188)
months(5)(2:2)=char(208)
months(5)(3:3)=char(217)
months(6)(1:1)=char(184)
months(6)(2:2)=char(238)
months(6)(3:3)=char(221)
months(6)(4:4)=char(236)
months(7)(1:1)=char(184)
months(7)(2:2)=char(238)
months(7)(3:3)=char(219)
months(7)(4:4)=char(236)
months(8)(1:1)=char(176)
months(8)(2:2)=char(210)
months(8)(3:3)=char(211)
months(8)(4:4)=char(227)
months(8)(5:5)=char(225)
months(8)(6:6)=char(226)
months(9)(1:1)=char(193)
months(9)(2:2)=char(213)
months(9)(3:3)=char(221)
months(9)(4:4)=char(226)
months(9)(5:5)=char(239)
months(9)(6:6)=char(209)
months(9)(7:7)=char(224)
months(9)(8:8)=char(236)
months(10)(1:1)=char(190)
months(10)(2:2)=char(218)
months(10)(3:3)=char(226)
months(10)(4:4)=char(239)
months(10)(5:5)=char(209)
months(10)(6:6)=char(224)
months(10)(7:7)=char(236)
months(11)(1:1)=char(189)
months(11)(2:2)=char(222)
months(11)(3:3)=char(239)
months(11)(4:4)=char(209)
months(11)(5:5)=char(224)
months(11)(6:6)=char(236)
months(12)(1:1)=char(180)
months(12)(2:2)=char(213)
months(12)(3:3)=char(218)
months(12)(4:4)=char(208)
months(12)(5:5)=char(209)
months(12)(6:6)=char(224)
months(12)(7:7)=char(236)
weekdays_abbr(1)(1:1)=char(191)
weekdays_abbr(1)(2:2)=char(221)
weekdays_abbr(2)(1:1)=char(178)
weekdays_abbr(2)(2:2)=char(226)
weekdays_abbr(3)(1:1)=char(193)
weekdays_abbr(3)(2:2)=char(224)
weekdays_abbr(4)(1:1)=char(199)
weekdays_abbr(4)(2:2)=char(226)
weekdays_abbr(5)(1:1)=char(191)
weekdays_abbr(5)(2:2)=char(226)
weekdays_abbr(6)(1:1)=char(193)
weekdays_abbr(6)(2:2)=char(209)
weekdays_abbr(7)(1:1)=char(178)
weekdays_abbr(7)(2:2)=char(225)
months_abbr(1)(1:1)=char(239)
months_abbr(1)(2:2)=char(221)
months_abbr(1)(3:3)=char(210)
months_abbr(2)(1:1)=char(228)
months_abbr(2)(2:2)=char(213)
months_abbr(2)(3:3)=char(210)
months_abbr(3)(1:1)=char(220)
months_abbr(3)(2:2)=char(208)
months_abbr(3)(3:3)=char(224)
months_abbr(4)(1:1)=char(208)
months_abbr(4)(2:2)=char(223)
months_abbr(4)(3:3)=char(224)
months_abbr(5)(1:1)=char(220)
months_abbr(5)(2:2)=char(208)
months_abbr(5)(3:3)=char(217)
months_abbr(6)(1:1)=char(216)
months_abbr(6)(2:2)=char(238)
months_abbr(6)(3:3)=char(221)
months_abbr(7)(1:1)=char(216)
months_abbr(7)(2:2)=char(238)
months_abbr(7)(3:3)=char(219)
months_abbr(8)(1:1)=char(208)
months_abbr(8)(2:2)=char(210)
months_abbr(8)(3:3)=char(211)
months_abbr(9)(1:1)=char(225)
months_abbr(9)(2:2)=char(213)
months_abbr(9)(3:3)=char(221)
months_abbr(10)(1:1)=char(222)
months_abbr(10)(2:2)=char(218)
months_abbr(10)(3:3)=char(226)
months_abbr(11)(1:1)=char(221)
months_abbr(11)(2:2)=char(222)
months_abbr(11)(3:3)=char(239)
months_abbr(12)(1:1)=char(212)
months_abbr(12)(2:2)=char(213)
months_abbr(12)(3:3)=char(218)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_russian


subroutine locale_norwegian()
! LANG=norwegian
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "man.","tir.","ons.","tor.","fre.","lør.","søn."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "mandag","tirsdag","onsdag","torsdag","fredag","lørdag","søndag"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan","feb","mar","apr","mai","jun", &
& "jul","aug","sep","okt","nov","des" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "januar","februar","mars","april","mai","juni", &
& "juli","august","september","oktober","november","desember" ]
weekdays(6)(2:2)=char(248)
weekdays(7)(2:2)=char(248)
 ! ASCII months
weekdays_abbr(6)(2:2)=char(248)
weekdays_abbr(7)(2:2)=char(248)
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_norwegian


subroutine locale_bokmal()
! LANG=bokmal
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "man.","tir.","ons.","tor.","fre.","lør.","søn."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "mandag","tirsdag","onsdag","torsdag","fredag","lørdag","søndag"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan","feb","mar","apr","mai","jun", &
& "jul","aug","sep","okt","nov","des" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "januar","februar","mars","april","mai","juni", &
& "juli","august","september","oktober","november","desember" ]

weekdays(6)(2:2)=char(248)
weekdays(7)(2:2)=char(248)
 ! ASCII months
weekdays_abbr(6)(2:2)=char(248)
weekdays_abbr(7)(2:2)=char(248)
 ! ASCII months_abbr

   call locale("user",months,weekdays,months_abbr,weekdays_abbr)

end subroutine locale_bokmal

subroutine locale_dansk()
! LANG=dansk
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "ma","ti","on","to","fr","lø","sø"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "mandag","tirsdag","onsdag","torsdag","fredag","lørdag","søndag"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan","feb","mar","apr","maj","jun", &
& "jul","aug","sep","okt","nov","dec" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "januar","februar","marts","april","maj","juni", &
& "juli","august","september","oktober","november","december" ]
weekdays(6)(2:2)=char(248)
weekdays(7)(2:2)=char(248)
 ! ASCII months
weekdays_abbr(6)(2:2)=char(248)
weekdays_abbr(7)(2:2)=char(248)
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_dansk


subroutine locale_nynorsk()
! LANG=nynorsk
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "mån","tys","ons","tor","fre","lau","søn"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "måndag","tysdag","onsdag","torsdag","fredag","laurdag","søndag"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan","feb","mar","apr","mai","jun", &
& "jul","aug","sep","okt","nov","des" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "januar","februar","mars","april","mai","juni", &
& "juli","august","september","oktober","november","desember" ]
weekdays(1)(2:2)=char(229)
weekdays(7)(2:2)=char(248)
 ! ASCII months
weekdays_abbr(1)(2:2)=char(229)
weekdays_abbr(7)(2:2)=char(248)
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_nynorsk



subroutine locale_swedish()
! LANG=swedish
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "mån","tis","ons","tor","fre","lör","sön"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "måndag","tisdag","onsdag","torsdag","fredag","lördag","söndag"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan","feb","mar","apr","maj","jun", &
& "jul","aug","sep","okt","nov","dec" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "januari","februari","mars","april","maj","juni", &
& "juli","augusti","september","oktober","november","december" ]
weekdays(1)(2:2)=char(229)
weekdays(6)(2:2)=char(246)
weekdays(7)(2:2)=char(246)
 ! ASCII months
weekdays_abbr(1)(2:2)=char(229)
weekdays_abbr(6)(2:2)=char(246)
weekdays_abbr(7)(2:2)=char(246)
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_swedish


subroutine locale_dutch()
! LANG=dutch
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "ma","di","wo","do","vr","za","zo"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag","zondag"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan","feb","mrt","apr","mei","jun", &
& "jul","aug","sep","okt","nov","dec" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "januari","februari","maart","april","mei","juni", &
& "juli","augustus","september","oktober","november","december" ]
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_dutch


subroutine locale_finnish()
! LANG=finnish
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "ma","ti","ke","to","pe","la","su"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "maanantai","tiistai","keskiviikko","torstai","perjantai","lauantai","sunnuntai"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "tammi","helmi","maalis","huhti","touko","kesä", &
& "heinä","elo","syys","loka","marras","joulu" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "tammikuu","helmikuu","maaliskuu","huhtikuu","toukokuu","kesäkuu", &
& "heinäkuu","elokuu","syyskuu","lokakuu","marraskuu","joulukuu" ]
 ! ASCII weekdays
months(6)(4:4)=char(228)
months(7)(5:5)=char(228)
 ! ASCII weekdays_abbr
months_abbr(6)(4:4)=char(228)
months_abbr(7)(5:5)=char(228)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_finnish


subroutine locale_french()
! LANG=french
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "lun.","mar.","mer.","jeu.","ven.","sam.","dim."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "janv.","févr.","mars","avr.","mai","juin", &
& "juil.","août","sept.","oct.","nov.","déc." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "janvier","février","mars","avril","mai","juin", &
& "juillet","août","septembre","octobre","novembre","décembre" ]
 ! ASCII weekdays
months(2)(2:2)=char(233)
months(8)(3:3)=char(251)
months(12)(2:2)=char(233)
 ! ASCII weekdays_abbr
months_abbr(2)(2:2)=char(233)
months_abbr(8)(3:3)=char(251)
months_abbr(12)(2:2)=char(233)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_french


subroutine locale_galego()
! LANG=galego
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "Luns","Mar.","Mér.","Xov.","Ven.","Sáb.","Dom."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "luns","martes","mércores","xoves","venres","sábado","domingo"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "Xan.","Feb.","Mar.","Abr.","Maio","Xuño", &
& "Xul.","Ago.","Set.","Out.","Nov.","Dec." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "Xaneiro","Febreiro","Marzo","Abril","Maio","Xuño", &
& "Xullo","Agosto","Setembro","Outubro","Novembro","Decembro" ]
weekdays(3)(2:2)=char(233)
weekdays(6)(2:2)=char(225)
months(6)(3:3)=char(241)
weekdays_abbr(3)(2:2)=char(233)
weekdays_abbr(6)(2:2)=char(225)
months_abbr(6)(3:3)=char(241)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_galego

subroutine locale_greek()
! LANG=greek
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "Äåõ","Ôñé","Ôåô","Ðåì","Ðáñ","Óáâ","Êõñ"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "ÄåõôÝñá","Ôñßôç","ÔåôÜñôç","ÐÝìðôç","ÐáñáóêåõÞ","ÓÜââáôï","ÊõñéáêÞ"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "Éáí","Öåâ","Ìáñ","Áðñ","Ìáú","Éïõí", &
& "Éïõë","Áõã","Óåð","Ïêô","Íïå","Äåê" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "ÉáíïõÜñéïò","ÖåâñïõÜñéïò","ÌÜñôéïò","Áðñßëéïò","ÌÜéïò","Éïýíéïò", &
& "Éïýëéïò","Áýãïõóôïò","ÓåðôÝìâñéïò","Ïêôþâñéïò","ÍïÝìâñéïò","ÄåêÝìâñéïò" ]
weekdays(1)(1:1)=char(196)
weekdays(1)(2:2)=char(229)
weekdays(1)(3:3)=char(245)
weekdays(1)(4:4)=char(244)
weekdays(1)(5:5)=char(221)
weekdays(1)(6:6)=char(241)
weekdays(1)(7:7)=char(225)
weekdays(2)(1:1)=char(212)
weekdays(2)(2:2)=char(241)
weekdays(2)(3:3)=char(223)
weekdays(2)(4:4)=char(244)
weekdays(2)(5:5)=char(231)
weekdays(3)(1:1)=char(212)
weekdays(3)(2:2)=char(229)
weekdays(3)(3:3)=char(244)
weekdays(3)(4:4)=char(220)
weekdays(3)(5:5)=char(241)
weekdays(3)(6:6)=char(244)
weekdays(3)(7:7)=char(231)
weekdays(4)(1:1)=char(208)
weekdays(4)(2:2)=char(221)
weekdays(4)(3:3)=char(236)
weekdays(4)(4:4)=char(240)
weekdays(4)(5:5)=char(244)
weekdays(4)(6:6)=char(231)
weekdays(5)(1:1)=char(208)
weekdays(5)(2:2)=char(225)
weekdays(5)(3:3)=char(241)
weekdays(5)(4:4)=char(225)
weekdays(5)(5:5)=char(243)
weekdays(5)(6:6)=char(234)
weekdays(5)(7:7)=char(229)
weekdays(5)(8:8)=char(245)
weekdays(5)(9:9)=char(222)
weekdays(6)(1:1)=char(211)
weekdays(6)(2:2)=char(220)
weekdays(6)(3:3)=char(226)
weekdays(6)(4:4)=char(226)
weekdays(6)(5:5)=char(225)
weekdays(6)(6:6)=char(244)
weekdays(6)(7:7)=char(239)
weekdays(7)(1:1)=char(202)
weekdays(7)(2:2)=char(245)
weekdays(7)(3:3)=char(241)
weekdays(7)(4:4)=char(233)
weekdays(7)(5:5)=char(225)
weekdays(7)(6:6)=char(234)
weekdays(7)(7:7)=char(222)
months(1)(1:1)=char(201)
months(1)(2:2)=char(225)
months(1)(3:3)=char(237)
months(1)(4:4)=char(239)
months(1)(5:5)=char(245)
months(1)(6:6)=char(220)
months(1)(7:7)=char(241)
months(1)(8:8)=char(233)
months(1)(9:9)=char(239)
months(1)(10:10)=char(242)
months(2)(1:1)=char(214)
months(2)(2:2)=char(229)
months(2)(3:3)=char(226)
months(2)(4:4)=char(241)
months(2)(5:5)=char(239)
months(2)(6:6)=char(245)
months(2)(7:7)=char(220)
months(2)(8:8)=char(241)
months(2)(9:9)=char(233)
months(2)(10:10)=char(239)
months(2)(11:11)=char(242)
months(3)(1:1)=char(204)
months(3)(2:2)=char(220)
months(3)(3:3)=char(241)
months(3)(4:4)=char(244)
months(3)(5:5)=char(233)
months(3)(6:6)=char(239)
months(3)(7:7)=char(242)
months(4)(1:1)=char(193)
months(4)(2:2)=char(240)
months(4)(3:3)=char(241)
months(4)(4:4)=char(223)
months(4)(5:5)=char(235)
months(4)(6:6)=char(233)
months(4)(7:7)=char(239)
months(4)(8:8)=char(242)
months(5)(1:1)=char(204)
months(5)(2:2)=char(220)
months(5)(3:3)=char(233)
months(5)(4:4)=char(239)
months(5)(5:5)=char(242)
months(6)(1:1)=char(201)
months(6)(2:2)=char(239)
months(6)(3:3)=char(253)
months(6)(4:4)=char(237)
months(6)(5:5)=char(233)
months(6)(6:6)=char(239)
months(6)(7:7)=char(242)
months(7)(1:1)=char(201)
months(7)(2:2)=char(239)
months(7)(3:3)=char(253)
months(7)(4:4)=char(235)
months(7)(5:5)=char(233)
months(7)(6:6)=char(239)
months(7)(7:7)=char(242)
months(8)(1:1)=char(193)
months(8)(2:2)=char(253)
months(8)(3:3)=char(227)
months(8)(4:4)=char(239)
months(8)(5:5)=char(245)
months(8)(6:6)=char(243)
months(8)(7:7)=char(244)
months(8)(8:8)=char(239)
months(8)(9:9)=char(242)
months(9)(1:1)=char(211)
months(9)(2:2)=char(229)
months(9)(3:3)=char(240)
months(9)(4:4)=char(244)
months(9)(5:5)=char(221)
months(9)(6:6)=char(236)
months(9)(7:7)=char(226)
months(9)(8:8)=char(241)
months(9)(9:9)=char(233)
months(9)(10:10)=char(239)
months(9)(11:11)=char(242)
months(10)(1:1)=char(207)
months(10)(2:2)=char(234)
months(10)(3:3)=char(244)
months(10)(4:4)=char(254)
months(10)(5:5)=char(226)
months(10)(6:6)=char(241)
months(10)(7:7)=char(233)
months(10)(8:8)=char(239)
months(10)(9:9)=char(242)
months(11)(1:1)=char(205)
months(11)(2:2)=char(239)
months(11)(3:3)=char(221)
months(11)(4:4)=char(236)
months(11)(5:5)=char(226)
months(11)(6:6)=char(241)
months(11)(7:7)=char(233)
months(11)(8:8)=char(239)
months(11)(9:9)=char(242)
months(12)(1:1)=char(196)
months(12)(2:2)=char(229)
months(12)(3:3)=char(234)
months(12)(4:4)=char(221)
months(12)(5:5)=char(236)
months(12)(6:6)=char(226)
months(12)(7:7)=char(241)
months(12)(8:8)=char(233)
months(12)(9:9)=char(239)
months(12)(10:10)=char(242)
weekdays_abbr(1)(1:1)=char(196)
weekdays_abbr(1)(2:2)=char(229)
weekdays_abbr(1)(3:3)=char(245)
weekdays_abbr(2)(1:1)=char(212)
weekdays_abbr(2)(2:2)=char(241)
weekdays_abbr(2)(3:3)=char(233)
weekdays_abbr(3)(1:1)=char(212)
weekdays_abbr(3)(2:2)=char(229)
weekdays_abbr(3)(3:3)=char(244)
weekdays_abbr(4)(1:1)=char(208)
weekdays_abbr(4)(2:2)=char(229)
weekdays_abbr(4)(3:3)=char(236)
weekdays_abbr(5)(1:1)=char(208)
weekdays_abbr(5)(2:2)=char(225)
weekdays_abbr(5)(3:3)=char(241)
weekdays_abbr(6)(1:1)=char(211)
weekdays_abbr(6)(2:2)=char(225)
weekdays_abbr(6)(3:3)=char(226)
weekdays_abbr(7)(1:1)=char(202)
weekdays_abbr(7)(2:2)=char(245)
weekdays_abbr(7)(3:3)=char(241)
months_abbr(1)(1:1)=char(201)
months_abbr(1)(2:2)=char(225)
months_abbr(1)(3:3)=char(237)
months_abbr(2)(1:1)=char(214)
months_abbr(2)(2:2)=char(229)
months_abbr(2)(3:3)=char(226)
months_abbr(3)(1:1)=char(204)
months_abbr(3)(2:2)=char(225)
months_abbr(3)(3:3)=char(241)
months_abbr(4)(1:1)=char(193)
months_abbr(4)(2:2)=char(240)
months_abbr(4)(3:3)=char(241)
months_abbr(5)(1:1)=char(204)
months_abbr(5)(2:2)=char(225)
months_abbr(5)(3:3)=char(250)
months_abbr(6)(1:1)=char(201)
months_abbr(6)(2:2)=char(239)
months_abbr(6)(3:3)=char(245)
months_abbr(6)(4:4)=char(237)
months_abbr(7)(1:1)=char(201)
months_abbr(7)(2:2)=char(239)
months_abbr(7)(3:3)=char(245)
months_abbr(7)(4:4)=char(235)
months_abbr(8)(1:1)=char(193)
months_abbr(8)(2:2)=char(245)
months_abbr(8)(3:3)=char(227)
months_abbr(9)(1:1)=char(211)
months_abbr(9)(2:2)=char(229)
months_abbr(9)(3:3)=char(240)
months_abbr(10)(1:1)=char(207)
months_abbr(10)(2:2)=char(234)
months_abbr(10)(3:3)=char(244)
months_abbr(11)(1:1)=char(205)
months_abbr(11)(2:2)=char(239)
months_abbr(11)(3:3)=char(229)
months_abbr(12)(1:1)=char(196)
months_abbr(12)(2:2)=char(229)
months_abbr(12)(3:3)=char(234)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_greek


subroutine locale_hebrew()
! LANG=hebrew
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "éåí á","éåí â","éåí ã","éåí ä","éåí å","ùáú","éåí à"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "éåí ùðé","éåí ùìéùé","éåí øáéòé","éåí çîéùé","éåí ùéùé","ùáú","éåí øàùåï"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "éðå","ôáø","îøõ","àôø","îàé","éåð", &
& "éåì","àåâ","ñôè","àå÷","ðåá","ãöî" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "éðåàø","ôáøåàø","îøõ","àôøéì","îàé","éåðé", &
& "éåìé","àåâåñè","ñôèîáø","àå÷èåáø","ðåáîáø","ãöîáø" ]
weekdays(1)(1:1)=char(233)
weekdays(1)(2:2)=char(229)
weekdays(1)(3:3)=char(237)
weekdays(1)(4:4)=char(160)
weekdays(1)(5:5)=char(249)
weekdays(1)(6:6)=char(240)
weekdays(1)(7:7)=char(233)
weekdays(2)(1:1)=char(233)
weekdays(2)(2:2)=char(229)
weekdays(2)(3:3)=char(237)
weekdays(2)(4:4)=char(160)
weekdays(2)(5:5)=char(249)
weekdays(2)(6:6)=char(236)
weekdays(2)(7:7)=char(233)
weekdays(2)(8:8)=char(249)
weekdays(2)(9:9)=char(233)
weekdays(3)(1:1)=char(233)
weekdays(3)(2:2)=char(229)
weekdays(3)(3:3)=char(237)
weekdays(3)(4:4)=char(160)
weekdays(3)(5:5)=char(248)
weekdays(3)(6:6)=char(225)
weekdays(3)(7:7)=char(233)
weekdays(3)(8:8)=char(242)
weekdays(3)(9:9)=char(233)
weekdays(4)(1:1)=char(233)
weekdays(4)(2:2)=char(229)
weekdays(4)(3:3)=char(237)
weekdays(4)(4:4)=char(160)
weekdays(4)(5:5)=char(231)
weekdays(4)(6:6)=char(238)
weekdays(4)(7:7)=char(233)
weekdays(4)(8:8)=char(249)
weekdays(4)(9:9)=char(233)
weekdays(5)(1:1)=char(233)
weekdays(5)(2:2)=char(229)
weekdays(5)(3:3)=char(237)
weekdays(5)(4:4)=char(160)
weekdays(5)(5:5)=char(249)
weekdays(5)(6:6)=char(233)
weekdays(5)(7:7)=char(249)
weekdays(5)(8:8)=char(233)
weekdays(6)(1:1)=char(249)
weekdays(6)(2:2)=char(225)
weekdays(6)(3:3)=char(250)
weekdays(7)(1:1)=char(233)
weekdays(7)(2:2)=char(229)
weekdays(7)(3:3)=char(237)
weekdays(7)(4:4)=char(160)
weekdays(7)(5:5)=char(248)
weekdays(7)(6:6)=char(224)
weekdays(7)(7:7)=char(249)
weekdays(7)(8:8)=char(229)
weekdays(7)(9:9)=char(239)
months(1)(1:1)=char(233)
months(1)(2:2)=char(240)
months(1)(3:3)=char(229)
months(1)(4:4)=char(224)
months(1)(5:5)=char(248)
months(2)(1:1)=char(244)
months(2)(2:2)=char(225)
months(2)(3:3)=char(248)
months(2)(4:4)=char(229)
months(2)(5:5)=char(224)
months(2)(6:6)=char(248)
months(3)(1:1)=char(238)
months(3)(2:2)=char(248)
months(3)(3:3)=char(245)
months(4)(1:1)=char(224)
months(4)(2:2)=char(244)
months(4)(3:3)=char(248)
months(4)(4:4)=char(233)
months(4)(5:5)=char(236)
months(5)(1:1)=char(238)
months(5)(2:2)=char(224)
months(5)(3:3)=char(233)
months(6)(1:1)=char(233)
months(6)(2:2)=char(229)
months(6)(3:3)=char(240)
months(6)(4:4)=char(233)
months(7)(1:1)=char(233)
months(7)(2:2)=char(229)
months(7)(3:3)=char(236)
months(7)(4:4)=char(233)
months(8)(1:1)=char(224)
months(8)(2:2)=char(229)
months(8)(3:3)=char(226)
months(8)(4:4)=char(229)
months(8)(5:5)=char(241)
months(8)(6:6)=char(232)
months(9)(1:1)=char(241)
months(9)(2:2)=char(244)
months(9)(3:3)=char(232)
months(9)(4:4)=char(238)
months(9)(5:5)=char(225)
months(9)(6:6)=char(248)
months(10)(1:1)=char(224)
months(10)(2:2)=char(229)
months(10)(3:3)=char(247)
months(10)(4:4)=char(232)
months(10)(5:5)=char(229)
months(10)(6:6)=char(225)
months(10)(7:7)=char(248)
months(11)(1:1)=char(240)
months(11)(2:2)=char(229)
months(11)(3:3)=char(225)
months(11)(4:4)=char(238)
months(11)(5:5)=char(225)
months(11)(6:6)=char(248)
months(12)(1:1)=char(227)
months(12)(2:2)=char(246)
months(12)(3:3)=char(238)
months(12)(4:4)=char(225)
months(12)(5:5)=char(248)
weekdays_abbr(1)(1:1)=char(233)
weekdays_abbr(1)(2:2)=char(229)
weekdays_abbr(1)(3:3)=char(237)
weekdays_abbr(1)(4:4)=char(160)
weekdays_abbr(1)(5:5)=char(225)
weekdays_abbr(2)(1:1)=char(233)
weekdays_abbr(2)(2:2)=char(229)
weekdays_abbr(2)(3:3)=char(237)
weekdays_abbr(2)(4:4)=char(160)
weekdays_abbr(2)(5:5)=char(226)
weekdays_abbr(3)(1:1)=char(233)
weekdays_abbr(3)(2:2)=char(229)
weekdays_abbr(3)(3:3)=char(237)
weekdays_abbr(3)(4:4)=char(160)
weekdays_abbr(3)(5:5)=char(227)
weekdays_abbr(4)(1:1)=char(233)
weekdays_abbr(4)(2:2)=char(229)
weekdays_abbr(4)(3:3)=char(237)
weekdays_abbr(4)(4:4)=char(160)
weekdays_abbr(4)(5:5)=char(228)
weekdays_abbr(5)(1:1)=char(233)
weekdays_abbr(5)(2:2)=char(229)
weekdays_abbr(5)(3:3)=char(237)
weekdays_abbr(5)(4:4)=char(160)
weekdays_abbr(5)(5:5)=char(229)
weekdays_abbr(6)(1:1)=char(249)
weekdays_abbr(6)(2:2)=char(225)
weekdays_abbr(6)(3:3)=char(250)
weekdays_abbr(7)(1:1)=char(233)
weekdays_abbr(7)(2:2)=char(229)
weekdays_abbr(7)(3:3)=char(237)
weekdays_abbr(7)(4:4)=char(160)
weekdays_abbr(7)(5:5)=char(224)
months_abbr(1)(1:1)=char(233)
months_abbr(1)(2:2)=char(240)
months_abbr(1)(3:3)=char(229)
months_abbr(2)(1:1)=char(244)
months_abbr(2)(2:2)=char(225)
months_abbr(2)(3:3)=char(248)
months_abbr(3)(1:1)=char(238)
months_abbr(3)(2:2)=char(248)
months_abbr(3)(3:3)=char(245)
months_abbr(4)(1:1)=char(224)
months_abbr(4)(2:2)=char(244)
months_abbr(4)(3:3)=char(248)
months_abbr(5)(1:1)=char(238)
months_abbr(5)(2:2)=char(224)
months_abbr(5)(3:3)=char(233)
months_abbr(6)(1:1)=char(233)
months_abbr(6)(2:2)=char(229)
months_abbr(6)(3:3)=char(240)
months_abbr(7)(1:1)=char(233)
months_abbr(7)(2:2)=char(229)
months_abbr(7)(3:3)=char(236)
months_abbr(8)(1:1)=char(224)
months_abbr(8)(2:2)=char(229)
months_abbr(8)(3:3)=char(226)
months_abbr(9)(1:1)=char(241)
months_abbr(9)(2:2)=char(244)
months_abbr(9)(3:3)=char(232)
months_abbr(10)(1:1)=char(224)
months_abbr(10)(2:2)=char(229)
months_abbr(10)(3:3)=char(247)
months_abbr(11)(1:1)=char(240)
months_abbr(11)(2:2)=char(229)
months_abbr(11)(3:3)=char(225)
months_abbr(12)(1:1)=char(227)
months_abbr(12)(2:2)=char(246)
months_abbr(12)(3:3)=char(238)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_hebrew


subroutine locale_hrvatski()
! LANG=hrvatski
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "pon","uto","sri","èet","pet","sub","ned"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "ponedjeljak","utorak","srijeda","èetvrtak","petak","subota","nedjelja"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "sij","vlj","o¾u","tra","svi","lip", &
& "srp","kol","ruj","lis","stu","pro" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "sijeèanj","veljaèa","o¾ujak","travanj","svibanj","lipanj", &
& "srpanj","kolovoz","rujan","listopad","studeni","prosinac" ]
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
weekdays(4)(1:1)=char(232)
months(1)(5:5)=char(232)
months(2)(6:6)=char(232)
months(3)(2:2)=char(190)
weekdays_abbr(4)(1:1)=char(232)
months_abbr(3)(2:2)=char(190)

end subroutine locale_hrvatski


subroutine locale_hungarian()
! LANG=hungarian
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "H","K","Sze","Cs","P","Szo","V"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "hétfõ","kedd","szerda","csütörtök","péntek","szombat","vasárnap"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan.","febr.","márc.","ápr.","máj.","jún.", &
& "júl.","aug.","szept.","okt.","nov.","dec." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "január","február","március","április","május","június", &
& "július","augusztus","szeptember","október","november","december" ]
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
weekdays(1)(2:2)=char(233)
weekdays(1)(5:5)=char(245)
weekdays(4)(3:3)=char(252)
weekdays(4)(5:5)=char(246)
weekdays(4)(8:8)=char(246)
weekdays(5)(2:2)=char(233)
weekdays(7)(4:4)=char(225)
months(1)(5:5)=char(225)
months(2)(6:6)=char(225)
months(3)(2:2)=char(225)
months(4)(1:1)=char(225)
months(5)(2:2)=char(225)
months(6)(2:2)=char(250)
months(7)(2:2)=char(250)
months(10)(4:4)=char(243)
 ! ASCII weekdays_abbr
months_abbr(3)(2:2)=char(225)
months_abbr(4)(1:1)=char(225)
months_abbr(5)(2:2)=char(225)
months_abbr(6)(2:2)=char(250)
months_abbr(7)(2:2)=char(250)
end subroutine locale_hungarian


subroutine locale_icelandic()
! LANG=icelandic
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "mán.","þri.","mið.","fim.","fös.","lau.","sun."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "mánudagur","þriðjudagur","miðvikudagur","fimmtudagur","föstudagur","laugardagur","sunnudagur"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan.","feb.","mar.","apr.","maí","jún.", &
& "júl.","ágú.","sep.","okt.","nóv.","des." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "janúar","febrúar","mars","apríl","maí","júní", &
& "júlí","ágúst","september","október","nóvember","desember" ]
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
weekdays(1)(2:2)=char(225)
weekdays(2)(1:1)=char(254)
weekdays(2)(4:4)=char(240)
weekdays(3)(3:3)=char(240)
weekdays(5)(2:2)=char(246)
months(1)(4:4)=char(250)
months(2)(5:5)=char(250)
months(4)(4:4)=char(237)
months(5)(3:3)=char(237)
months(6)(2:2)=char(250)
months(6)(4:4)=char(237)
months(7)(2:2)=char(250)
months(7)(4:4)=char(237)
months(8)(1:1)=char(225)
months(8)(3:3)=char(250)
months(10)(4:4)=char(243)
months(11)(2:2)=char(243)
weekdays_abbr(1)(2:2)=char(225)
weekdays_abbr(2)(1:1)=char(254)
weekdays_abbr(3)(3:3)=char(240)
weekdays_abbr(5)(2:2)=char(246)
months_abbr(5)(3:3)=char(237)
months_abbr(6)(2:2)=char(250)
months_abbr(7)(2:2)=char(250)
months_abbr(8)(1:1)=char(225)
months_abbr(8)(3:3)=char(250)
months_abbr(11)(2:2)=char(243)
end subroutine locale_icelandic


subroutine locale_italian()
! LANG=italian
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "lun","mar","mer","gio","ven","sab","dom"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "lunedì","martedì","mercoledì","giovedì","venerdì","sabato","domenica"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "gen","feb","mar","apr","mag","giu", &
& "lug","ago","set","ott","nov","dic" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "gennaio","febbraio","marzo","aprile","maggio","giugno", &
& "luglio","agosto","settembre","ottobre","novembre","dicembre" ]
weekdays(1)(6:6)=char(236)
weekdays(2)(7:7)=char(236)
weekdays(3)(9:9)=char(236)
weekdays(4)(7:7)=char(236)
weekdays(5)(7:7)=char(236)
 ! ASCII months
 ! ASCII weekdays_abbr
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_italian


subroutine locale_japanese()
! LANG=japanese
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "·î","²Ð","¿å","ÌÚ","¶â","ÅÚ","Æü"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "·îÍËÆü","²ÐÍËÆü","¿åÍËÆü","ÌÚÍËÆü","¶âÍËÆü","ÅÚÍËÆü","ÆüÍËÆü"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "1·î","2·î","3·î","4·î","5·î","6·î", &
& "7·î","8·î","9·î","10·î","11·î","12·î" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "1·î","2·î","3·î","4·î","5·î","6·î", &
& "7·î","8·î","9·î","10·î","11·î","12·î" ]
weekdays(1)(1:1)=char(183)
weekdays(1)(2:2)=char(238)
weekdays(1)(3:3)=char(205)
weekdays(1)(4:4)=char(203)
weekdays(1)(5:5)=char(198)
weekdays(1)(6:6)=char(252)
weekdays(2)(1:1)=char(178)
weekdays(2)(2:2)=char(208)
weekdays(2)(3:3)=char(205)
weekdays(2)(4:4)=char(203)
weekdays(2)(5:5)=char(198)
weekdays(2)(6:6)=char(252)
weekdays(3)(1:1)=char(191)
weekdays(3)(2:2)=char(229)
weekdays(3)(3:3)=char(205)
weekdays(3)(4:4)=char(203)
weekdays(3)(5:5)=char(198)
weekdays(3)(6:6)=char(252)
weekdays(4)(1:1)=char(204)
weekdays(4)(2:2)=char(218)
weekdays(4)(3:3)=char(205)
weekdays(4)(4:4)=char(203)
weekdays(4)(5:5)=char(198)
weekdays(4)(6:6)=char(252)
weekdays(5)(1:1)=char(182)
weekdays(5)(2:2)=char(226)
weekdays(5)(3:3)=char(205)
weekdays(5)(4:4)=char(203)
weekdays(5)(5:5)=char(198)
weekdays(5)(6:6)=char(252)
weekdays(6)(1:1)=char(197)
weekdays(6)(2:2)=char(218)
weekdays(6)(3:3)=char(205)
weekdays(6)(4:4)=char(203)
weekdays(6)(5:5)=char(198)
weekdays(6)(6:6)=char(252)
weekdays(7)(1:1)=char(198)
weekdays(7)(2:2)=char(252)
weekdays(7)(3:3)=char(205)
weekdays(7)(4:4)=char(203)
weekdays(7)(5:5)=char(198)
weekdays(7)(6:6)=char(252)
months(1)(2:2)=char(183)
months(1)(3:3)=char(238)
months(2)(2:2)=char(183)
months(2)(3:3)=char(238)
months(3)(2:2)=char(183)
months(3)(3:3)=char(238)
months(4)(2:2)=char(183)
months(4)(3:3)=char(238)
months(5)(2:2)=char(183)
months(5)(3:3)=char(238)
months(6)(2:2)=char(183)
months(6)(3:3)=char(238)
months(7)(2:2)=char(183)
months(7)(3:3)=char(238)
months(8)(2:2)=char(183)
months(8)(3:3)=char(238)
months(9)(2:2)=char(183)
months(9)(3:3)=char(238)
months(10)(3:3)=char(183)
months(10)(4:4)=char(238)
months(11)(3:3)=char(183)
months(11)(4:4)=char(238)
months(12)(3:3)=char(183)
months(12)(4:4)=char(238)
weekdays_abbr(1)(1:1)=char(183)
weekdays_abbr(1)(2:2)=char(238)
weekdays_abbr(2)(1:1)=char(178)
weekdays_abbr(2)(2:2)=char(208)
weekdays_abbr(3)(1:1)=char(191)
weekdays_abbr(3)(2:2)=char(229)
weekdays_abbr(4)(1:1)=char(204)
weekdays_abbr(4)(2:2)=char(218)
weekdays_abbr(5)(1:1)=char(182)
weekdays_abbr(5)(2:2)=char(226)
weekdays_abbr(6)(1:1)=char(197)
weekdays_abbr(6)(2:2)=char(218)
weekdays_abbr(7)(1:1)=char(198)
weekdays_abbr(7)(2:2)=char(252)
months_abbr(1)(2:2)=char(183)
months_abbr(1)(3:3)=char(238)
months_abbr(2)(2:2)=char(183)
months_abbr(2)(3:3)=char(238)
months_abbr(3)(2:2)=char(183)
months_abbr(3)(3:3)=char(238)
months_abbr(4)(2:2)=char(183)
months_abbr(4)(3:3)=char(238)
months_abbr(5)(2:2)=char(183)
months_abbr(5)(3:3)=char(238)
months_abbr(6)(2:2)=char(183)
months_abbr(6)(3:3)=char(238)
months_abbr(7)(2:2)=char(183)
months_abbr(7)(3:3)=char(238)
months_abbr(8)(2:2)=char(183)
months_abbr(8)(3:3)=char(238)
months_abbr(9)(2:2)=char(183)
months_abbr(9)(3:3)=char(238)
months_abbr(10)(3:3)=char(183)
months_abbr(10)(4:4)=char(238)
months_abbr(11)(3:3)=char(183)
months_abbr(11)(4:4)=char(238)
months_abbr(12)(3:3)=char(183)
months_abbr(12)(4:4)=char(238)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_japanese


subroutine locale_korean()
! LANG=korean
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "¿ù","È­","¼ö","¸ñ","±Ý","Åä","ÀÏ"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "¿ù¿äÀÏ","È­¿äÀÏ","¼ö¿äÀÏ","¸ñ¿äÀÏ","±Ý¿äÀÏ","Åä¿äÀÏ","ÀÏ¿äÀÏ"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "1¿ù","2¿ù","3¿ù","4¿ù","5¿ù","6¿ù", &
& "7¿ù","8¿ù","9¿ù","10¿ù","11¿ù","12¿ù" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "1¿ù","2¿ù","3¿ù","4¿ù","5¿ù","6¿ù", &
& "7¿ù","8¿ù","9¿ù","10¿ù","11¿ù","12¿ù" ]
weekdays(1)(1:1)=char(191)
weekdays(1)(2:2)=char(249)
weekdays(1)(3:3)=char(191)
weekdays(1)(4:4)=char(228)
weekdays(1)(5:5)=char(192)
weekdays(1)(6:6)=char(207)
weekdays(2)(1:1)=char(200)
weekdays(2)(2:2)=char(173)
weekdays(2)(3:3)=char(191)
weekdays(2)(4:4)=char(228)
weekdays(2)(5:5)=char(192)
weekdays(2)(6:6)=char(207)
weekdays(3)(1:1)=char(188)
weekdays(3)(2:2)=char(246)
weekdays(3)(3:3)=char(191)
weekdays(3)(4:4)=char(228)
weekdays(3)(5:5)=char(192)
weekdays(3)(6:6)=char(207)
weekdays(4)(1:1)=char(184)
weekdays(4)(2:2)=char(241)
weekdays(4)(3:3)=char(191)
weekdays(4)(4:4)=char(228)
weekdays(4)(5:5)=char(192)
weekdays(4)(6:6)=char(207)
weekdays(5)(1:1)=char(177)
weekdays(5)(2:2)=char(221)
weekdays(5)(3:3)=char(191)
weekdays(5)(4:4)=char(228)
weekdays(5)(5:5)=char(192)
weekdays(5)(6:6)=char(207)
weekdays(6)(1:1)=char(197)
weekdays(6)(2:2)=char(228)
weekdays(6)(3:3)=char(191)
weekdays(6)(4:4)=char(228)
weekdays(6)(5:5)=char(192)
weekdays(6)(6:6)=char(207)
weekdays(7)(1:1)=char(192)
weekdays(7)(2:2)=char(207)
weekdays(7)(3:3)=char(191)
weekdays(7)(4:4)=char(228)
weekdays(7)(5:5)=char(192)
weekdays(7)(6:6)=char(207)
months(1)(2:2)=char(191)
months(1)(3:3)=char(249)
months(2)(2:2)=char(191)
months(2)(3:3)=char(249)
months(3)(2:2)=char(191)
months(3)(3:3)=char(249)
months(4)(2:2)=char(191)
months(4)(3:3)=char(249)
months(5)(2:2)=char(191)
months(5)(3:3)=char(249)
months(6)(2:2)=char(191)
months(6)(3:3)=char(249)
months(7)(2:2)=char(191)
months(7)(3:3)=char(249)
months(8)(2:2)=char(191)
months(8)(3:3)=char(249)
months(9)(2:2)=char(191)
months(9)(3:3)=char(249)
months(10)(3:3)=char(191)
months(10)(4:4)=char(249)
months(11)(3:3)=char(191)
months(11)(4:4)=char(249)
months(12)(3:3)=char(191)
months(12)(4:4)=char(249)
weekdays_abbr(1)(1:1)=char(191)
weekdays_abbr(1)(2:2)=char(249)
weekdays_abbr(2)(1:1)=char(200)
weekdays_abbr(2)(2:2)=char(173)
weekdays_abbr(3)(1:1)=char(188)
weekdays_abbr(3)(2:2)=char(246)
weekdays_abbr(4)(1:1)=char(184)
weekdays_abbr(4)(2:2)=char(241)
weekdays_abbr(5)(1:1)=char(177)
weekdays_abbr(5)(2:2)=char(221)
weekdays_abbr(6)(1:1)=char(197)
weekdays_abbr(6)(2:2)=char(228)
weekdays_abbr(7)(1:1)=char(192)
weekdays_abbr(7)(2:2)=char(207)
months_abbr(1)(2:2)=char(191)
months_abbr(1)(3:3)=char(249)
months_abbr(2)(2:2)=char(191)
months_abbr(2)(3:3)=char(249)
months_abbr(3)(2:2)=char(191)
months_abbr(3)(3:3)=char(249)
months_abbr(4)(2:2)=char(191)
months_abbr(4)(3:3)=char(249)
months_abbr(5)(2:2)=char(191)
months_abbr(5)(3:3)=char(249)
months_abbr(6)(2:2)=char(191)
months_abbr(6)(3:3)=char(249)
months_abbr(7)(2:2)=char(191)
months_abbr(7)(3:3)=char(249)
months_abbr(8)(2:2)=char(191)
months_abbr(8)(3:3)=char(249)
months_abbr(9)(2:2)=char(191)
months_abbr(9)(3:3)=char(249)
months_abbr(10)(3:3)=char(191)
months_abbr(10)(4:4)=char(249)
months_abbr(11)(3:3)=char(191)
months_abbr(11)(4:4)=char(249)
months_abbr(12)(3:3)=char(191)
months_abbr(12)(4:4)=char(249)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_korean


subroutine locale_lithuanian()
! LANG=lithuanian
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "pr","an","tr","kt","pn","ðt","sk"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "pirmadienis","antradienis","treèiadienis","ketvirtadienis","penktadienis","ðeðtadienis","sekmadienis"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "saus.","vas.","kov.","bal.","geg.","birþ.", &
& "liep.","rugp.","rugs.","spal.","lapkr.","gruod." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "sausis","vasaris","kovas","balandis","geguþë","birþelis", &
& "liepa","rugpjûtis","rugsëjis","spalis","lapkritis","gruodis" ]
weekdays(3)(4:4)=char(232)
weekdays(6)(1:1)=char(240)
weekdays(6)(3:3)=char(240)
months(5)(5:5)=char(254)
months(5)(6:6)=char(235)
months(6)(4:4)=char(254)
months(8)(6:6)=char(251)
months(9)(5:5)=char(235)
weekdays_abbr(6)(1:1)=char(240)
months_abbr(6)(4:4)=char(254)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_lithuanian


subroutine locale_polish()
! LANG=polish
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "pon.","wt.","¶r.","czw.","pt.","sob.","niedz."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "poniedzia³ek","wtorek","¶roda","czwartek","pi±tek","sobota","niedziela"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "sty","lut","mar","kwi","maj","cze", &
& "lip","sie","wrz","pa¼","lis","gru" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "styczeñ","luty","marzec","kwiecieñ","maj","czerwiec", &
& "lipiec","sierpieñ","wrzesieñ","pa¼dziernik","listopad","grudzieñ" ]
weekdays(1)(10:10)=char(179)
weekdays(3)(1:1)=char(182)
weekdays(5)(3:3)=char(177)
months(1)(7:7)=char(241)
months(4)(8:8)=char(241)
months(8)(8:8)=char(241)
months(9)(8:8)=char(241)
months(10)(3:3)=char(188)
months(12)(8:8)=char(241)
weekdays_abbr(3)(1:1)=char(182)
months_abbr(10)(3:3)=char(188)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_polish


subroutine locale_portuguese()
! LANG=portuguese
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "seg","ter","qua","qui","sex","sáb","dom"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "segunda-feira","terça-feira","quarta-feira","quinta-feira","sexta-feira","sábado","domingo"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan","fev","mar","abr","mai","jun", &
& "jul","ago","set","out","nov","dez" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "janeiro","fevereiro","março","abril","maio","junho", &
& "julho","agosto","setembro","outubro","novembro","dezembro" ]
weekdays(2)(4:4)=char(231)
weekdays(6)(2:2)=char(225)
months(3)(4:4)=char(231)
weekdays_abbr(6)(2:2)=char(225)
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_portuguese


subroutine locale_romanian()
! LANG=romanian
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "lun.","mar.","mie.","joi","vin.","sâm.","dum."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "luni","mari","miercuri","joi","vineri","sâmbãtã","duminicã"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "ian.","feb.","mar.","apr.","mai","iun.", &
& "iul.","aug.","sept.","oct.","nov.","dec." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "ianuarie","februarie","martie","aprilie","mai","iunie", &
& "iulie","august","septembrie","octombrie","noiembrie","decembrie" ]
weekdays(6)(2:2)=char(226)
weekdays(6)(5:5)=char(227)
weekdays(6)(7:7)=char(227)
weekdays(7)(8:8)=char(227)
 ! ASCII months
weekdays_abbr(6)(2:2)=char(226)
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_romanian


subroutine locale_slovene()
! LANG=slovene
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "pon.","tor.","sre.","èet.","pet.","sob.","ned."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "ponedeljek","torek","sreda","èetrtek","petek","sobota","nedelja"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jan.","feb.","mar.","apr.","maj","jun.", &
& "jul.","avg.","sep.","okt.","nov.","dec." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "januar","februar","marec","april","maj","junij", &
& "julij","avgust","september","oktober","november","december" ]
weekdays(4)(1:1)=char(232)
 ! ASCII months
weekdays_abbr(4)(1:1)=char(232)
 ! ASCII months_abbr
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_slovene


subroutine locale_thai()
! LANG=thai
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "¨.","Í.","¾.","¾Ä.","È.","Ê.","ÍÒ."]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "¨Ñ¹·Ãì","ÍÑ§¤ÒÃ","¾Ø¸","¾ÄËÑÊº´Õ","ÈØ¡Ãì","àÊÒÃì","ÍÒ·ÔµÂì"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "Á.¤.","¡.¾.","ÁÕ.¤.","àÁ.Â.","¾.¤.","ÁÔ.Â.", &
& "¡.¤.","Ê.¤.","¡.Â.","µ.¤.","¾.Â.","¸.¤." ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "Á¡ÃÒ¤Á","¡ØÁÀÒ¾Ñ¹¸ì","ÁÕ¹Ò¤Á","àÁÉÒÂ¹","¾ÄÉÀÒ¤Á","ÁÔ¶Ø¹ÒÂ¹", &
& "¡Ã¡®Ò¤Á","ÊÔ§ËÒ¤Á","¡Ñ¹ÂÒÂ¹","µØÅÒ¤Á","¾ÄÈ¨Ô¡ÒÂ¹","¸Ñ¹ÇÒ¤Á" ]
weekdays(1)(1:1)=char(168)
weekdays(1)(2:2)=char(209)
weekdays(1)(3:3)=char(185)
weekdays(1)(4:4)=char(183)
weekdays(1)(5:5)=char(195)
weekdays(1)(6:6)=char(236)
weekdays(2)(1:1)=char(205)
weekdays(2)(2:2)=char(209)
weekdays(2)(3:3)=char(167)
weekdays(2)(4:4)=char(164)
weekdays(2)(5:5)=char(210)
weekdays(2)(6:6)=char(195)
weekdays(3)(1:1)=char(190)
weekdays(3)(2:2)=char(216)
weekdays(3)(3:3)=char(184)
weekdays(4)(1:1)=char(190)
weekdays(4)(2:2)=char(196)
weekdays(4)(3:3)=char(203)
weekdays(4)(4:4)=char(209)
weekdays(4)(5:5)=char(202)
weekdays(4)(6:6)=char(186)
weekdays(4)(7:7)=char(180)
weekdays(4)(8:8)=char(213)
weekdays(5)(1:1)=char(200)
weekdays(5)(2:2)=char(216)
weekdays(5)(3:3)=char(161)
weekdays(5)(4:4)=char(195)
weekdays(5)(5:5)=char(236)
weekdays(6)(1:1)=char(224)
weekdays(6)(2:2)=char(202)
weekdays(6)(3:3)=char(210)
weekdays(6)(4:4)=char(195)
weekdays(6)(5:5)=char(236)
weekdays(7)(1:1)=char(205)
weekdays(7)(2:2)=char(210)
weekdays(7)(3:3)=char(183)
weekdays(7)(4:4)=char(212)
weekdays(7)(5:5)=char(181)
weekdays(7)(6:6)=char(194)
weekdays(7)(7:7)=char(236)
months(1)(1:1)=char(193)
months(1)(2:2)=char(161)
months(1)(3:3)=char(195)
months(1)(4:4)=char(210)
months(1)(5:5)=char(164)
months(1)(6:6)=char(193)
months(2)(1:1)=char(161)
months(2)(2:2)=char(216)
months(2)(3:3)=char(193)
months(2)(4:4)=char(192)
months(2)(5:5)=char(210)
months(2)(6:6)=char(190)
months(2)(7:7)=char(209)
months(2)(8:8)=char(185)
months(2)(9:9)=char(184)
months(2)(10:10)=char(236)
months(3)(1:1)=char(193)
months(3)(2:2)=char(213)
months(3)(3:3)=char(185)
months(3)(4:4)=char(210)
months(3)(5:5)=char(164)
months(3)(6:6)=char(193)
months(4)(1:1)=char(224)
months(4)(2:2)=char(193)
months(4)(3:3)=char(201)
months(4)(4:4)=char(210)
months(4)(5:5)=char(194)
months(4)(6:6)=char(185)
months(5)(1:1)=char(190)
months(5)(2:2)=char(196)
months(5)(3:3)=char(201)
months(5)(4:4)=char(192)
months(5)(5:5)=char(210)
months(5)(6:6)=char(164)
months(5)(7:7)=char(193)
months(6)(1:1)=char(193)
months(6)(2:2)=char(212)
months(6)(3:3)=char(182)
months(6)(4:4)=char(216)
months(6)(5:5)=char(185)
months(6)(6:6)=char(210)
months(6)(7:7)=char(194)
months(6)(8:8)=char(185)
months(7)(1:1)=char(161)
months(7)(2:2)=char(195)
months(7)(3:3)=char(161)
months(7)(4:4)=char(174)
months(7)(5:5)=char(210)
months(7)(6:6)=char(164)
months(7)(7:7)=char(193)
months(8)(1:1)=char(202)
months(8)(2:2)=char(212)
months(8)(3:3)=char(167)
months(8)(4:4)=char(203)
months(8)(5:5)=char(210)
months(8)(6:6)=char(164)
months(8)(7:7)=char(193)
months(9)(1:1)=char(161)
months(9)(2:2)=char(209)
months(9)(3:3)=char(185)
months(9)(4:4)=char(194)
months(9)(5:5)=char(210)
months(9)(6:6)=char(194)
months(9)(7:7)=char(185)
months(10)(1:1)=char(181)
months(10)(2:2)=char(216)
months(10)(3:3)=char(197)
months(10)(4:4)=char(210)
months(10)(5:5)=char(164)
months(10)(6:6)=char(193)
months(11)(1:1)=char(190)
months(11)(2:2)=char(196)
months(11)(3:3)=char(200)
months(11)(4:4)=char(168)
months(11)(5:5)=char(212)
months(11)(6:6)=char(161)
months(11)(7:7)=char(210)
months(11)(8:8)=char(194)
months(11)(9:9)=char(185)
months(12)(1:1)=char(184)
months(12)(2:2)=char(209)
months(12)(3:3)=char(185)
months(12)(4:4)=char(199)
months(12)(5:5)=char(210)
months(12)(6:6)=char(164)
months(12)(7:7)=char(193)
weekdays_abbr(1)(1:1)=char(168)
weekdays_abbr(2)(1:1)=char(205)
weekdays_abbr(3)(1:1)=char(190)
weekdays_abbr(4)(1:1)=char(190)
weekdays_abbr(4)(2:2)=char(196)
weekdays_abbr(5)(1:1)=char(200)
weekdays_abbr(6)(1:1)=char(202)
weekdays_abbr(7)(1:1)=char(205)
weekdays_abbr(7)(2:2)=char(210)
months_abbr(1)(1:1)=char(193)
months_abbr(1)(3:3)=char(164)
months_abbr(2)(1:1)=char(161)
months_abbr(2)(3:3)=char(190)
months_abbr(3)(1:1)=char(193)
months_abbr(3)(2:2)=char(213)
months_abbr(3)(4:4)=char(164)
months_abbr(4)(1:1)=char(224)
months_abbr(4)(2:2)=char(193)
months_abbr(4)(4:4)=char(194)
months_abbr(5)(1:1)=char(190)
months_abbr(5)(3:3)=char(164)
months_abbr(6)(1:1)=char(193)
months_abbr(6)(2:2)=char(212)
months_abbr(6)(4:4)=char(194)
months_abbr(7)(1:1)=char(161)
months_abbr(7)(3:3)=char(164)
months_abbr(8)(1:1)=char(202)
months_abbr(8)(3:3)=char(164)
months_abbr(9)(1:1)=char(161)
months_abbr(9)(3:3)=char(194)
months_abbr(10)(1:1)=char(181)
months_abbr(10)(3:3)=char(164)
months_abbr(11)(1:1)=char(190)
months_abbr(11)(3:3)=char(194)
months_abbr(12)(1:1)=char(184)
months_abbr(12)(3:3)=char(164)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_thai


subroutine locale_turkish()
! LANG=turkish
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "Pzt","Sal","Çar","Per","Cum","Cmt","Paz"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "Pazartesi","Salý","Çarþamba","Perþembe","Cuma","Cumartesi","Pazar"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "Oca","Þub","Mar","Nis","May","Haz", &
& "Tem","Aðu","Eyl","Eki","Kas","Ara" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "Ocak","Þubat","Mart","Nisan","Mayýs","Haziran", &
& "Temmuz","Aðustos","Eylül","Ekim","Kasým","Aralýk" ]
weekdays(2)(4:4)=char(253)
weekdays(3)(1:1)=char(199)
weekdays(3)(4:4)=char(254)
weekdays(4)(4:4)=char(254)
months(2)(1:1)=char(222)
months(5)(4:4)=char(253)
months(8)(2:2)=char(240)
months(9)(4:4)=char(252)
months(11)(4:4)=char(253)
months(12)(5:5)=char(253)
weekdays_abbr(3)(1:1)=char(199)
months_abbr(2)(1:1)=char(222)
months_abbr(8)(2:2)=char(240)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_turkish


subroutine locale_POSIX()
! LANG=POSIX
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "Mon","Tue","Wed","Thu","Fri","Sat","Sun"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "Jan","Feb","Mar","Apr","May","Jun", &
& "Jul","Aug","Sep","Oct","Nov","Dec" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "January","February","March","April","May","June", &
& "July","August","September","October","November","December" ]
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_POSIX


subroutine locale_eesti()
! LANG=eesti
! AM= PM=
character(len=20),save :: weekdays_abbr(7)=[character(len=20) :: &
& "E","T","K","N","R","L","P"]
character(len=20),save :: weekdays(7)=[character(len=20) :: &
& "esmaspäev","teisipäev","kolmapäev","neljapäev","reede","laupäev","pühapäev"]
character(len=20),save :: months_abbr(12)=[character(len=20) :: &
& "jaan","veebr","märts","apr","mai","juuni", &
& "juuli","aug","sept","okt","nov","dets" ]
character(len=20),save :: months(12)=[character(len=20) :: &
& "jaanuar","veebruar","märts","aprill","mai","juuni", &
& "juuli","august","september","oktoober","november","detsember" ]
weekdays(1)(7:7)=char(228)
weekdays(2)(7:7)=char(228)
weekdays(3)(7:7)=char(228)
weekdays(4)(7:7)=char(228)
weekdays(6)(5:5)=char(228)
weekdays(7)(2:2)=char(252)
weekdays(7)(6:6)=char(228)
months(3)(2:2)=char(228)
 ! ASCII weekdays_abbr
months_abbr(3)(2:2)=char(228)
   call locale("user",months,weekdays,months_abbr,weekdays_abbr)
end subroutine locale_eesti
function get_env(name,default) result(value)
! a function that makes calling get_environment_variable(3) simple
implicit none
character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: default
character(len=:),allocatable         :: value
integer                              :: howbig
integer                              :: stat
integer                              :: length
   length=0
   value=''
   if(name.ne.'')then
      call get_environment_variable( name, &
      & length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
         !print *, name, " is not defined in the environment. Strange..."
         value=''
      case (2)
         !print *, "This processor does not support environment variables. Boooh!"
         value=''
      case default
         ! make string of sufficient size to hold value
         if(allocated(value))deallocate(value)
         allocate(character(len=max(howbig,1)) :: value)
         ! get value
         call get_environment_variable( &
         & name,value,status=stat,trim_name=.true.)
         if(stat.ne.0)value=''
      end select
   endif
   if(value.eq.''.and.present(default))value=default
end function get_env
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     phase_of_moon(3f) - [M_time:ASTROLOGICAL] return name for phase of
!!     moon for given date
!!     (LICENSE:MIT)
!!##SYNOPSIS
!!
!!   function phase_of_moon(dat)
!!
!!    integer,intent(in)            :: dat(8)
!!    character(len=:),allocatable  :: phase_of_moon
!!
!!##DESCRIPTION
!!   Phases Of The Moon
!!
!!   This procedure is used to support the %p field descriptor for the
!!   fmtdate(3f) routine.
!!
!!   The moon circles the earth every 29.530588853 days on average, so pick a
!!   starting point and count. A new moon occurred at Julian date 2451550.1
!!   (January 6, 2000, 18:14 UTC). Then it is easy to count the number of
!!   days since the last new moon. This is an approximate calculation.
!!
!!   There are eight generally recognized phases of the moon in common use
!!
!!    o new or dark
!!    o waxing crescent
!!    o first quarter
!!    o waxing gibbous
!!    o full
!!    o waning gibbous
!!    o last quarter
!!    o waning crescent
!!
!!   To calculate the phase of the moon simply divide the days since the
!!   last new moon by eight and select the appropriate phase.
!!
!!   Note that technically the four states (new, first quarter, full, third
!!   quarter) are events not phases. That is to say, the moon is technically
!!   only new for an instant.
!!
!!##EXAMPLES
!!
!!  Sample:
!!
!!   program demo_phase_of_moon
!!   use M_time, only : now
!!   use M_time, only : phase_of_moon
!!   use M_time, only : moon_fullness
!!   implicit none
!!   integer             :: dat(8)
!!    ! generate DAT array
!!    call date_and_time(values=dat)
!!    ! show DAT array
!!    write(*,'(" Today is:",*(i0:,":"))')dat
!!    ! the %p and %P fields are supported by fmtdate(3f)
!!    write(*,*)&
!!    & now('The phase of the moon is %p, with a fullness of %P')
!!    write(*,'(1x,*(a))',advance='no')&
!!    & 'The phase of the moon is ',trim( phase_of_moon(dat)),','
!!    write(*,'(1x,a,i0,a)')'with a fullness of ',moon_fullness(dat),'%'
!!   end program demo_phase_of_moon
!!
!!  Sample output:
!!
!!     Today is:2018:11:3:-240:20:18:44:245
!!     The phase of the moon is Waning crescent, with a fullness of -30%
!!     The phase of the moon is Waning crescent, with a fullness of -30%
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function phase_of_moon(dat)

! ident_28="@(#) M_time phase_of_moon(3f) return name for phase of moon for given date"

integer,intent(in)            :: dat(8)
character(len=:),allocatable  :: phase_of_moon

real(kind=realtime),parameter :: syndonic_month=29.530588853_realtime ! average period of a lunar cycle, or days per lunation
integer,parameter             :: reference(*)= [2000,1,6,0,18,14,0,0] ! new moon of January 2000 was January 6, 18:14 UTC.
character(len=15),parameter   :: phase_names(*)=[ "New            ", "Waxing crescent", &
                                                  "First quarter  ", "Waxing gibbous ", &
                                                  "Full           ", "Waning gibbous ", &
                                                  "Last quarter   ", "Waning crescent"  ]
real(kind=realtime),parameter :: phase_length=syndonic_month/8_realtime  ! days per phase
integer                       :: phase
real(kind=realtime)           :: days

days= d2j(dat)-d2j(reference)                               ! days between reference date and input date
days = mod(days + phase_length/2.0_dp, syndonic_month)        ! modulo calculation of which phase rounding up
if(days<0)days=days+syndonic_month                         ! correct for days before reference date
phase = int( days * ( size(phase_names) / syndonic_month ))+1 ! index into phase names
phase_of_moon=trim(phase_names(phase))

end function phase_of_moon
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     moon_fullness(3f) - [M_time:ASTROLOGICAL] return percentage of moon phase
!!     from new to full
!!     (LICENSE:MIT)
!!##SYNOPSIS
!!
!!   function moon_fullness(dat)
!!
!!    integer,intent(in) :: dat(8)
!!    integer            :: moon_fullness
!!
!!##DESCRIPTION
!!
!!   This procedure is used to support the %P field descriptor for the
!!   fmtdate(3f) routine.
!!
!!   The moon circles the earth every 29.530588853 days on average, so pick
!!   a starting point and count. A new moon occurred at January 6, 2000,
!!   18:14 UTC. Then it is easy to count the number of days since the last
!!   new moon. This is an approximate calculation.
!!
!!##OPTIONS
!!
!!    dat    DAT Date array describing input date
!!
!!##RESULTS
!!
!!    moon_fullness  0 is a new or dark moon, 100 is a full moon, + for waxing
!!                   and - for waning.
!!
!!##EXAMPLES
!!
!!    Sample:
!!
!!     program demo_moon_fullness
!!     use M_time, only : now
!!     use M_time, only : phase_of_moon
!!     use M_time, only : moon_fullness
!!     implicit none
!!     integer             :: dat(8)
!!        ! generate DAT array
!!        call date_and_time(values=dat)
!!        ! show DAT array
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        ! the %p and %P fields are supported by fmtdate(3f)
!!        write(*,*)&
!!        &now('The phase of the moon is %p, with a fullness of %P')
!!        write(*,'(1x,*(a))',advance='no')&
!!        &'The phase of the moon is ',trim( phase_of_moon(dat)),','
!!        write(*,'(1x,a,i0,a)')&
!!        &'with a fullness of ', moon_fullness(dat),'%'
!!     end program demo_moon_fullness
!!
!!    Sample output:
!!
!!      Today is:2018:11:3:-240:20:18:44:245
!!      The phase of the moon is Waning crescent, with a fullness of -30%
!!      The phase of the moon is Waning crescent, with a fullness of -30%
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
function moon_fullness(dat)

! ident_29="@(#) M_time moon_fullness(3f) return percentage of moon phase from new to full"

integer,intent(in)            :: dat(8)
integer                       :: moon_fullness

real(kind=realtime),parameter :: syndonic_month=29.530588853_realtime  ! average period of a lunar cycle, or days per lunation
integer,parameter             :: reference(*)= [2000,1,6,0,18,14,0,0]  ! new moon of January 2000 was January 6, 18:14 UTC.
real(kind=realtime)           :: days_into_cycle

days_into_cycle = mod(d2j(dat)-d2j(reference) , syndonic_month)      ! number of days into lunar cycle
if(days_into_cycle<0)days_into_cycle=days_into_cycle+syndonic_month ! correct for input date being before reference date

if(days_into_cycle<=syndonic_month/2.0_realtime)then                 ! if waxing from new to full report as 0% to 100%
   moon_fullness=int((days_into_cycle/syndonic_month)*200.0_realtime+0.5_realtime)
else                                                                   ! if waning from full to new report as -99% to -1%
   moon_fullness=-(200-int((days_into_cycle/syndonic_month)*200.0_realtime))
endif

end function moon_fullness
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    easter(3f) - [M_time:ASTROLOGICAL] calculate date for Easter given a year
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!   subroutine easter(year,dat)
!!
!!     integer, intent(in)   :: year
!!     integer, intent(out)  :: dat
!!
!!##DESCRIPTION
!!   The Date of Easter (Sunday)
!!
!!   The algorithm is due to J.-M. Oudin (1940) and is reprinted
!!   in the Explanatory Supplement to the Astronomical Almanac,
!!   ed. P. K. Seidelmann (1992). See Chapter 12, "Calendars", by
!!   L. E. Doggett.
!!
!!   The following are dates of Easter from 1980 to 2024:
!!
!!        1980  April  6        1995  April 16        2010  April  4
!!        1981  April 19        1996  April  7        2011  April 24
!!        1982  April 11        1997  March 30        2012  April  8
!!        1983  April  3        1998  April 12        2013  March 31
!!        1984  April 22        1999  April  4        2014  April 20
!!        1985  April  7        2000  April 23        2015  April  5
!!        1986  March 30        2001  April 15        2016  March 27
!!        1987  April 19        2002  March 31        2017  April 16
!!        1988  April  3        2003  April 20        2018  April  1
!!        1989  March 26        2004  April 11        2019  April 21
!!        1990  April 15        2005  March 27        2020  April 12
!!        1991  March 31        2006  April 16        2021  April  4
!!        1992  April 19        2007  April  8        2022  April 17
!!        1993  April 11        2008  March 23        2023  April  9
!!        1994  April  3        2009  April 12        2024  March 31
!!
!!   N.B. The date of Easter for the Eastern Orthodox Church may be different.
!!
!!##OPTIONS
!!      year    Year for which to calculate day that Easter falls on
!!##RESULTS
!!      dat     Date array for noon on Easter for the specified year
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!     program demo_easter
!!     use M_time, only : easter, fmtdate
!!     implicit none
!!     integer :: year
!!     integer :: dat(8) ! year,month,day,tz,hour,minute,second,millisecond
!!       call date_and_time(values=dat)  ! get current year
!!       year=dat(1)
!!       call easter(year, dat)
!!       write(*,*)fmtdate(dat,&
!!       "Easter day: the %d day of %L in the year of our Lord %Y")
!!     end program demo_easter
!!
!!    Sample output:
!!
!!     Easter day: the 16th day of April in the year of our Lord 2017
!>
!!
!!   U.S. Naval Observatory Astronomical Applications Department
!!
!!   This code assembled by Alan Miller
!!   Reference web site:
!!   http://aa.usno.navy.mil/faq/docs/easter.html
!!   Latest revision 8 April 2002
SUBROUTINE Easter(year, dat)

! ident_30="@(#) M_time easter(3f) calculate date for Easter given a year"

integer,intent(in)    :: year
integer,intent(out)   :: dat(8) ! year,month,day,tz,hour,minute,second,millisecond
integer               :: day, month
integer               :: c, i, j, k, l, n

   c = year / 100
   n = year - 19 * ( year / 19 )
   k = ( c - 17 ) / 25
   i = c - c / 4 - ( c - k ) / 3 + 19 * n + 15
   i = i - 30 * ( i / 30 )
   i = i - (i / 28) * (1 - (i / 28) * (29 / (i + 1 )) * ( (21 - n) / 11) )
   j = year + year / 4 + i + 2 - c + c / 4
   j = j - 7 * ( j / 7 )
   l = i - j
   month = 3 + ( l + 40 ) / 44
   day = l + 28 - 31 * ( month / 4 )

   ! fill out a date_and_time array
   dat=getnow() ! get current year
   dat=[year,month,day,dat(4),12,0,0,0]

end subroutine Easter
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!
!   XXXX
!  X    X
! X
! X
! X
! X
! X
!  X    X
!   XXXX
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    system_sleep(3f) - [M_time:C_INTERFACE] call C sleep(3c) or usleep(3c)
!!    procedure
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    subroutine system_sleep(wait_seconds)
!!
!!       integer,intent(in)  :: wait_seconds
!!          or
!!       real,intent(in)  :: wait_seconds
!!
!!##DESCRIPTION
!!   The system_sleep(3f) routine uses the intrinsic ISO_C_BINDING
!!   interface to call the C sleep(3c) procedure or usleep(3c)
!!   routine.
!!
!!##OPTIONS
!!    wait_seconds  integer,real or doubleprecision number of seconds for
!!                  process to sleep.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_system_sleep
!!     use M_time, only : system_sleep, now
!!     implicit none
!!     integer :: i
!!        !
!!        write(*,'(a)')"Time before integer call is: ",now()
!!        call system_sleep(4)
!!        write(*,'(a)')"Time after integer call is: ",now()
!!        !
!!        write(*,'(a)')"Time before real call is: ",now()
!!        call system_sleep(4.0)
!!        write(*,'(a)')"Time after real call is: ",now()
!!        !
!!        write(*,'(a)')"Time before loop is: ",now()
!!        do i=1,1000
!!           call system_sleep(4.0/1000.0)
!!        enddo
!!        write(*,'(a)')"Time after loop is: ",now()
!!        !
!!     end program demo_system_sleep
!!
!!  results
!!
!!      Time before integer call is:
!!      Sunday, July 17th, 2016 2:29:45 AM UTC-0240
!!      Time after integer call is:
!!      Sunday, July 17th, 2016 2:29:49 AM UTC-0240
!!      Time before real call is:
!!      Sunday, July 17th, 2016 2:29:49 AM UTC-0240
!!      Time after real call is:
!!      Sunday, July 17th, 2016 2:29:53 AM UTC-0240
!!      Time before loop is:
!!      Sunday, July 17th, 2016 2:29:53 AM UTC-0240
!!      Time after loop is:
!!      Sunday, July 17th, 2016 2:30:09 AM UTC-0240
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!
!!##LICENSE
!!    MIT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine system_sleep(seconds)
use,intrinsic                 :: iso_c_binding, only: c_int

! ident_31="@(#) M_time system_sleep(3f) call sleep(3c) or usleep(3c)"

class(*),intent(in)           :: seconds
integer(kind=c_int)           :: cint
   select type(seconds)
   type is (integer);             cint=seconds                    ; call call_sleep(cint)
   type is (real);                cint=nint(seconds*1000000.0_dp) ; call call_usleep(cint)
   type is (real(kind=realtime)); cint=nint(seconds*1000000.0_dp) ; call call_usleep(cint)
   end select
end SUBROUTINE system_sleep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine call_sleep(wait_seconds)
use,intrinsic                   :: iso_c_binding, only: c_int

! ident_32="@(#) M_time call_sleep(3fp) call sleep(3c)"

integer(kind=c_int),intent(in)  :: wait_seconds
integer(kind=c_int)             :: how_long
interface
   function c_sleep(seconds) bind (C,name="sleep")
      import
      integer(c_int)       :: c_sleep ! should be unsigned int (not available in Fortran). OK until highest bit gets set.
      integer(c_int), intent(in), VALUE :: seconds
   end function c_sleep
end interface
   if(wait_seconds>0)then
      how_long=c_sleep(wait_seconds)
   endif
end subroutine call_sleep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine call_usleep(milliseconds)

! ident_33="@(#) M_time call_usleep(3fp) call usleep(3c)"

use,intrinsic                   :: iso_c_binding, only: c_int
integer(kind=c_int),intent(in)  :: milliseconds
integer(kind=c_int)             :: status
interface
   function c_usleep(mseconds) bind (C,name="usleep")
      import
      integer(c_int)       :: c_usleep ! should be unsigned int (not available in Fortran). OK until highest bit gets set.
      integer(c_int), intent(in), VALUE :: mseconds
   end function c_usleep
end interface
   if(milliseconds>0)then
      status=c_usleep(milliseconds)
   endif
end subroutine call_usleep
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
function getnow() result(dat)

! ident_34="@(#) M_time getnow(3f) get DAT for current time or value of SOURCE_DATE_EPOCH"

integer :: dat(8)
   call date_and_time(values=dat)
   ! VALUES : An array of at least eight elements. If there is no data
   ! available for a value it is set to -HUGE(VALUES). Otherwise, it contains:
   !    â€¢  VALUES(1) : The year, including the century.
   !    â€¢  VALUES(2) : The month of the year
   !    â€¢  VALUES(3) : The day of the month
   !    â€¢  VALUES(4) : Time difference in minutes between the reported time and UTC time.
   !    â€¢  VALUES(5) : The hour of the day, in the range 0 to 23.
   !    â€¢  VALUES(6) : The minutes of the hour, in the range 0 to 59
   !    â€¢  VALUES(7) : The seconds of the minute, in the range 0 to 60
   !    â€¢  VALUES(8) : The milliseconds of the second, in the range 0 to 999.
   if(any(dat == -huge(0)))then
      write(stderr,"('<ERROR>*getnow*: date_and_time(3f) contains unsupported values')")
      stop 3
   endif
end function getnow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_time
! M_time calls now_ex as a regular procedure t)o prevent a dependency loop
function now_ex(format)
use M_time, only: now

! ident_35="@(#) M_time now_ex(3f) use of now(3f) outside of a module"

character(len=*),intent(in),optional :: format
character(len=:),allocatable         :: now_ex
   now_ex=now(format)
end function now_ex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
