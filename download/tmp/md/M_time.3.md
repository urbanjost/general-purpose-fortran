NAME
    M_time - [M_time]Fortran module for manipulating and presenting time and date values
DESCRIPTION

    The M_time(3f) Fortran module and associated utility programs provide date and time related procedures. Both a procedural and
    OOP (Object Oriented Programming) interface are provided. Each routine is accompanied by a man(1) page which includes a sample
    program for that procedure. This manual, the source and example utility programs are included in the download.

    Utility programs that use the M_time(3f) module:

      + now(1) prints a date in many formats
      + ttee(1) a filter which timestamps stdout from another command
      + month(1) which lets you print simple calendars
      + days2sec(1) converts dd-hh:mm:ss to seconds
      + sec2days(1) converts seconds to dd-hh:mm:ss
      + paws(1) pause until specified time or for specified interval

    The M_time(3f) module

      + provides for formatting dates.
      + facilitates simple computations using time and date values in the recent era.
      + allow for macro-level timing of code.

    The M_TIME(3f) module complements the DATE_AND_TIME(3f) procedure, which is the standard intrinsic subroutine that returns the
    current date and time in the Gregorian calendar. That is, the primary way this module represents dates is as an integer array
    with the same meaning for elements as defined by the DATE_AND_TIME(3f) routine. In addition it can calculate or read many other
    date representations such as ...

      + Julian Dates
      + Unix Epoch Dates
      + High-level date formatting
      + Ordinal days of the year
      + days of the week
      + ISO-8601 week numbers
      + month and weekday names

    Julian and Unix Epoch Dates are particulary useful for manipulating dates in simple numeric expressions.

    The extensive formatting options include showing SYSTEM_CLOCK(3f) and CPU_USAGE(3f) information along with Gregorian date
    information, allowing for the easy incorporation of timing information into program messages. In addition to conventional
    Civilian Calendar dates, the module supports the ISO-8601 standard methods of displaying dates.

    A Fortran-callable sleep(3c)/usleep(3c) procedure is also provided.

SYNOPSIS
    +-----------------------------------------------------------------------------------------------------------------------------+
    |                                                         UNIX EPOCH                                                          |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |date_to_unix(dat,UNIXTIME,IERR)             |%epoch()        |Convert date array to Unix Time                                |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |unix_to_date(unixtime,DAT,IERR)             |                |Convert Unix Time to date array                                |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |d2u(dat) result (UNIXTIME)                  |                |Convert date array to Unix Time                                |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |u2d(unixtime) result (DAT)                  |                |Convert Unix Time to date array                                |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                           JULIAN                                                            |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |julian_to_date(julian,DAT,IERR)             |                |Convert Julian Date to date array                              |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |date_to_julian(dat,JULIAN,IERR)             |%julian()       |Converts date array to Julian Date                             |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |d2j(dat) result (JULIAN)                    |                |Convert date array to Julian Date                              |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |j2d(julian) result (DAT)                    |                |Convert Julian Date to date array                              |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                         DAY OF WEEK                                                         |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |dow(dat,[WEEKDAY],[DAY],IERR)               |%weekday()      |Convert date array to day of the week as number(Mon=1) and name|
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                        WEEK OF YEAR                                                         |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |d2w                                         |                |calculate iso-8601 Week-numbering year date yyyy-Www-d         |
    |(dat,ISO_YEAR,ISO_WEEK,ISO_WEEKDAY,ISO_NAME)|                |                                                               |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |w2d(iso_year,iso_week,iso_weekday,DAT)      |                |calculate date given iso-8601 Week date yyyy-Www-d             |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                         ORDINAL DAY                                                         |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |d2o(dat) result(ORDINAL)                    |%ordinal()      |given date array return ordinal day of year, Jan 1st=1         |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |o2d(ordinal,[year]) result(DAT)             |                |given ordinal day of year return date array, Jan 1st=1         |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |ordinal_to_date(ordinal,year,DAT)           |                |given ordinal day of year return date array, Jan 1st=1         |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                       PRINTING DATES                                                        |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |fmtdate(dat,format) result (TIMESTR)        |%format         |Convert date array to string using format                      |
    |                                            |([STRING])      |                                                               |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |fmtdate_usage(indent)                       |                |display macros recognized by fmtdate(3f)                       |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |now(format) result (NOW)                    |                |return string representing current time given format           |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |box_month(dat,CALEN)                        |                |print specified month into character array                     |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                         MONTH NAME                                                          |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |mo2v(month_name) result (MONTH_NUMBER)      |                |given month name return month number                           |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |v2mo(month_number) result (MONTH_NAME)      |                |given month number return month name                           |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |mo2d(month_name) result (DAT)               |                |return date array for first day of given month name in current |
    |                                            |                |year                                                           |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                           EASTER                                                            |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |easter(year,month,day)                      |                |calculate month and day Easter falls on for given year         |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                          DURATION                                                           |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |sec2days(seconds) result(dhms)              |                |converts seconds to string D-HH:MM:SS                          |
    |--------------------------------------------+----------------+---------------------------------------------------------------|
    |days2sec(string) result(seconds)            |                |converts string D-HH:MM:SS to seconds                          |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                        READING DATES                                                        |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |guessdate(anot,dat)                         |                |Converts a date string to a date array, in various formats     |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |                                                         C INTERFACE                                                         |
    |-----------------------------------------------------------------------------------------------------------------------------|
    |system_sleep(wait_seconds)                  |                |Call sleep(3c) or usleep(3c)                                   |
    +-----------------------------------------------------------------------------------------------------------------------------+
FORMATTING OPTIONS IN FMTDATE

    You can easily use Julian Ephemeris Dates and Unix Epoch Times to add and subtract times from dates or to calculate the
    interval between dates. But JEDs and UETs and even the Gregorian Calendar arrays in the DAT arrays are not the way we typically
    describe a date on the Civilian Calendar. So the fmtdate(3f) routine lets us print a DAT array in a variety of familiar styles.

    The fmtdate() and now() procedures let you display a Gregorian date using either keywords for standard formats or using macros
    in a user-specified formatting string. A formatting string may contain the following macros:

       Description                                        Example

        Base time array:
         (1) %Y -- year, yyyy                                2016
         (2) %M -- month of year, 01 to 12                   07
         (3) %D -- day of month, 01 to 31                    27
             %d -- day of month, with suffix (1st, 2nd,...)  27th
         (4) %Z -- minutes from UTC                          -0240
             %z -- -+hh:mm from UTC                          -04:00
         (5) %h -- hours, 00 to 23                           21
             %H -- hour (1 to 12, or twelve-hour clock)      09
             %N -- midnight< AM <=noon; noon<= PM <midnight  PM
         (6) %m -- minutes, 00 to 59                         24
         (7) %s -- sec, 00 to 59                             22
         (8) %x -- milliseconds 000 to 999                   512
        Conversions:
             %E -- Unix Epoch time                           1469669062.5129952
             %e -- integer value of Unix Epoch time          1469669063
             %J -- Julian  date                              2457597.559
             %j -- integer value of Julian Date(Julian Day)  2457597
             %O -- Ordinal day (day of year)                 209
             %U -- day of week, 1..7 Sunday=1                4
             %u -- day of week, 1..7 Monday=1                3
             %i -- ISO week of year 1..53                    30
             %I -- iso-8601 week-numbering date(yyyy-Www-d)  2016-W30-3
         Names:
             %l -- abbreviated month name                    Jul
             %L -- full month name                           July
             %w -- first three characters of weekday         Wed
             %W -- weekday name                              Wednesday
             %p -- phase of moon                             New
             %P -- percent of way from new to full moon      -1%
         Literals:
             %% -- a literal %                               %
             %t -- tab character
             %b -- blank character
             %n -- new line (system dependent)
             %q -- single quote (apostrophe)
             %Q -- double quote
         Program timing:
             %c -- CPU_TIME(3f) output                       .78125000000000000E-001
             %C -- number of times this routine is used      1
             %S -- seconds since last use of this format     .0000000000000000
             %k -- time in seconds from SYSTEM_CLOCK(3f)     588272.750

       If no percent (%) is found in the format one of several
       alternate substitutions occurs.

       If the format is composed entirely of one of the following
       keywords the following substitution occurs:

         "iso-8601",
         "iso"        ==> %Y-%M-%DT%h:%m:%s%Z
         "iso-8601W",
         "isoweek"    ==> %I
         "sql"        ==> "%Y-%M-%D %h:%m:%s.%x"
         "sqlday"     ==> "%Y-%M-%D"
         "sqltime"    ==> "%h:%m:%s.%x"
         "rfc-2822"   ==> %w, %D %l %Y %h:%m:%s %Z
         "rfc-3339"   ==> %Y-%M-%DT%h:%m:%s%Z
         "date"       ==> %w %l %D %h:%m:%s UTC%z %Y
         "short"      ==> %w, %l %d, %Y %H:%m:%s %N UTC%z
         "long"," "   ==> %W, %L %d, %Y %H:%m:%s %N UTC%z
       otherwise the following words are replaced with the most
       common macros:

          STRING   MACRO  EXAMPLE
          year     %Y     2016
          month    %M     07
          day      %D     27
          hour     %h     21
          minute   %m     24
          second   %s     22
          epoch    %e     1469669063
          julian   %j     2457597
          ordinal  %O     209
          weekday  %u     3

       if none of these keywords are found then every letter that
       is a macro is assumed to have an implied percent in front
       of it. For example:

          YMDhms ==> %Y%M%D%h%m%s ==> 20160727212422

OOPS INTERFACE

    If you prefer an Object-oriented interface the M_time_oop module (included with the M_time module source) provides an OOP
    interface to the M_time module; as described in the subroutine OBJECT_ORIENTED() in the example section.

EXAMPLES

    The following example program demonstrates the extensive options available for formatting a date as well as how to use the
    module to calculate dates such as "Yesterday" and "Tomorrow", as well as how to use the Object Oriented interface to the
    conventional procedures found in the M_time(3fm) module.

    program testit
       call procedural()
       call object_oriented()
    !===============================================================================
    contains
    !===============================================================================
    subroutine procedural()
    use M_time, only:  j2d, d2j, u2d, d2u, fmtdate, realtime
    integer                      :: dat(8)
    real(kind=realtime)          :: julian, unixtime
    character(len=*),parameter   :: iso_fmt='%Y-%M-%DT%h:%m:%s.%x%Z'
    character(len=:),allocatable :: friendly

       friendly='%W, %L %d, %Y %H:%m:%s %N' ! a nice friendly format

       call date_and_time(values=dat)  ! current time is placed in array

       write(*,*)'Today'
       write(*,*)'ISO       ',fmtdate(dat,iso_fmt)
       write(*,*)'Friendly  ',fmtdate(dat,friendly)
       write(*,*)'ISO week  ',fmtdate(dat,'%I')

       julian=d2j(dat)
       unixtime=d2u(dat)

       write(*,*)'Yesterday' ! subtract a day from scalar time and print
       write(*,*)'          ',fmtdate(u2d(unixtime-86400),iso_fmt)
       write(*,*)'          ',fmtdate(j2d(julian-1.0),friendly)
       write(*,*)'          ',fmtdate(j2d(julian-1.0),'%I')

       write(*,*)'Tomorrow'  ! add a day to scalar time and print
       write(*,*)'          ',fmtdate(u2d(unixtime+86400),iso_fmt)
       write(*,*)'          ',fmtdate(j2d(julian+1.0),friendly)
       write(*,*)'          ',fmtdate(j2d(julian+1.0),'%I')

       write(*,*)'Next Week'  ! add a week to scalar time and print
       write(*,*)'          ',fmtdate(u2d(unixtime+7*86400),iso_fmt)
       write(*,*)'          ',fmtdate(j2d(julian+7.0),friendly)
       write(*,*)'          ',fmtdate(j2d(julian+7.0),'%I')

    end subroutine procedural
    !=========================================================================================
    subroutine object_oriented()
    !
    ! This is an example using the object-oriented class/type model
    ! This is essentially the same functionality as the procedures
    ! described above, but if you prefer this type of syntax this may
    ! seem more intuitive ...
    !
    use M_time_oop,only : date_time
    !!use M_time_oop,only : operator(+),operator(-),operator(>),operator(<)
    !!use M_time_oop,only : operator(<=),operator(>=),operator(==),operator(/=)
    implicit none
    integer         :: dat(8)
    TYPE(date_time) :: event
    TYPE(date_time) :: otherdate
    TYPE(date_time) :: answer

    character(len=*),parameter   :: iso_fmt='%Y-%M-%DT%h:%m:%s.%x%Z'
       ! DIFFERENT INITIALIZATION STYLES (Still debating on how best to do this)
       write(*,*)
       write(*,*)'Various initialization styes'

       ! DEFINE TYPE(DATE_TIME) WITH CONSTRUCTOR
       otherdate=date_time()
       print *,'DEFAULT CONSTRUCTOR %FORMAT()               ',otherdate%format()
       print *,'DEFAULT CONSTRUCTOR %FORMAT("")             ',otherdate%format("")
       print *,'DEFAULT CONSTRUCTOR %FORMAT(user-specified) ',otherdate%format(iso_fmt)
       print *,'DEFAULT CONSTRUCTOR %FORMAT("USA")          ',otherdate%format("USA")

       otherdate=date_time(1492,10,12,0,0,0,0,0)
       print *,'DEFAULT CONSTRUCTOR SETTING VALUES          ',otherdate%format()

       otherdate=date_time(2016,6,11)
       print *,'DEFAULT CONSTRUCTOR WITH PARTIAL VALUES     ',otherdate%format()

       otherdate=date_time(year=2016,month=6,day=11,tz=-240,hour=21,minute=09,second=11,millisecond=500)
       print *,'DEFAULT CONSTRUCTOR WITH VALUES BY NAME     ',otherdate%format()

       otherdate=date_time([1776,7,4,0,0,0,0,0])
       print *,'CONSTRUCTOR WITH A DAT ARRAY                ',otherdate%format()

       otherdate=date_time([1776,7,4])
       print *,'CONSTRUCTOR WITH A PARTIAL DAT ARRAY        ',otherdate%format()

       ! the init() method supports several methods
       call otherdate%init()                           ! initialize to current time using INIT
       call otherdate%init(type="now")                 ! initialize to current time using INIT

       call otherdate%init(type="epoch")               ! initialize to beginning of Unix Epoch Time
       ! Note
       ! currently, DATE_TIME DATE array is set to Unix Epoch start USING LOCAL TIMEZONE
       ! whereas default constructor is using default of Unix Epoch start using Z time (GMT or UTC time)

       ! initialize with a DAT array using INIT, compatible with DATE_AND_TIME VALUES(8)
       call otherdate%init(dat=[1970,1,1,0,0,0,0,0])
       call otherdate%init(2016,6,11,-300,23,1,0,0)    ! using INIT with ordered values
       ! using INIT with names
       call otherdate%init(year=2016,month=6,day=11,tz=-300,hour=23,minute=1,second=0,millisecond=0)
       !============================================================================
       ! take current date and exercise the OOP interface
       call event%init()                                           ! initialize to current time using INIT
       write(*,*)
       write(*,*)'Print members of type(DATE_TIME)'
       write(*,404)'EVENT=',event                                  ! show derived type
       404 format(a,i0,*(",",i0:))

       ! MEMBERS ( basic time values are all integers)
       write(*,101)'%year        Year................... ',event%year           ! print members of type
       write(*,101)'%month       Month.................. ',event%month
       write(*,101)'%day         Day.................... ',event%day
       write(*,101)'%tz          Timezone............... ',event%tz
       write(*,101)'%hour        Hour................... ',event%hour
       write(*,101)'%minute      Minute................. ',event%minute
       write(*,101)'%second      Second................. ',event%second
       write(*,101)'%millisecond Millisecond............ ',event%millisecond

       ! PRINT METHODS OF TYPE
       write(*,*)'Print methods of type(DATE_TIME)'
       write(*,101)'%ordinal     Ordinal day of year.... ',  event%ordinal()
       write(*,101)'%weekday     Weekday................ ',  event%weekday()
       101 format(1x,a,i0)
       ! DOUBLE PRECISION VALUES EASILY MANIPULATED MATHEMATICALLY
       write(*,202)'%epoch      Unix epoch time........ ',  event%epoch()
       write(*,202)'%julian     Julian date............ ',  event%julian()
       202 format(1x,a,g0)

       ! FORMATTED STRINGS (many strings possible. Takes the same format string as fmtdate(3f))
       write(*,*)
       write(*,*)'Formatted Strings (%format("STRING") -- see fmtdate(3f) for format descriptions'
       write(*,303)'Short month............ ',event%format("%l")  ! abbreviated month name             %l  Dec
       write(*,303)'Month.................. ',event%format("%L")  ! full month name                    %L  December
       write(*,303)'Short week............. ',event%format("%w")  ! first three characters of weekday  %w  Sat
       write(*,303)'Week .................. ',event%format("%W")  ! weekday name                       %W  Saturday
       ! with no percent (%) characters
       write(*,303)'Calendar Time ......... ',event%format("Y-M-D h:m:s.x z")
       ! keywords with no percent (%) characters
       write(*,303)'Calendar Time ......... ',event%format('"year-month-day hour:minute:second.millisecond timezone"')
       write(*,*)event%format('Longer format.......... "%W, %L %d, %Y %H:%m:%s %N"') ! a nice friendly format
       303 format(1x,a,'"',a,'"')

       dat=event%datout()            ! convert date_time to integer array (maybe to use with module M_TIME base procedures)
       write(*,*)
       write(*,404)'DAT=',dat

       ! OVERLOADED OPERATORS (add and subtract)
       answer=event+1*86400.0d0   ! a date_time object can have seconds added
       write(*,*)answer%format('TOMORROW="%W, %L %d, %Y %H:%m:%s %N"') ! a nice friendly format
       answer=event-1*86400.0d0   ! a date_time object can have seconds subtracted
       write(*,*)answer%format('YESTERDAY=="%W, %L %d, %Y %H:%m:%s %N"') ! a nice friendly format
       ! if both operands are DATE_TIME objects a subtraction finds the time in seconds between the two dates
       write(*,*)'DIFFERENCE (subtracting one date_time from another)=',answer-event

       ! OVERLOADED OPERATORS (logical comparisons)
       ! NOTE COMPARISONS ARE PERFORMED BY CONVERTING TIMES TO INTEGER SECONDS
       write(*,*)event.eq.event   ,event.lt.event   ,event.gt.event   ,event.le.event   ,event.ge.event   ,event.ne.event
       write(*,*)event.eq.answer  ,event.lt.answer  ,event.gt.answer  ,event.le.answer  ,event.ge.answer  ,event.ne.answer
       write(*,*)answer.eq.event  ,answer.lt.event  ,answer.gt.event  ,answer.le.event  ,answer.ge.event  ,answer.ne.event

       ! %DELTA easily lets you change dates by common increments
       write(*,*)
       write(*,404)'%DELTA tests starting with date ',event%delta()
       write(*,*) event%format("                             %W, %L %d, %Y %H:%m:%s %N")

       write(*,*)'Remember years and months are not constant units'

       answer=event%delta(year=1)
       write(*,*)answer%format("FOR %%DELTA(YEAR=+1)            %W, %L %d, %Y %H:%m:%s %N")
       answer=event%delta(year=-1)
       write(*,*)answer%format("FOR %%DELTA(YEAR=-1)            %W, %L %d, %Y %H:%m:%s %N")

       answer=event%delta(month=24)
       write(*,*)answer%format("FOR %%DELTA(MONTH=+24)          %W, %L %d, %Y %H:%m:%s %N")
       answer=event%delta(month=-24)
       write(*,*)answer%format("FOR %%DELTA(MONTH=-24)          %W, %L %d, %Y %H:%m:%s %N")

       answer=event%delta(week=1)
       write(*,*)answer%format("FOR %%DELTA(WEEK=+1)            %W, %L %d, %Y %H:%m:%s %N")
       answer=event%delta(week=-1)
       write(*,*)answer%format("FOR %%DELTA(WEEK=-1)            %W, %L %d, %Y %H:%m:%s %N")

       answer=event%delta(day=1)
       write(*,*)answer%format("FOR %%DELTA(DAY=+1)             %W, %L %d, %Y %H:%m:%s %N")
       answer=event%delta(day=-1)
       write(*,*)answer%format("FOR %%DELTA(DAY=-1)             %W, %L %d, %Y %H:%m:%s %N")

       answer=event%delta(hour=4)
       write(*,*)answer%format("FOR %%DELTA(HOUR=+4)            %W, %L %d, %Y %H:%m:%s %N")
       answer=event%delta(hour=-4)
       write(*,*)answer%format("FOR %%DELTA(HOUR=-4)            %W, %L %d, %Y %H:%m:%s %N")

       answer=event%delta(minute=180)
       write(*,*)answer%format("FOR %%DELTA(MINUTE=+180)        %W, %L %d, %Y %H:%m:%s %N")
       answer=event%delta(minute=-180)
       write(*,*)answer%format("FOR %%DELTA(MINUTE=-180)        %W, %L %d, %Y %H:%m:%s %N")

       answer=event%delta(second=1800)
       write(*,*)answer%format("FOR %%DELTA(SECOND=+1800)       %W, %L %d, %Y %H:%m:%s %N")
       answer=event%delta(second=-1800)
       write(*,*)answer%format("FOR %%DELTA(SECOND=-1800)       %W, %L %d, %Y %H:%m:%s %N")

       answer=event%delta(millisecond=10000)
       write(*,*)answer%format("FOR %%DELTA(MILLISECOND=+10000) %W, %L %d, %Y %H:%m:%s %N")
       answer=event%delta(millisecond=-10000)
       write(*,*)answer%format("FOR %%DELTA(MILLISECOND=-10000) %W, %L %d, %Y %H:%m:%s %N")

       answer=event%delta(year=3,month=2,day=100,hour=200,week=-1,minute=300,second=1000,millisecond=-10000)
       write(*,*)answer%format(&
       &"FOR %%DELTA(year=3,month=2,day=100,hour=200,week=-1,minute=300,second=1000,millisecond=100000)  %W, %L %d, %Y %H:%m:%s %N")

       answer=event%delta(duration="1-20:30:40.50")
       write(*,*)answer%format("FOR %%DELTA(DURATION='1-20:30:40.50')      %W, %L %d, %Y %H:%m:%s %N")

    end subroutine object_oriented
    end program testit

    Sample output of example program ...

    The example from the conventional calls looks like this ...

     Today
     ISO       2015-12-22T08:07:34.025-0300
     Friendly  Tuesday, December 22nd, 2015 08:07:34 AM
     ISO week  2015-W52-2
     Yesterday
               2015-12-21T08:07:34.025-0300
               Monday, December 21st, 2015 08:07:34 AM
               2015-W52-1
     Tomorrow
               2015-12-23T08:07:34.025-0300
               Wednesday, December 23rd, 2015 08:07:34 AM
               2015-W52-3
     Next Week
               2015-12-29T08:07:34.025-0300
               Tuesday, December 29th, 2015 08:07:34 AM
               2015-W53-2

    The example from the object-oriented calls looks like this ...

     Various initialization styes
     DEFAULT CONSTRUCTOR %FORMAT()               1970-01-01T00:00:00.000+0000
     DEFAULT CONSTRUCTOR %FORMAT("")             1970-01-01T00:00:00.000+0000
     DEFAULT CONSTRUCTOR %FORMAT(user-specified) 1970-01-01T00:00:00.000+0000
     DEFAULT CONSTRUCTOR %FORMAT("USA")          Thursday, January 1st, 1970 12:00:00 AM
     DEFAULT CONSTRUCTOR SETTING VALUES          1492-10-12T00:00:00.000+0000
     DEFAULT CONSTRUCTOR WITH PARTIAL VALUES     2016-06-11T00:00:00.000+0000
     DEFAULT CONSTRUCTOR WITH VALUES BY NAME     2016-06-11T21:09:11.500-0240
     CONSTRUCTOR WITH A DAT ARRAY                1776-07-04T00:00:00.000+0000
     CONSTRUCTOR WITH A PARTIAL DAT ARRAY        1776-07-04T20:00:00.000-0240

     Print members of type(DATE_TIME)
    EVENT=2016,6,14,-240,22,22,31,253
     Year................... 2016
     Month.................. 6
     Day.................... 14
     Timezone............... -240
     Hour................... 22
     Minute................. 22
     Second................. 31
     Millisecond............ 253
     Print methods of type(DATE_TIME)
     Ordinal day of year.... 166
     Weekday................ 3
     Unix epoch time........ 1465957351.2529941
     Julian date............ 2457554.5989728356

     Formatted Strings
     Short month............ "Jun"
     Month.................. "June"
     Short week............. "Tue"
     Week .................. "Tuesday"
     Longer format.......... "Tuesday, June 14th, 2016 10:22:31 PM"

    DAT=2016,6,14,-240,22,22,31,253
     TOMORROW="Wednesday, June 15th, 2016 10:22:31 PM"
     YESTERDAY=="Wednesday, June 13th, 2016 10:22:31 PM"
     DIFFERENCE (subtracting one date_time from another)=   86400.000000000000
     T F F T T F
     F T F T F T
     F F T F T T

    %DELTA tests starting with date 2016,6,14,-240,22,22,31,253
                                  Tuesday, June 14th, 2016 10:22:31 PM
     Remember years and months are not constant units
     FOR DELTA YEAR=+1            Wednesday, June 14th, 2017 10:22:31 PM
     FOR DELTA YEAR=-1            Sunday, June 14th, 2015 10:22:31 PM
     FOR DELTA MONTH=+24          Saturday, June 16th, 2018 10:22:31 PM
     FOR DELTA MONTH=-24          Saturday, June 14th, 2014 10:22:31 PM
     FOR DELTA WEEK=+1            Tuesday, June 21st, 2016 10:22:31 PM
     FOR DELTA WEEK=-1            Tuesday, June 7th, 2016 10:22:31 PM
     FOR DELTA DAY=+1             Wednesday, June 15th, 2016 10:22:31 PM
     FOR DELTA DAY=+1             Monday, June 13th, 2016 10:22:31 PM
     FOR DELTA HOUR=+4            Wednesday, June 15th, 2016 02:22:31 AM
     FOR DELTA HOUR=-4            Tuesday, June 14th, 2016 06:22:31 PM
     FOR DELTA MINUTE=+180        Wednesday, June 15th, 2016 01:22:31 AM
     FOR DELTA MINUTE=-180        Tuesday, June 14th, 2016 07:22:31 PM
     FOR DELTA SECOND=+1800       Tuesday, June 14th, 2016 10:52:31 PM
     FOR DELTA SECOND=-1800       Tuesday, June 14th, 2016 09:52:31 PM
     FOR DELTA MILLISECOND=+10000 Tuesday, June 14th, 2016 10:22:41 PM
     FOR DELTA MILLISECOND=-10000 Tuesday, June 14th, 2016 10:22:21 PM
     FOR DELTA ONE-OF-EACH        Sunday, November 24th, 2019 11:39:01 AM

DEFINITIONS

    A "date_and_time" array "DAT" has the same format as the array of values generated by the Fortran intrinsic DATE_AND_TIME(3f).
    That is, it is an 8-element integer array containing year, month, day, Time zone difference from UTC in minutes, hour, minutes,
    seconds, and milliseconds of the second. This array represents a date on the Proleptic Gregorian Calendar.

    The Proleptic Gregorian Calendar assumes the Gregorian Calendar existed back to the beginning of the Julian Day calendar (4713
    BC). This means historic dates will often be confused, as the Julian Calendar was used in the USA until 1752-09-03, for
    example. The Gregorian Calendar was formally decreed on 1582-10-15 but was not adapted in many countries. The Julian Calendar
    was first used around 45 BC. Note that the Proleptic Gregorian Calendar includes a year zero (0). It is frequently used in
    computer software to simplify the handling of older dates. For example, it is the calendar used by MySQL, SQLite, PHP, CIM,
    Delphi, Python and COBOL. The Proleptic Gregorian Calendar is explicitly required for all dates before 1582 by ISO 8601:2004
    (clause 4.3.2.1 The Gregorian calendar) if the partners to information exchange agree.

    Unix Epoch Time (UET) is defined as the number of seconds since 00:00:00 on January 1st. 1970, UTC.

    A JED is defined as a Julian Ephemeris Date. JED days start at noon (not at midnight). 4713-01-01 BC at noon is defined as JED
    0.0.

    If you are not familiar with them, in this context Julian Dates and Unix Epoch Times are scalar numbers that allow for easy
    computations using dates (to go back one day just subtract one from a Julian Date, for example). Since these values are
    generally not considered intelligible, routines are included to convert between these scalar values and the date array so
    human-readable results can be obtained.

    Coordinated Universal Time (French: Temps universel coordonn' e), abbreviated as UTC, is the primary time standard by which the
    world regulates clocks and time. It is within about 1 second of mean solar time at 0o longitude;[1] it does not observe
    daylight saving time. It is one of several closely related successors to Greenwich Mean Time (GMT). For most purposes, UTC is
    considered interchangeable with GMT, but GMT is no longer precisely defined by the scientific community.

LIMITATIONS

    Like most collections of date and time procedures M_time is not a high-precision library that accounts internally for leap
    seconds and relativistic effects.

    M_time(3f) is intended for use in the recent era and is not appropriate for use with historical dates that used some other
    calendar scheme such as the Julian Calendar. That is, you have to remember to account for conversions to other calendar systems
    when using historical dates.

    When Daylight Savings is in effect calculations will generally be correct, as the date model includes a timezone value; but you
    are responsible for ensuring dates you create use the correct timezone value or otherwise account for Daylight Savings Time as
    needed.

    Currently, dates are manipulated using the current system timezone, which can typically be set using the environment variable
    TZ. So if you desire to set the default timezone you generally set the environment variable before executing your program. This
    is compatible with current observed behavior for the intrinsic procedure DATE_AND_TIME(3f) with compilers I have tested with,
    but does not seem to be a specified behavior as far as the standard is concerned. That is, DATE_AND_TIME(3f) returns a vector
    that contains a current time zone, but does not specify how a current time zone can be explicitly set. Since this library is
    intentionally designed to complement DATE_AND_TIME(3f) it adopts the same behavior. A routine to let you set a default time
    zone could be added in the future.

    There is no warranty on this code, and it is certain to change.

SEE ALSO

    The ISO-8601 standard is often used for business-related transactions.

    There are (of course) the C/C++ intrinsics which provide much of the same functionality that should be bindable to Fortran via
    the ISO_C_BINDING module.

    If you are looking for a high-precision Fortran library that is well tested for manipulating dates I would suggest looking at
    the NASA SPICElib library. If you care about Leap Seconds, Orbital Mechanics, GPS/Satellite communications, and Astronomy it is
    worth a look.

    The Fortran Wiki fortranwiki.org contains information on other libraries and modules that provide date-time procedures.

