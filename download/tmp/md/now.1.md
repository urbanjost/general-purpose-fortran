[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                 Manual Reference Pages  - now (1)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    now(1) - [TIME] print the date and time

CONTENTS

    Synopsis
    Description
    Options
    Examples
    Limits
    See Also

SYNOPSIS

    now [Format [-date date_str|-ued Unix_time|-jed Julian_Date|-dat date_vector]
    [-delta dd-hh:mm:ss]]|--help |--version|-test]

DESCRIPTION

    Report the current time or a Fortran date vector in a variety of formats. Julian dates, Unix Epoch time, weekdays, monthnames,
    ordinal days, AM/PM and iso-8601 week-numbering are supported by building a format string containing the desired macros.

OPTIONS

             Format This string, containing macro names or keywords, creates the format used to print the specified date.

             The FORMAT string is expanded using the following macros:

                   Description                                        Example


                   Base time array:
                    (1) %Y -- year, yyyy                                2017
                    (2) %M -- month of year, 01 to 12                   06
                    (3) %D -- day of month, 01 to 31                    30
                        %d -- day of month, with suffix (1st, 2nd,...)  30th
                    (4) %Z -- minutes from UTC                          -0240
                        %z -- -+hh:mm from UTC                          -04:00
                    (5) %h -- hours, 00 to 23                           06
                        %H -- hour (1 to 12, or twelve-hour clock)      6
                        %N -- midnight< AM <=noon; noon<= PM <midnight  AM
                    (6) %m -- minutes, 00 to 59                         32
                    (7) %s -- sec, 00 to 59                             23
                    (8) %x -- milliseconds 000 to 999                   064
                   Conversions:
                        %E -- Unix Epoch time                           1498818743.0650055
                        %e -- integer value of Unix Epoch time          1498818743
                        %J -- Julian  date                              2457934.9391558566
                        %j -- integer value of Julian Date(Julian Day)  2457934
                        %O -- Ordinal day (day of year)                 181
                        %U -- day of week, 1..7 Sunday=1                5
                        %u -- day of week, 1..7 Monday=1                5
                        %i -- ISO week of year 1..53                    26
                        %I -- iso-8601 week-numbering date(yyyy-Www-d)  2017-W26-5
                    Names:
                        %l -- abbreviated month name                    Jun
                        %L -- full month name                           June
                        %w -- first three characters of weekday         Fri
                        %W -- weekday name                              Friday
                        %p -- phase of moon                             First quarter
                        %P -- percent of way from new to full moon      41%
                    Literals:
                        %% -- a literal %                               %
                        %t -- tab character
                        %b -- blank character
                        %n -- new line (system dependent)


                        %q -- single quote (apostrophe)                  
                        %Q -- double quote                              "
                    Program timing:
                        %c -- CPU_TIME(3f) output                       .10937500000000000
                        %C -- number of times this routine is used      1
                        %S -- seconds since last use of this format     .0000000000000000
                        %k -- time in seconds from SYSTEM_CLOCK(3f)     1340080.62


                   If no percent (%) is found in the format one of several
                   alternate substitutions occurs.


                   If the format is composed entirely of one of the following
                   keywords the following substitutions occur:
                     "iso-8601",
                     "iso"        ==> %Y-%M-%DT%h:%m:%s%Z             2017-06-30T06:32:23-0240
                     "iso-8601W",
                     "isoweek"    ==> %I                              2017-W26-5
                     "sql"        ==> "%Y-%M-%D %h:%m:%s.%x"          "2017-06-30 06:32:23.162"
                     "sqlday"     ==> "%Y-%M-%D"                      "2017-06-30"
                     "sqltime"    ==> "%h:%m:%s.%x"                   "06:32:23.164"
                     "rfc-2822"   ==> %w, %D %l %Y %h:%m:%s %Z
                                      Fri, 30 Jun 2017 06:32:23 -0240
                     "rfc-3339"   ==> %Y-%M-%DT%h:%m:%s%Z             2017-06-30T06:32:23-0240
                     "date"       ==> %w %l %D %h:%m:%s UTC%z %Y
                                      Fri Jun 30 06:32:23 UTC-04:00 2017
                     "short"      ==> %w, %l %d, %Y %H:%m:%s %N UTC%z
                                      Fri, Jun 30th, 2017 6:32:23 AM UTC-04:00
                     "long"," "   ==> %W, %L %d, %Y %H:%m:%s %N UTC%z
                                      Friday, June 30th, 2017 6:32:23 AM UTC-04:00
                   otherwise the following words are replaced with the most
                   common macros:
                      year          %Y  2017
                      month         %M  06
                      day           %D  30
                      hour          %h  06
                      minute        %m  32
                      millisecond   %x  172
                      timezone      %z  -04:00
                      second        %s  23
                      epoch         %e  1498818743
                      julian        %j  2457934
                      ordinal       %O  181
                      weekday       %u  5
                   if none of these keywords are found then every letter that
                   is a macro is assumed to have an implied percent in front
                   of it. For example:
                      YMDhms ==> %Y%M%D%h%m%s ==> 20170630063223





             -dat date_vector A date vector is eight integers representing a date in the same manner as the Fortran DATE_AND_TIME
             (3f) function: yyyy mm dd zone hh mm ss mss only numeric time zones are supported.

             When present, the specified date is used instead of the current time.

             -uet Unix_Epoch_Time When present a value is used as the Unix Epoch Time. This date is is then adjusted using any
             -delta value and then printed using the specified format.

             -jed Julian_Date When present a value is used as the Julian Ephemeris Date.

             -delta dd-hh:mm:ss Add the specified duration to the date.

             -date date_str The guessdate(3f) routine is used to try to convert a date description to a date vector. For the guess
             to work, dates must either be in the form YYYY-MM-DD or the order of numeric values must be ""dd yy yyy". Only
             four-digit years are supported. Month names are preferred over numeric values. See the guessdate(3f) documentation for
             further details.

    -test : To list allowed macros use the -test switch.

    When present, the specified date is used instead of the current time.

EXAMPLES

    Sample commands:

      now
        Friday, June 17th, 2016 03:22:53 PM UTC-0240


      now -delta  1-0:0:0  # Tomorrow
        Sunday, June 19th, 2016 11:32:26 AM UTC-0240


      now -delta -1-0:0:0  # Yesterday
        Friday, June 17th, 2016 11:32:43 AM UTC-0240


      now long -delta  7-0:0:0  # Next week
        Saturday, June 25th, 2016 11:32:57 AM UTC-04:00


      now The date is %Y/%M/%D %h:%m:%s  # user-specified formats using macros
        The date is 2009/08/10 00:33:48


      now Y/M/D h:m:s # user-specified format with no % character
        2009/08/10 00:33:48


      now year-month-day # user-specified format with no % with long keywords
      2016-07-29


      now -dat 2016 07 23 -240 1 01 00 00  # alternate date
      Saturday, July 23rd, 2016 1:01:00 AM UTC-0240


      now -uet  1469250060                 # alternate Unix Epoch date
      now -date January 4th, 1999 10:20:30 # try to determine date from description.


      now YEAR=%Y MONTH=%M DAY=%D
        YEAR=2009 MONTH=08 DAY=10


      now HOUR=%h MINUTES=%m SECONDS=%s MILLISECONDS=%x
        HOUR=01 MINUTES=18 SECONDS=44 MILLISECONDS=946


      # double-quotes are trickly (double them) to put in literally in this program:
      now  ""year-month-day"",""hour-minute-second"" 
         "2017-04-23","14-41-09"


      # quotes are easier to control using the single-letter macros(use %q and %Q):
      now QY-M-DQ,Qh:m:sQ
         "2017-04-23","14-41-09"


      now -test       # Show formatting options, handy way to look up macro names



LIMITS

    See the M_time module description. Basically, A Gregorian Calendar is assumed, and Leap Seconds are not specifically accounted
    for.

SEE ALSO

    calen(1), sec2days(1), days2sec(1)

-----------------------------------------------------------------------------------------------------------------------------------

                                                              now (1)                                                 July 02, 2017

Generated by manServer 1.08 from 8d11f054-a7d3-4384-9dbe-0d36f341bf6a using man macros.
                                                               [now]
