[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                 Manual Reference Pages  - w2d (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    w2d - [M_time] calculate DAT date-time array from iso-8601 Week-numbering year date yyyy-Www-d

CONTENTS

    Synopsis
    Description
    Options
    Returns
    Example
    Definition
    Method
    Reference
    Author

SYNOPSIS

    subroutine w2d(iso_year,iso_week,iso_weekday,dat)


        integer,intent(in)      :: iso_year, iso_week, iso_weekday
        integer,intent(out)     :: dat(8)     ! output date array



DESCRIPTION

    Given an ISO-8601 week return a "DAT" array defining a date and time, The ISO-8601 is supplied as three integer values defining
    the ISO year, week of year and weekday.

OPTIONS

          iso_year ISO-8601 year number for the given date

          iso_week ISO-8601 week number for the given date

          iso_weekday ISO-8601 weekday number for the given date

          iso_name ISO-8601 Week string for the data in the form "yyyy-Www-d".

RETURNS

             dat "DAT" array (an integer array of the same format as the array returned by the intrinsic DATE_AND_TIME(3f))
             describing the date to be used, which is the basic time description used by the other M_time(3fm) module procedures.

EXAMPLE

    Sample program:

        program demo_w2d
        use M_time, only : w2d, fmtdate
        implicit none


           write(*, (a) )Given Monday 29 December 2008 is written "2009-W01-1" 
           call printit(2009,1,1)
           write(*, (a) )Given Sunday 3 January 2010 is written "2009-W53-7" 
           call printit(2009,53,7)
           write(*, (a) )Given the Gregorian date Sun 31 December 2006 is written 2006-W52-7 
           call printit(2006,52,7)
           write(*, (a) )Given 27 September 2008 is 2008-W39-6 
           call printit(2008,39,6)



        contains subroutine printit(iso_year,iso_week,iso_weekday)

              integer :: iso_year, iso_week, iso_weekday ! ISO-8601 Week: 2016-W29-1

              integer :: dat(8) ! input date array

               call w2d(iso_year,iso_week,iso_weekday,dat)
               write(*, (a,i0) ) GIVEN:            
               write(*, (a,i0) ) ISO-8601 year     ,iso_year
               write(*, (a,i0) ) ISO-8601 week     ,iso_week
               write(*, (a,i0) ) ISO-8601 weekday  ,iso_weekday
               write(*, (a,i0) ) RESULT:           
               write(*, (a,*(i0:,",")) )    DAT array         ,dat
               write(*, (a,/,77("=")) )      //fmtdate(dat, long )



        end subroutine printit end program demo_w2d

    Results:

        Given Monday 29 December 2008 is written "2009-W01-1"
        GIVEN:
        ISO-8601 year    2009
        ISO-8601 week    1
        ISO-8601 weekday 1
        RESULT:
           DAT array        2008,12,29,-240,0,0,0,0
            Monday, December 29th, 2008 12:00:00 AM UTC-04:00
        =============================================================================
        Given Sunday 3 January 2010 is written "2009-W53-7"
        GIVEN:
        ISO-8601 year    2009
        ISO-8601 week    53
        ISO-8601 weekday 7
        RESULT:
           DAT array        2010,1,3,-240,0,0,0,0
            Sunday, January 3rd, 2010 12:00:00 AM UTC-04:00
        =============================================================================
        Given the Gregorian date Sun 31 December 2006 is written 2006-W52-7
        GIVEN:
        ISO-8601 year    2006
        ISO-8601 week    52
        ISO-8601 weekday 7
        RESULT:
           DAT array        2006,12,31,-240,0,0,0,0
            Sunday, December 31st, 2006 12:00:00 AM UTC-04:00
        =============================================================================
        Given 27 September 2008 is 2008-W39-6
        GIVEN:
        ISO-8601 year    2008
        ISO-8601 week    39
        ISO-8601 weekday 6
        RESULT:
           DAT array        2008,9,27,-240,0,0,0,0
            Saturday, September 27th, 2008 12:00:00 AM UTC-04:00
        =============================================================================



DEFINITION

    The ISO-8601 date and time standard was issued by the International

             Organization for Standardization (ISO). It is used (mainly) in government and business for fiscal years, as well as in
             timekeeping. The system specifies a week year atop the Gregorian calendar by defining a notation for ordinal weeks of
             the year.

    An ISO week-numbering year (also called ISO year informally) has

             52 or 53 full weeks. That is 364 or 371 days instead of the usual

             365 or 366 days. The extra week is referred to here as a leap week,

             although ISO-8601 does not use this term. Weeks start with Monday. The first week of a year is the week that contains
             the first Thursday

             of the year (and, hence, always contains 4 January). ISO week year numbering therefore slightly deviates from the
             Gregorian for some days close to January 1st.

METHOD

    Calculating a date given the year, week number and weekday

    This method requires that one know the weekday of 4 January of the year in question. Add 3 to the number of this weekday,
    giving a correction to be used for dates within this year.

    Method: Multiply the week number by 7, then add the weekday. From this sum subtract the correction for the year. The result is
    the ordinal date, which can be converted into a calendar date. If the ordinal date thus obtained is zero or negative, the date
    belongs to the previous calendar year; if greater than the number of days in the year, to the following year.

    Example: year 2008, week 39, Saturday (day 6) Correction for 2008: 5 + 3 = 8 (39 x 7) + 6 = 279 279 - 8 = 271 Ordinal day 271
    of a leap year is day 271 - 244 = 27 September Result: 27 September 2008

ISO_NAME Week date representations are in the format YYYYWww-D.

    o    [YYYY] indicates the ISO week-numbering year which is slightly different from the traditional Gregorian calendar year.

    o    [Www] is the week number prefixed by the letter W, from W01 through W53.

    o    [D] is the weekday number, from 1 through 7, beginning with Monday and ending with Sunday.

For example, the Gregorian date 31 December 2006 corresponds to the Sunday of the 52nd week of 2006, and is written

    2006-W52-7 (extended form)
    or
    2006W527 (compact form).



REFERENCE

    From Wikipedia, the free encyclopedia 2016-08-08

AUTHOR

    John S. Urban

-----------------------------------------------------------------------------------------------------------------------------------

                                                              w2d (3)                                                 July 02, 2017

Generated by manServer 1.08 from e426b83c-723c-4a64-ba3b-38eabb92d306 using man macros.
                                                               [w2d]
