[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                Manual Reference Pages  - ttee (1)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    ttee(1f) - [TIME] write input to stdout and a file with timing info.

CONTENTS

    Synopsis
    Description
    Limits
    Examples

SYNOPSIS

    ttee [[-o|--output] filename(s)] [-a|--append] [--timestamp FLAG]] ...
    [-fmt FORMAT] [--help |--version]

DESCRIPTION

    Read from standard input and write to standard output and files with an optional timestamp in front of each line.

    -o|--output FILENAME(S) specify name of output file(s). If the filenames are first the keyword -o|--output is optional.

    -a|--append append to the given output file(s), do not overwrite

    -t|--timestamp FLAG which files to add the timestamp to. Default is "all" Allowed values are stdout, output, all, none.

    -fmt FORMAT Change format for timestamp prefix using a call to now(3f).

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
               (6) %m -- minutes, 00 to 59                         34
               (7) %s -- sec, 00 to 59                             17
               (8) %x -- milliseconds 000 to 999                   978
              Conversions:
                   %E -- Unix Epoch time                           1498818857.9780011
                   %e -- integer value of Unix Epoch time          1498818858
                   %J -- Julian  date                              2457934.9404858681
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
                   %c -- CPU_TIME(3f) output                       .21875000000000000
                   %C -- number of times this routine is used      1
                   %S -- seconds since last use of this format     .0000000000000000
                   %k -- time in seconds from SYSTEM_CLOCK(3f)     1340195.50


              If no percent (%) is found in the format one of several
              alternate substitutions occurs.


              If the format is composed entirely of one of the following
              keywords the following substitutions occur:
                "iso-8601",
                "iso"        ==> %Y-%M-%DT%h:%m:%s%Z             2017-06-30T06:34:17-0240
                "iso-8601W",
                "isoweek"    ==> %I                              2017-W26-5
                "sql"        ==> "%Y-%M-%D %h:%m:%s.%x"          "2017-06-30 06:34:17.993"
                "sqlday"     ==> "%Y-%M-%D"                      "2017-06-30"
                "sqltime"    ==> "%h:%m:%s.%x"                   "06:34:17.994"
                "rfc-2822"   ==> %w, %D %l %Y %h:%m:%s %Z
                                 Fri, 30 Jun 2017 06:34:17 -0240
                "rfc-3339"   ==> %Y-%M-%DT%h:%m:%s%Z             2017-06-30T06:34:17-0240
                "date"       ==> %w %l %D %h:%m:%s UTC%z %Y
                                 Fri Jun 30 06:34:17 UTC-04:00 2017
                "short"      ==> %w, %l %d, %Y %H:%m:%s %N UTC%z
                                 Fri, Jun 30th, 2017 6:34:17 AM UTC-04:00
                "long"," "   ==> %W, %L %d, %Y %H:%m:%s %N UTC%z
                                 Friday, June 30th, 2017 6:34:17 AM UTC-04:00
              otherwise the following words are replaced with the most
              common macros:
                 year          %Y  2017
                 month         %M  06
                 day           %D  30
                 hour          %h  06
                 minute        %m  34
                 millisecond   %x  999
                 timezone      %z  -04:00
                 second        %s  17
                 epoch         %e  1498818858
                 julian        %j  2457934
                 ordinal       %O  181
                 weekday       %u  5
              if none of these keywords are found then every letter that
              is a macro is assumed to have an implied percent in front
              of it. For example:
                 YMDhms ==> %Y%M%D%h%m%s ==> 20170630063418



        --help display this help and exit

        --version output version information and exit

LIMITS

    Program limits:

          o Input line width maximum is 1024 characters.

          o Maximum length of output filenames is 4098, individual filename is 1024.

          o Minimum number of output files is probably at least 90; but is system dependent.

EXAMPLES

    Basic command usage:

        # write stdout of "program" to ttee.out with a timestamp and stdout
        program|ttee --output ttee.out --timestamp output|grep -i iteration


        # write stdout of "program" to log.txt and stdout with a Julian Day
        program|ttee log.txt -fmt "%J :"
        2457565.488 :Iteration 1 : Error: 1.20
        2457565.558 :Iteration 2 : Error: 0.08
        2467569.684 :Iteration 3 : Error: 1.2e-3



-----------------------------------------------------------------------------------------------------------------------------------

                                                             ttee (1)                                                 July 02, 2017

Generated by manServer 1.08 from 5067cab8-4608-4dd1-9414-b715eca28e8c using man macros.
                                                              [ttee]
