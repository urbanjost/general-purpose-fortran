     program demo_M_constants
     use, intrinsic :: iso_fortran_env, only : stdout=>OUTPUT_UNIT
     use M_constants, only : uc, fmt, lets, calen
     implicit none
     ! give a local name to a constant:
     ! universal constant, single-precision, e
     real,parameter             :: e=uc%sp%e
     character(len=*),parameter :: all=fmt%all
     integer                    :: i

        ! or just use full name
        ! universal constant, quad-precision, gamma
        print fmt%all, 'gamma=',uc%qp%gamma
        print all, 'e=',e
        !
        ! or rename it with ASSOCIATE
        associate (gamma => uc%dp%gamma)
           print all,'gamma=',gamma
        end associate
        !
        ! string constants:
        !
        ! strings of letter sets
        print all,'lets%upper=',lets%upper
        print all,'lets%lower=',lets%lower
        print all,'lets%hexadecimal=',lets%hexadecimal
        print all,'lets%digits=',lets%digits
        !
        ! English civil calendar names
        print fmt%commas,'calen%months=',new_line('a'),(calen%months(i:i+2) &
               & ,new_line('a'),i=1,size(calen%months),3)
        print fmt%commas,'calen%mths=',calen%mths
        print fmt%commas,'calen%weekdays=',&
                & (trim(calen%weekdays(i)),i=1,size(calen%weekdays))
        print fmt%commas,'calen%wkds=',calen%wkds

     end program demo_M_constants
