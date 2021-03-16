            program demo_string_to_values
             use M_strings, only : string_to_values
             implicit none
             character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
             integer,parameter  :: isz=10
             real               :: array(isz)
             integer            :: inums, ierr, ii

             call string_to_values(s,10,array,inums,' ;',ierr)
             call reportit()

             call string_to_values('10;2.3;3.1416',isz,array,inums,' ;',ierr)
             call reportit()

             contains
                subroutine reportit()
                   write(*,*)'string_to_values:'
                   write(*,*)'input string.............',trim(s)
                   write(*,*)'number of values found...',inums
                   write(*,*)'values...................',(array(ii),ii=1,inums)
                end subroutine reportit
            end program demo_string_to_values
