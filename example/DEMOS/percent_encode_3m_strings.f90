     program demo_percent_encode
     use M_strings, only : percent_encode
     use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
     implicit none
        write(*,*)percent_encode('[this is a string]')
     end program demo_percent_encode
