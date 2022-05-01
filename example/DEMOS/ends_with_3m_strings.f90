     program demo_ends_with
     use M_strings, only : ends_with
     use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
     implicit none
        write(stdout,*)ends_with('prog.a',['.o','.i','.s'])
        write(stdout,*)ends_with('prog.f90',['.F90','.f90'])
        write(stdout,*)ends_with('prog.pdf','.pdf')
        write(stdout,*)ends_with('prog.doc','.txt')
     end program demo_ends_with
