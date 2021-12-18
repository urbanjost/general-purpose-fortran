     program demo_ends_with
     use M_strings, only : ends_with
     implicit none
        write(*,*)ends_with('prog.a',['.o','.i','.s'])
        write(*,*)ends_with('prog.f90',['.F90','.f90'])
        write(*,*)ends_with('prog.pdf','.pdf')
        write(*,*)ends_with('prog.doc','.txt')
     end program demo_ends_with
