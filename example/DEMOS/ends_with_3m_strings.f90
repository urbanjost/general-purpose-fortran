     program demo_ends_with
     use M_strings, only : ends_with
     use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
     implicit none
     character(len=:),allocatable :: line, pattern
     !
        write(*,*)'basic usage'
        write(stdout,*)ends_with('prog.a','.a'), 'should be true'
        write(stdout,*)ends_with('prog.a','.o'), 'should be false'
        write(stdout,*)ends_with('prog.a',['.o','.i','.s'])
        write(stdout,*)ends_with('prog.f90',['.F90','.f90','.f  ','.F  '])
        !
        write(*,*)'ignored case'
        write(stdout,*)ends_with('prog.F90',['.f90','.f  '],ignorecase=.true.)
        !
        write(*,*)'trailing whitespace is ignored'
        write(stdout,*)ends_with('prog.pdf','.pdf')
        write(stdout,*)ends_with('prog.pdf','.pdf ')
        write(stdout,*)ends_with('prog.pdf ','.pdf ')
        write(stdout,*)ends_with('prog.pdf  ','.pdf ')
        !
        write(*,*)'equivalent using index(3f)'
        line=   'myfile.doc  '
        pattern='.doc        '
        write(stdout,*)&
        &index(trim(line),trim(pattern),back=.true.)==len_trim(line)-len_trim(pattern)+1
        write(stdout,*)ends_with(line,pattern)
     end program demo_ends_with
