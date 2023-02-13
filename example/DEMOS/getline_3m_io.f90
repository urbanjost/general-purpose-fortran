     program demo_getline
     use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
     use,intrinsic :: iso_fortran_env, only : iostat_end
     use M_io, only : getline
     implicit none
     integer :: iostat
     character(len=:),allocatable :: line
        open(unit=stdin,pad='yes')
        INFINITE: do while (getline(line,iostat=iostat)==0)
           write(*,'(a)')'['//line//']'
        enddo INFINITE
        if(iostat /= iostat_end)then
           write(*,*)'error reading input:',trim(line)
        endif
     end program demo_getline
