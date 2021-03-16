          program demo_getline
          use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
          use M_io, only : getline
          implicit none
          character(len=:),allocatable :: line
             open(unit=stdin,pad='yes')
             INFINITE: do while (getline(line)==0)
                write(*,'(a)')'['//line//']'
             enddo INFINITE
          end program demo_getline
