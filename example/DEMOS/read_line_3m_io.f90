           program demo_read_line
           use,intrinsic :: iso_fortran_env, only : stdin  => input_unit
           use,intrinsic :: iso_fortran_env, only : stderr => error_unit
           use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor
           use M_io, only : read_line
           implicit none
           character (len =: ), allocatable :: line
           integer                          :: stat
           integer                          :: icount=0
              open(unit=stdin,pad='yes')
              INFINITE: do while (read_line(line,ios=stat) == 0)
                 icount=icount
                 write (*, '(*(g0))') icount,' [',line,']'
              enddo INFINITE
              if ( .not.is_iostat_end(stat) ) then
                 write (stderr, '(*(g0))') &
                 & 'error: line ',icount,'==>',trim (line)
              endif
           end program demo_read_line
