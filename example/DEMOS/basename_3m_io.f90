          program demo_basename
          use M_io, only : basename
          implicit none
          character(len=:),allocatable :: fn
          integer                      :: filename_length
          integer                      :: i
          ! get pathname from command line arguments
          do i = 1, command_argument_count()
             call get_command_argument (i, length=filename_length)
             if(allocated(fn))deallocate(fn)
             allocate(character(len=filename_length) :: fn)
             call get_command_argument (i, value=fn)
             ! leaf with any suffix removed
             ! leaf with suffix retained
             ! with suffix unless it is ".f90"
             write(*,'(*(a,1x))') basename(fn), basename(fn,''), basename(fn,'.f90')
             deallocate(fn)
          enddo
          end program demo_basename
