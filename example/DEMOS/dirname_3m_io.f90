          program demo_dirname
          use M_io, only : dirname
          implicit none
          character(len=:),allocatable :: filename
          integer                      :: filename_length
          integer                      :: i
          ! get pathname from command line arguments
          do i = 1 , command_argument_count()
             call get_command_argument (i , length=filename_length)
             if(allocated(filename))deallocate(filename)
             allocate(character(len=filename_length) :: filename)
             call get_command_argument (i , value=filename)
             write(*,'(a)')dirname(filename)
             deallocate(filename)
          enddo
          end program demo_dirname
