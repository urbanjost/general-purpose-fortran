          program demo_open
          integer :: ios
          character(len=256) :: message
          open (10, file='employee.names', action='read', iostat=ios,iomsg=message)
          if (ios < 0) then
             ! Perform end-of-file processing on the file connected to unit 3.
             call end_processing()
          elseif (ios > 0) then
             ! Perform error processing
             write(*,'(a)')trim(message)
             call error_processing()
             stop
          else
             write(*,*)'OPENED FILE'
          endif
          contains
             subroutine end_processing()
                write(*,*)'END OF FILE:',ios,'MESSAGE=',trim(message)
                close(unit=10,iostat=ios)
                stop
             end subroutine end_processing
             subroutine error_processing()
                write(*,*)'ERROR:',ios,'MESSAGE=',trim(message)
                close(unit=10,iostat=ios)
                stop
             end subroutine error_processing
          end program demo_open
